library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# Herbicide data
herbicides_top_usage <- herbicides[COMPOUND %in% compounds, ]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng=-87.6244, lat=41.8756, zoom=4)
      
  })

  herbicides_bounded <- reactive({
    if (is.null(input$map_bounds))
      return(herbicides[FALSE, ])
    
    bounds <- input$map_bounds
    lat_range <- range(bounds$north, bounds$south)
    lng_range <- range(bounds$east, bounds$west)
    
    subset(herbicides,
           Latitude >= lat_range[1] & Latitude <= lat_range[2] &
             Longitude >= lng_range[1] & Longitude <= lng_range[2] & 
             COMPOUND %in% c(input$herbicide, input$check))
  })
  
  output$histogram <- renderPlot({
    # Nothing to see here
    if (nrow(herbicides_bounded()) == 0)
      return(NULL)
    
    p <- ggplot(herbicides_bounded(), 
                aes(x=EPEST_HIGH_KG, color=COMPOUND)) +
      geom_histogram(fill="white") + 
      scale_x_log10()
    p
  })
  
  output$scatterplot <- renderPlot({
    if (nrow(herbicides_bounded()) == 0)
      return(NULL)
    
    ggplot(herbicides_bounded(), 
           aes(x=COMPOUND, y=EPEST_HIGH_KG, color=COMPOUND)) + 
      geom_violin() + scale_y_log10() + 
      geom_jitter(shape=16, position=position_jitter(0.2)) + 
      geom_violin()
  })
  
  observe({ # DEBUG map markers
    compound <- input$herbicide
    check <- input$check
    print(compound)
    print(check)
    
    herbicides_targeted = herbicides[COMPOUND %in% c(compound, check), ]
    herbicides_targeted <- dcast(herbicides_targeted, 
                                 Places+Latitude+Longitude~COMPOUND, 
                                 value.var="EPEST_HIGH_KG")
    print(head(herbicides_targeted))
    
    usage.compound <- herbicides_targeted[[compound]]
    usage.check <- herbicides_targeted[[check]]
    print(head(usage.compound))
    print(head(usage.check))
  })
  
  # Wrangle data for rendering based on the compound and check 
  # selected by the user.
  pivoted_data <- reactive({
    compound <- input$herbicide
    check <- input$check
    
    herbicides_targeted = herbicides[COMPOUND %in% c(compound, check), ]
    herbicides_targeted <- dcast(herbicides_targeted, 
                                 Places+Latitude+Longitude~COMPOUND, 
                                 value.var="EPEST_HIGH_KG")
  })

  # Render the usage patterns for the selected compound and check.
  # This observed responds to the user's choice of compound and check.
  observe({
    compound <- input$herbicide
    check <- input$check

    herbicides_targeted <- pivoted_data()
    
    usage.compound <- herbicides_targeted[[compound]]
    usage.check <- herbicides_targeted[[check]]

    pal.compound <- colorQuantile(palette="Yellow", domain=usage.compound, n=5)
    pal.check <- colorQuantile(palette="viridis", domain=usage.check, n=5)

    radius.compound <- sqrt(usage.compound / max(usage.compound, na.rm=TRUE)) * 70000
    radius.check <- sqrt(usage.check / max(usage.check, na.rm=TRUE)) * 70000
      
    leafletProxy("map", data=herbicides_targeted) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude,
                 radius = radius.compound,
                 fillColor = pal.compound(usage.compound),
                 stroke = FALSE,
                 fillOpacity = 0.3
      ) %>%
      addCircles(~Longitude, ~Latitude,
                 radius=radius.check,
                 fillColor = pal.check(usage.check),
                 stroke = FALSE,
                 fillOpacity = 0.7,
                 layerId = ~Places) %>%
      addLegend("bottomleft", pal=pal.check, values=usage.check, title=check,
                layerId = "colorLegend")

  })

  observe({ # DEBUG popup, etc.
  # Acquire context to handle a mouse-click over the map area.
    event <- input$map_shape_click
    if (is.null(event) | is.null(event$id)) 
      return()
    else {
      # Unpack the event object
      print(event$id)
      print(event$lat)
      print(event$lng)
      
      # Apply context for action
      # Obtain pivot table for selected compound and check, and
      # select the record for the location clicked
      herbicides_targeted <- pivoted_data()
      selection = herbicides_targeted[Places == event$id, ]
      print(selection)
      print(as.character(selection[["Places"]]))
      print(selection[[input$herbicide]])
    
    }
  })
  
  # Show detailed information about herbicide usage at location
  # indicated by map-click.
  showLocationSummary <- function(location, lat, lng) {
    herbicides_targeted <- pivoted_data()
    selection <- herbicides_targeted[Places == location, ]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selection[[input$herbicide]])),
      tags$strong(HTML(sprintf("%s", selection$Places))),
      tags$br(),
      sprintf("Maximum est. %s usage: %d", input$herbicide, as.integer(selection[[input$herbicide]])),
      tags$br(),
      sprintf("Maximum est. %s usage: %d", input$check, as.integer(selection[[input$check]])), 
      tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = location)
  }
  

  # When map is clicked, show a popup with city info
  observe({
    herbicides_targeted <- pivoted_data()
    
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event) | is.null(event$id))
      return()

    isolate({
      showLocationSummary(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
