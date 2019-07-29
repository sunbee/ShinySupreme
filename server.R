library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Future option to compare all other chemistries to glyphosate.
# Random sampling will address potential latency issues with
# rending in leaflet.
herbicides_top_usage <- herbicides[COMPOUND %in% compounds, ]

function(input, output, session) {

  ## Interactive Map ###########################################

  output$map <- renderLeaflet({
  # Create the map
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng=-87.6244, lat=41.8756, zoom=4)
      
  })

  herbicides_bounded <- reactive({
  # Show data within bounds of the interactive map.
  # input$map_bounds: use to achieve JS equivalent of:
  #   map.getBounds().getEast()
  #   map.getBounds().getWest()
  #   map.getBounds().getNorth()
  #   map.getBounds().getSouth()
  # input$herbicide: selected herbicide
  # input$check: selected comparator
  # herbicides: global table of all compounds and their attributes
  #   Subset global herbicide data for the bounded region
  #   Subset for selected compound and check
  # output$histogram: plot the usage patterns for selected compounds
  # output$scattedplot: plot the usage patterns for selected compounds
    
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
      scale_x_log10() +
      theme(legend.position = "top")
    p
  })
  
  output$scatterplot <- renderPlot({
    if (nrow(herbicides_bounded()) == 0)
      return(NULL)
    
    ggplot(herbicides_bounded(), 
           aes(x=COMPOUND, y=EPEST_HIGH_KG, color=COMPOUND)) + 
      geom_violin() + scale_y_log10() + 
      geom_jitter(shape=16, position=position_jitter(0.2)) + 
      geom_violin() +
      theme(legend.position = "top")
  })
  
  observe({ 
  # DEBUG map markers
    compound <- input$herbicide
    check <- input$check
    print(compound)
    print(check)
    
    herbicides_targeted = herbicides[COMPOUND %in% c(compound, check), ]
    herbicides_targeted <- dcast(herbicides_targeted, 
                                 Location+Latitude+Longitude~COMPOUND, 
                                 value.var="EPEST_HIGH_KG")
    herbicides_targeted$Score <- ceiling(ecdf(herbicides_targeted[[check]])(herbicides_targeted[[check]]) * 100)
    print(head(herbicides_targeted))
    
    usage.compound <- herbicides_targeted[[compound]]
    usage.check <- herbicides_targeted[[check]]
    print(head(usage.compound))
    print(head(usage.check))
  })
  
  pivoted_data <- reactive({
  # Wrangle data for rendering based on the user's selection
  # of compounds to view and compare. Pivot the table which is
  # in long form to wide form so locations are in rows and the
  # selected compounds in columns, data being max. est. usage.
  #
  # input$herbicide: selected compound
  # input$check: selected comparator
  # herbicides: global table with usage of all compounds and all locations
  #   in long form with USGS data for 2012.
  # herbicides_targeted: usage data for selected compounds across locations
  #   in wide form, showing max usage (estimate). Add a score for selected
  #   comparator (default: glyphosate) based on percentile rank.
  
    compound <- input$herbicide
    check <- input$check
    
    herbicides_targeted = herbicides[COMPOUND %in% c(compound, check), ]
    herbicides_targeted <- dcast(herbicides_targeted, 
                                 Location+Latitude+Longitude~COMPOUND, 
                                 value.var="EPEST_HIGH_KG")
    herbicides_targeted$Score <- ceiling(ecdf(herbicides_targeted[[check]])(herbicides_targeted[[check]]) * 100)
    herbicides_targeted
  })

  observe({
  # Render the geospatial usage patterns for the selected compound and check.
  # input$herbicide: selected compound
  # input$check: selected comparator
  # herbicides_targeted: react chain yielding data in shape for mapping
  # proxy_map: geo-spatial rendering of data about the selected compounds
    compound <- input$herbicide
    check <- input$check

    herbicides_targeted <- pivoted_data()
    
    usage.compound <- herbicides_targeted[[compound]]
    usage.check <- herbicides_targeted[[check]]

    pal.compound <- colorQuantile(palette="blue", domain=usage.compound, n=5)
    pal.check <- colorQuantile(palette="viridis", domain=usage.check, n=5)

    radius.compound <- sqrt(usage.compound / max(usage.compound, na.rm=TRUE)) * 70000
    radius.check <- sqrt(usage.check / max(usage.check, na.rm=TRUE)) * 70000
      
    proxy_map <- leafletProxy("map", data=herbicides_targeted) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude,
                 radius = radius.compound,
                 fillColor = pal.compound(usage.compound),
                 stroke = FALSE,
                 fillOpacity = 0.6,
                 layerId = ~ paste(Location, "_"),
                 group = "Compound"
      ) %>%
      addCircles(~Longitude, ~Latitude,
                 radius=radius.check,
                 fillColor = pal.check(usage.check),
                 stroke = FALSE,
                 fillOpacity = 0.6,
                 layerId = ~Location,
                 group = "Check") %>% 
      addLegend("bottomleft", pal=pal.check, values=usage.check, title=check,
                layerId = "colorLegend",
                group = "Check") 
    proxy_map
  
  })

  observe({
  # Show or hide the comparator which is a semi-transparent overlay.
    if (input$compare == FALSE) {
      leafletProxy("map") %>% hideGroup("Check")
      # Legend shows although grouped together.
    } else {
      leafletProxy("map") %>% showGroup("Check") 
    }
    
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
      selection = herbicides_targeted[Location == event$id, ]
      print(selection)
      print(as.character(selection[["Location"]]))
      print(selection[[input$herbicide]])
    
    }
  })
  
  showLocationSummary <- function(location, lat, lng) {
  # Show detailed information about herbicide usage at clicked location.
  # location: obtained from map-click event bearing location as layer Id
  # lat, lng: latitude and longitude obtained from map-click event object
  # herbicides_targeted: reach chain yield with data upon selected compounds  
  #   across all locations, in wide form for plotting. Has the score as
  #   rank-based percentile for the selected comparator. 
  
    herbicides_targeted <- pivoted_data()
    selection <- herbicides_targeted[Location == location, ]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selection[["Score"]])),
      tags$strong(HTML(sprintf("%s", selection$Location))),
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
    # Strip the lagging underscore from layer Id for the compound.
      showLocationSummary(sub(" _", "", event$id), event$lat, event$lng)
    })
  })


  ## Supreme Predictor #######################################
  
  x <- reactive({
    # Condition inputs for feeding into predictive models
    this = list()
    this$Circuit = input$circuit
    this$Issue = paste0(strsplit(input$issue, " ")[[1]], collapse = '')
    this$Petitioner = paste0(strsplit(toupper(input$petitioner), " ")[[1]], collapse='.')
    this$Respondent = paste0(strsplit(toupper(input$respondent), " ")[[1]], collapse='.')
    this$LowerCourt = ifelse(input$ideology, "liberal", "conser")
    this$Unconst = ifelse(input$unconstitutional, 1, 0)
    print(paste(unlist(this), sep="", collapse="|"))
    this
  })
  
  xdf <- reactive({ # Chain reaction
    # Assemple predictors into a data frame with the newdata template
    that <- x()
    predictor <- newdata
    predictor$Circuit     <- that$Circuit
    predictor$Issue       <- that$Issue
    predictor$Petitioner  <- that$Petitioner
    predictor$Respondent  <- that$Respondent
    predictor$LowerCourt  <- that$LowerCourt
    predictor$Unconst     <- that$Unconst
    print(paste(unlist(predictor), sep="", collapse="|"))
    predictor
  })
  
  output$judge <- renderImage({
    # Show the judge's reaction based on the prediction from random forests. The judge 
    # is mad (i.e. over-turns the lower court's ruling) or glad.
    that <- x()
    if (that$Unconst) {
      return(list(
        src = "images/judge_mad.jpg", 
        filetype = "image/jpeg", 
        alt = "Judge mad",
        height = 150))
    } else {
      return(list(
        src = "images/judge_glad.jpg", 
        filetype = "image/jpeg", 
        alt = "Judge glad",
        height = 150))
    }
  }, deleteFile = FALSE)
  
  output$CART <- renderC3Gauge({
    # Show the judge's reaction as probability of over-turning a lower court's ruling.
    out <- predict(StevensTree, newdata=xdf(), type='prob')
    prob = floor(out[1, 2] * 100)
    print(prob)
    C3Gauge(prob)
  })
  
  ## Data Explorer ###########################################

  observe({
    counties <- if (is.null(input$states)) character(0) else {  
      herbicides[State %in% input$states, sort(unique(County))]
    }
    stillSelected <- isolate(input$counties[input$counties %in% counties])
    updateSelectInput(session, "counties", choices = counties,
                      selected = stillSelected)
  })

  observe({
    chemistries <- if (is.null(input$states)) character(0) else {
      herbicides[State %in% input$states, sort(unique(COMPOUND))]
    }
    stillSelected <- isolate(input$chemistries[input$chemistries %in% chemistries])
    updateSelectInput(session, "chemistries", choices = chemistries,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      loc <- input$goto$loc
      lat <- input$goto$lat
      lng <- input$goto$lng
      print(input$goto)
      #showLocationSummary(loc, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- herbicides %>%
      filter(
        # Score >= input$minScore,
        # Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$counties) | County %in% input$counties,
        is.null(input$chemistries) | COMPOUND %in% input$chemistries
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-loc="', Location, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
