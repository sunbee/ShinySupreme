library(data.table)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(C3)

# Choices for drop-downs
checks <- c("GLYPHOSATE", 
            "ATRAZINE",
            "2,4-D",
            "ACETOCHLOR",
            "DICAMBA")

navbarPage("Agricultural Chemistries", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
        
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Chemistries Explorer"),
        h4("United States Geological Survey"),

        selectInput("herbicide", "Compund", compounds),
        selectInput("check", "Comparator", checks),
        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
        #   # Only prompt for threshold when coloring or sizing by superzip
        #   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        # ),
        checkboxInput("compare", "Show/Hide", TRUE),

        #plotOutput("histCentile", height = 200),
        plotOutput("histogram", height=200),
        plotOutput("scatterplot", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  tabPanel("AI Predictions",
    sidebarLayout(
      sidebarPanel(
        # Circult court of origin
        # There are 13 circuit courts, of which the 1st through 11
        # and Washington, DC are defined by region and the federal
        # court is defined by the subject matter of the case.
         sliderTextInput("circuit", "Circuit Court", 
                         c("1st", "2nd", "3rd", "4th", "5th", "6th",
                           "7th", "8th", "9th", "10th", "11th",
                           "DC", "FED"),
                         selected = "9th",
                         grid=TRUE,
                         force_edges=TRUE),
        # Issue Area:
        selectInput("issue", "Issue Area",
                    c("Economic Activity",
                      "Judicial Power",
                      "Criminal Procedure",
                      "Due Process",
                      "Federalism And Interstate Relations",
                      "Civil Rights",
                      "First Amendment",
                      "Federal Taxation",
                      "Unions",
                      "Privacy",
                      "Attorneys"),
                    selected = "Due Process"),
        selectInput("petitioner", "Petitioner Type",
                    c("Business", 
                      "City",
                      "State",
                      "US",
                      "Employee",
                      "Employer",
                      "Goverment Official",
                      "Politician",
                      "Injured Person",
                      "American Indian",
                      "Other"),
                    selected = "Injured Person"),
        selectInput("respondent", "Petitioner Type",
                    c("Business", 
                      "City",
                      "State",
                      "US",
                      "Employee",
                      "Employer",
                      "Goverment Official",
                      "Politician",
                      "Injured Person",
                      "American Indian",
                      "Other"),
                    selected = "Business"),
         materialSwitch("unconstitutional", "Constitutionality", 
                        status="danger"),
         prettyCheckbox("ideology", "Liberal bias of lower court")
      ),   # sidebar panel 
       
       # Show a plot of the generated distribution
      mainPanel(
        # tags$style(".span12 {background-color: black;}"),
        fluidRow(
          column(width = 5, offset = 0, class = "well",
                 h4("CART"),
                 C3GaugeOutput("CART", height = 150, "auto")),
          column(width = 5, offset = 0, class = "well",
                 h4("Random Forests"),
                 align = "center", 
                 imageOutput("judge", height = 150, "auto"))
        ),   # row
        fluidRow(
          column(width = 3, offset = 0, class="well",
                 includeHTML("model_about.html")),
          column(width = 7, offset = 0, class="well",
                 includeHTML("model_about.html"))
        )
        # uiOutput("video")
      )   # main panel
    )   # sidebar layout
  ),
  
  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", herbicides[, sort(unique(State))], multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("counties", "Counties", c("All counties"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("chemistries", "Chemistries", c("All chemistries"=""), multiple=TRUE)
        )
      )
    ),
    # fluidRow(
    #   column(1,
    #     numericInput("minScore", "Min score", min=0, max=100, value=0)
    #   ),
    #   column(1,
    #     numericInput("maxScore", "Max score", min=0, max=100, value=100)
    #   )
    # ),
    hr(),
    DT::dataTableOutput("bigtable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
