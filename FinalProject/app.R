#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Kraig Sheetz
# ksheetz

# Final R Project : Pittsburgh Playgrounds and Fields

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(plotly)
library(sf)
library(shinyjs)
library(dplyr)

# Pittsburgh Playing Fields
pittsburgh.fields.load <- st_read("https://data.wprdc.org/dataset/87c77ec3-db98-4b2a-8891-d9b577b4c44d/resource/d569b513-44c0-4b65-9241-cc3d5c506760/download/fields_img.geojson")
#View(pittsburgh.fields.load)
# Pittsbugh Playground Equipment
pittsburgh.playground.load <- st_read("https://data.wprdc.org/dataset/4b9c5947-5645-4dbc-a7b6-eb323418fe02/resource/eb0cf52f-3da1-4b77-8f6d-891dff8adfa0/download/playgroundequipment_img.geojson")
pittsburgh.playground.load$ada_accessible <- ifelse(pittsburgh.playground.load$ada_accessible == 0, "NON_ADA", "ADA")
View(pittsburgh.playground.load)

pittsburgh.neighborhoods.load <- st_read("https://data.wprdc.org/dataset/e672f13d-71c4-4a66-8f38-710e75ed80a4/resource/4af8e160-57e9-4ebf-a501-76ca1b42fc99/download/pittsburghpaneighborhoods-.geojson")

#View(pittsburgh.neighborhoods.load)
#parks_in_neighborhoods <- st_join(pittsburgh.playground.load, pittsburgh.neighborhoods.load, join = st_within)
#View(parks_in_neighborhoods)

icons <- awesomeIconList(
  ADA = makeAwesomeIcon(icon = "child", library = "fa", markerColor = "yellow"),
  NON_ADA = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue")
)

# Define UI for application that draws a histogram
# Define UI for application
ui <- navbarPage("Pittsburgh Playgrounds and Fields",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # map inputs
                              #pittsburgh.fields.load
                              radioButtons(inputId = "showFields",
                                           label = "Show Fields",
                                           choices = c("With Lights" = 1, "Without Lights" = 0, "All" = 2),
                                           selected = c("With Lights" = 1)),
                              radioButtons(inputId = "showPlaygrounds",
                                           label = "Show Playgrounds",
                                           choices = c("Yes", "No"),
                                           selected = "Yes"),
                              pickerInput(inputId = "neighborhood",
                                          label = "Neighborhood",
                                          choices = unique(sort(pittsburgh.neighborhoods.load$hood)),
                                          options = list('actions-box' = TRUE),
                                          multiple = TRUE,
                                          selected = unique(sort(pittsburgh.neighborhoods.load$hood)))
                            ),
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              # Graph outputs
                              # If this doesn't work, put on other tab
                            )
                          )
                 ),
                 tabPanel("Graphs",
                          sidebarLayout(
                            sidebarPanel(
                              # Graph inputs
                            ),
                            mainPanel(

                              # Graph outputs
                            )
                          )
                 ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            #wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter fields based on user input (lights/no lights/all)
  fieldInputs <- reactive({
    fields <- pittsburgh.fields.load
    req(input$showFields)
    if (input$showFields == 2) {
      return (fields)
    } else {
      fields <- subset(fields, has_lights == input$showFields)
      return (fields)
    }
  })
  
  neighborhoods <- reactive({
    #print(input$neighborhood)
    pittsburgh.neighborhoods.load %>% filter(hood %in% input$neighborhood)
  })
  
  playgrounds <- reactive({
    # https://stackoverflow.com/questions/67724714/finding-points-with-polygon-in-sf-package
    playgrounds_in_neighborhoods <-  st_filter(pittsburgh.playground.load, neighborhoods())
    print(nrow(neighborhoods()))
    print(nrow(playgrounds_in_neighborhoods))
    return (playgrounds_in_neighborhoods)
  })

  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-79.94073, 40.448544, 13) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  
  observe({
    fields <- fieldInputs()
    leafletProxy("leaflet", data = fields) %>%
      clearGroup(group = "fields") %>%
      ## Add more info to label later, also make clickable in center
      addPolygons(popup = ~paste0("<b>", park, "</b>"), group = "fields", layerId = ~id, fill = TRUE, color = "green")
  })
  
  observe({
    hoods <- neighborhoods()
    leafletProxy("leaflet", data = hoods) %>%
      clearGroup(group = "hoods") %>%
      ## Add more info to label later, also make clickable in center
      addPolygons(popup = ~paste0("<b>", hood, "</b>"), group = "hoods", layerId = ~OBJECTID, fill = FALSE, color = "blue")
  })
  
  observe({
    play <- playgrounds()
    leafletProxy("leaflet", data = play) %>%
      clearGroup(group = "playgrounds") %>%
      addAwesomeMarkers(icon = ~icons[ada_accessible], popup = ~paste0("<b>", name, "</b>"), group = "playgrounds", layerId = ~id)
  })
  
  # Subset to data Only on screen - will need to extract lat and lon from playground dataset later 
  onScreen <- reactive({
    #req(input$leaflet_bounds)
    #bounds <- input$leaflet_bounds
    #latRng <- range(bounds$north, bounds$south)
    #lngRng <- range(bounds$east, bounds$west)
    
    #subset(greenInfInputs(), latitude >= latRng[1] & latitude <= latRng[2] & longitude >= lngRng[1] & longitude <= lngRng[2])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
