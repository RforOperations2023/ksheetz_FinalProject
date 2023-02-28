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
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)

# Pittsburgh Playing Fields
pittsburgh.fields.load <- st_read("https://data.wprdc.org/dataset/87c77ec3-db98-4b2a-8891-d9b577b4c44d/resource/d569b513-44c0-4b65-9241-cc3d5c506760/download/fields_img.geojson")
#View(pittsburgh.fields.load)
# Pittsbugh Playground Equipment
pittsburgh.playground.load <- st_read("https://data.wprdc.org/dataset/4b9c5947-5645-4dbc-a7b6-eb323418fe02/resource/eb0cf52f-3da1-4b77-8f6d-891dff8adfa0/download/playgroundequipment_img.geojson")
#View(pittsburgh.playground.load)
pittsburgh.neighborhoods.load <- st_read("https://data.wprdc.org/dataset/e672f13d-71c4-4a66-8f38-710e75ed80a4/resource/4af8e160-57e9-4ebf-a501-76ca1b42fc99/download/pittsburghpaneighborhoods-.geojson")
View(pittsburgh.neighborhoods.load)

# Define UI for application that draws a histogram
# Define UI for application
ui <- navbarPage("Pittsburgh Green Spaces",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # map inputs
                              #pittsburgh.fields.load
                              radioButtons(inputId = "showFields",
                                           label = "Show Fields",
                                           choices = c("Yes" = 1, "No" = 0),
                                           selected = "Yes"),
                              radioButtons(inputId = "showPlaygrounds",
                                           label = "Show Playgrounds",
                                           choices = c("Yes", "No"),
                                           selected = "Yes")
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
  
  # Filter fields based on user input
  fieldInputs <- reactive({
    fields <- pittsburgh.fields.load
    req(input$showFields)
    if (input$showFields == 1) {
      return(fields)
    }
  })

  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-79.98093, 40.444217, 12) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  
  observe({
    fields <- fieldInputs()
    leafletProxy("leaflet", data = fields) %>%
      # In this case either lines 92 or 93 will work
      clearShapes() %>%
      addPolygons(popup = ~paste0("<b>", park, "</b>"),  layerId = ~id, fill = FALSE, color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
