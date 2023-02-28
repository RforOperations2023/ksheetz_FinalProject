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

# Final R Project
# Test 2

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


# Define UI for application that draws a histogram
# Define UI for application
ui <- navbarPage("NYC Green Infrastructure",
                 theme = shinytheme("united"),
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
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Map inputs
                            ),
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet")
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

  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-79.98093, 40.444217, 12) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
