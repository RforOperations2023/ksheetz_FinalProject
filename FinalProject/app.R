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

# Final R Project : Pittsburgh Recreational Activity and Non-Constrained Entertainment (PRANCE)

library(shiny)
library(shinythemes)
library(leaflet)
library(httr)
library(leaflet.extras)
library(shinyWidgets)
library(plotly)
library(sf)
library(shinyjs)
library(jsonlite)
library(dplyr)
library(stringr)
library(DT)

# Import data sources

# Pittsburgh Playing Fields
pittsburgh.fields.load <- st_read("https://data.wprdc.org/dataset/87c77ec3-db98-4b2a-8891-d9b577b4c44d/resource/d569b513-44c0-4b65-9241-cc3d5c506760/download/fields_img.geojson")
# Get field Centroids, join back in to dataframe
pittsburgh.fields.cent <- pittsburgh.fields.load %>%
  st_centroid() %>%
  mutate(longitude = sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)
pittsburgh.fields.load <- pittsburgh.fields.load %>%
  left_join(pittsburgh.fields.cent)
pittsburgh.fields.load$has_lights <- ifelse(pittsburgh.fields.load$has_lights == 0, "Unlit", "Lit")


# Pittsbugh Playground Equipment
pittsburgh.playground.load <- st_read("https://data.wprdc.org/dataset/4b9c5947-5645-4dbc-a7b6-eb323418fe02/resource/eb0cf52f-3da1-4b77-8f6d-891dff8adfa0/download/playgroundequipment_img.geojson")
pittsburgh.playground.load$ada_accessible <- ifelse(pittsburgh.playground.load$ada_accessible == 0, "NON_ADA", "ADA")
#View(pittsburgh.playground.load)
pittsburgh.activities.load <- st_read("https://data.wprdc.org/dataset/8da92664-22a4-42b8-adae-1950048d70aa/resource/96d327a8-fb12-4174-a30d-7ec9a9920237/download/courts_img.geojson")
pittsburgh.activities.load$type <- lapply(pittsburgh.activities.load$type, function (x) word(x, 1))
# Get field Centroids, join back in to dataframe
pittsburgh.activities.cent <- pittsburgh.activities.load %>%
  st_centroid() %>%
  mutate(longitude = sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)
pittsburgh.activities.load <- pittsburgh.activities.load %>%
  left_join(pittsburgh.activities.cent)

# Pittsburgh Neighborhoods GeoJSON Data (Polygons)
pittsburgh.neighborhoods.load <- st_read("https://data.wprdc.org/dataset/e672f13d-71c4-4a66-8f38-710e75ed80a4/resource/4af8e160-57e9-4ebf-a501-76ca1b42fc99/download/pittsburghpaneighborhoods-.geojson")


# Icons to be used later for location markers
icons <- awesomeIconList(
  ADA = makeAwesomeIcon(icon = "wheelchair", library = "fa", markerColor = "blue"),
  NON_ADA = makeAwesomeIcon(icon = "child", library = "fa", markerColor = "green"),
  Lit = makeAwesomeIcon(icon = "baseball-bat-ball", library = "fa", markerColor = "lightgreen"),
  Unlit = makeAwesomeIcon(icon = "baseball-bat-ball", library = "fa", markerColor = "darkgreen"),
  Volleyball = makeAwesomeIcon(icon = "volleyball", library = "fa", markerColor = "orange"),
  Basketball = makeAwesomeIcon(icon = "basketball", library = "fa", markerColor = "orange"),
  Pickleball = makeAwesomeIcon(icon = "table-tennis-paddle-ball", library = "fa", markerColor = "orange"),
  Tennis = makeAwesomeIcon(icon = "baseball", library = "fa", markerColor = "orange"),
  Hockey = makeAwesomeIcon(icon = "hockey-puck", library = "fa", markerColor = "orange"),
  Dek = makeAwesomeIcon(icon = "broom-ball", library = "fa", markerColor = "orange"),
  General = makeAwesomeIcon(icon = "broom-ball", library = "fa", markerColor = "orange"),
  Street = makeAwesomeIcon(icon = "broom-ball", library = "fa", markerColor = "orange"),
  Bocce = makeAwesomeIcon(icon = "broom-ball", library = "fa", markerColor = "orange"),
  Horseshoe = makeAwesomeIcon(icon = "broom-ball", library = "fa", markerColor = "orange"),
  Lawn = makeAwesomeIcon(icon = "bowling-ball", library = "fa", markerColor = "orange")
)

# Define UI for application
ui <- navbarPage("Pittsburgh Recreational Activities and Non-Constrained Entertainment (PRANCE)",
                 theme = shinytheme("spacelab"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "haslights",
                                           label = "Show Fields",
                                           choices = c("With Lights" = 1, "Without Lights" = 0, "Both" = 2),
                                           selected = c("Both" = 2)),
                              radioButtons(inputId = "ada",
                                           label = "Show Playgrounds",
                                           choices = c("ADA Accessible" = 1, "Non-ADA Accessible" = 0, "Both" = 2),
                                           selected = c("Both" = 2)),
                              # Input for Neighborhood Selection
                              pickerInput(inputId = "neighborhood",
                                          label = "Select Neighborhood(s)",
                                          choices = unique(sort(pittsburgh.neighborhoods.load$hood)),
                                          options = list('actions-box' = TRUE),
                                          multiple = TRUE,
                                          selected = c("Point Breeze", "Shadyside", "South Oakland", "Squirrel Hill North", 
                                                       "Squirrel Hill South")
                                          ),
                              # Input for Activity Selection
                              pickerInput(inputId = "activity",
                                          label = "Select Activity(s)",
                                          choices = unique(pittsburgh.activities.load$type),
                                          options = list('actions-box' = TRUE),
                                          multiple = TRUE,
                                          selected = unique(pittsburgh.activities.load$type)),
                              actionButton("update_crime", "Load/Update Current Crime Data (Last 30 Days)", icon = icon("play"))
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
                 tabPanel("Graphs",
                          fluidPage(
                            # Graph Outputs
                            plotlyOutput("rec_per_neighborhood"),
                            br(),
                            br(),
                            plotlyOutput("crime_per_neighborhood")
                          )
                 ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("results"))
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get crime data via API Call to WPRDC - only run when the user presses Load/Update Crime Data button
  crimeLoad <- eventReactive(input$neighborhood, {
    # URL Encode the query
    q <- "SELECT * from \"1797ead8-8262-41cc-9099-cbc8a161924b\""
    formatQuery <- URLencode(q, repeated = TRUE)
    # Build URL for GET request
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=", formatQuery)
    # Run Get Request
    g <- GET(url)
    # Check if there's an error
    if (g$status_code != 200) {
      # Send error to table
      crime <- as_tibble(content(g)$error$info)
    } else {
      # Retrieve and display the results if successful
      crime <- fromJSON(content(g, "text"))$result$records
    }
    # Filter out values without X and Y coordinates
    crime <- crime %>% filter(!is.na(X))
    crime.SP <- st_as_sf(crime, coords = c("X", "Y"), crs = 4326)
    return(crime.SP)
  })
  
  # Filter displayed crime based on user input for neighborhoods
  crimeData <- reactive({
    crime <- st_filter(crimeLoad(), neighborhoods())
    return(crime)
  })
  
  # Filter fields based on user input for neighborhoods
  fieldInputs <- reactive({
    fields_in_neighborhoods <- st_filter(pittsburgh.fields.load, neighborhoods())
    if (input$haslights == 2) {
      return(fields_in_neighborhoods)
    } else if (input$haslights == 1) {
      fields_userfiltered <- fields_in_neighborhoods %>% filter(has_lights == "Lit")
      return (fields_userfiltered)
    } else {
      fields_userfiltered <- fields_in_neighborhoods %>% filter(has_lights == "Unlit")
      return (fields_userfiltered)
    }
  })
  # Filter neighborhoods based on user input
  neighborhoods <- reactive({
    neighborhoods <- pittsburgh.neighborhoods.load
    if (length(input$neighborhood) > 0) {
      neighborhoods <- neighborhoods %>% filter(hood %in% input$neighborhood)
    } else {
      # Default neighborhood == Shadyside
      neighborhoods <- neighborhoods %>% filter(hood == "Shadyside")
    }
    return (neighborhoods)
  })
  
  # Filter playgrounds based on user input for neighborhoods
  playgrounds <- reactive({
    # https://stackoverflow.com/questions/67724714/finding-points-with-polygon-in-sf-package
    playgrounds_in_neighborhoods <-  st_filter(pittsburgh.playground.load, neighborhoods())
    if (input$ada == 2) {
      return(playgrounds_in_neighborhoods)
    } else if (input$ada == 1) {
      playgrounds_userfiltered <- playgrounds_in_neighborhoods %>% filter(ada_accessible == "ADA")
      return (playgrounds_userfiltered)
    } else {
      playgrounds_userfiltered <- playgrounds_in_neighborhoods %>% filter(ada_accessible == "NON_ADA")
      return (playgrounds_userfiltered)
    }
  })
  
  # Filter activities based on user input for neighborhoods and also user input for activities
  activities <- reactive({
    activities_in_neighborhoods <- st_filter(pittsburgh.activities.load, neighborhoods())
    if (length(input$activity) > 0) {
      activities_filtered <- activities_in_neighborhoods %>% filter(type %in% input$activity)
    } else {
      # Default sport = Basketball
      activities_filtered <- activities_in_neighborhoods %>% filter(type == "Basketball")
    }
    activities_filtered$type <- unlist(activities_filtered$type)
    return (activities_filtered)
  })
  
  # Creates dataframe with all recreational activities in the neighborhoods selected by the user
  allRec <- reactive({
    fields <- st_join(fieldInputs(), neighborhoods(), join = st_contains, left = TRUE, largest = TRUE)
    playgrounds <- st_join(playgrounds(), neighborhoods(), join = st_contains, left = TRUE, largest = TRUE)
    activities <- st_join(activities(), neighborhoods(), join = st_contains, left = TRUE, largest = TRUE)

    field_df <- data.frame(Name = fields$park, Type = "Field", Sport = NA, ADA_Accessible = NA,
                           GoalPosts = fields$goal_post, Lights = fields$has_lights, 
                           Neighborhood = fields$hood, Latitude = fields$intptlat10, Longitude = fields$intptlon10)
    playground_df <- data.frame(Name = playgrounds$name, Type = "Playground", Sport = NA, ADA_Accessible = playgrounds$ada_accessible,
                           GoalPosts = NA, Lights = NA, Neighborhood = playgrounds$hood, 
                           Latitude = playgrounds$intptlat10, Longitude = playgrounds$intptlon10)
    activity_df <- data.frame(Name = activities$name, Type = "Activity", Sport = activities$type, ADA_Accessible = NA,
                                GoalPosts = NA, Lights = NA, Neighborhood = activities$hood, 
                                Latitude = activities$intptlat10, Longitude = activities$intptlon10)
    
    result = bind_rows(field_df, playground_df, activity_df)
    return(result[order(result$Name),])
  })
  
  # Restricts crime to only those occuring in neighborhoods selected
  neighborhood_crime <- reactive({
    hood_crime <- st_intersection(neighborhoods(), crimeData())
    return (hood_crime)
  })
  
  # Plot for crime per neighborhood selected
  output$crime_per_neighborhood <- renderPlotly({
    ggplot(data = neighborhood_crime(), aes(x = hood, fill = CLEAREDFLAG)) +
      geom_bar() + 
      ggtitle("Crime by Neighborhood") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Neighborhood") +
      ylab("# Incidents") +
      labs(fill = "Incident Cleared")
  })
  
  # Plot for Recreation per neighborhood selected
  output$rec_per_neighborhood <- renderPlotly({
    ggplot(data = allRec(), aes(x = Neighborhood, fill = Type)) +
      geom_bar() + 
      ggtitle("Recreation by Neighborhood") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Neighborhood") +
      ylab("# Recreational Facilities") 
  })
  
  # renderleaflet for map with additional layer controls for our custom overlay groups - crime hidden by default
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(-79.94073, 40.448544, 13) %>%
      addLayersControl(baseGroups = c("Google", "Satellite"),
                       overlayGroups = c("Crime", "Fields", "Playgrounds", "Activities"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Crime")
  })
  
  # Update activities via leaflet proxy
  observe({
    active <- activities()
    print(~icons["Basketball"])
    leafletProxy("leaflet", data = active) %>%
      clearGroup(group = "Activities") %>%
      addPolygons(popup = ~paste0("<b>", name, "</b>"), group = "Activities", layerId = ~id, fill = TRUE, color = "orange") %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = ~icons[type], 
                        popup = ~paste0("<b>", name, "</b>", "<br>", "Surface Material:", surface_material), 
                        group = "Activities", layerId = ~id)
  })
  
  # Update fields via leaflet proxy
  observe({
    fields <- fieldInputs()
    leafletProxy("leaflet", data = fields) %>%
      clearGroup(group = "Fields") %>%
      ## Add more info to label later, also make clickable in center
      addPolygons(popup = ~paste0("<b>", park, "</b>"), group = "Fields", layerId = ~id, fill = TRUE, color = "green") %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = ~icons[has_lights], 
                        popup = ~paste0("<b>", park, "</b>", "<br>", "Fields:", field_usage, "<br>", "Lit:", has_lights), 
                        group = "Fields", layerId = ~id)
  })
  
  # Update neighborhoods via leaflet proxy
  observe({
    hoods <- neighborhoods()
    leafletProxy("leaflet", data = hoods) %>%
      clearGroup(group = "hoods") %>%
      ## Add more info to label later, also make clickable in center
      addPolygons(popup = ~paste0("<b>", hood, "</b>"), group = "hoods", layerId = ~OBJECTID, fill = FALSE, color = "blue")
  })
  
  # Update playgrounds via leaflet proxy
  observe({
    play <- playgrounds()
    leafletProxy("leaflet", data = play) %>%
      clearGroup(group = "Playgrounds") %>%
      addAwesomeMarkers(icon = ~icons[ada_accessible], 
                        popup = ~paste0("<b>", name, "</b>", "<br>", "Equipment:", equipment_type, "<br>", "ADA Accessible:", ada_accessible), 
                        group = "Playgrounds", layerId = ~id)
  })
  
  # Update crime via leaflet proxy
  observe({
    crime <- crimeData()
    leafletProxy("leaflet", data = crime) %>%
      clearGroup(group = "Crime") %>%
      addHeatmap(group ="Crime")
  })
  
  # Display results in Table
  output$results <- DT::renderDataTable(
    allRec(),
    extensions = 'Buttons',
    options = list(scrollX = TRUE,
                   dom = 'Bplfrti',
                   buttons = c('copy', 'csv', 'excel')))

}

# Run the application 
shinyApp(ui = ui, server = server)
