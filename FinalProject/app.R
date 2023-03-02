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

# Final R Project : Pittsburgh Recreational Activity and Non-Constrained Entertainment

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
#View(pittsburgh.fields.load)

# Pittsbugh Playground Equipment
pittsburgh.playground.load <- st_read("https://data.wprdc.org/dataset/4b9c5947-5645-4dbc-a7b6-eb323418fe02/resource/eb0cf52f-3da1-4b77-8f6d-891dff8adfa0/download/playgroundequipment_img.geojson")
pittsburgh.playground.load$ada_accessible <- ifelse(pittsburgh.playground.load$ada_accessible == 0, "NON_ADA", "ADA")
#View(pittsburgh.playground.load)
pittsburgh.activities.load <- st_read("https://data.wprdc.org/dataset/8da92664-22a4-42b8-adae-1950048d70aa/resource/96d327a8-fb12-4174-a30d-7ec9a9920237/download/courts_img.geojson")
pittsburgh.activities.load$type <- lapply(pittsburgh.activities.load$type, function (x) word(x, 1))
pittsburgh.activities.cent <- pittsburgh.activities.load %>%
  st_centroid() %>%
  mutate(longitude = sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)
pittsburgh.activities.load <- pittsburgh.activities.load %>%
  left_join(pittsburgh.activities.cent)
#View(pittsburgh.activities.load)

pittsburgh.neighborhoods.load <- st_read("https://data.wprdc.org/dataset/e672f13d-71c4-4a66-8f38-710e75ed80a4/resource/4af8e160-57e9-4ebf-a501-76ca1b42fc99/download/pittsburghpaneighborhoods-.geojson")

#View(pittsburgh.neighborhoods.load)
#parks_in_neighborhoods <- st_join(pittsburgh.playground.load, pittsburgh.neighborhoods.load, join = st_within)
#View(parks_in_neighborhoods)

icons <- awesomeIconList(
  ADA = makeAwesomeIcon(icon = "child", library = "fa", markerColor = "green"),
  NON_ADA = makeAwesomeIcon(icon = "wheelchair", library = "fa", markerColor = "blue"),
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

# Define UI for application that draws a histogram
# Define UI for application
ui <- navbarPage("Pittsburgh Recreational Activities and Non-Constrained Entertainment (PRANCE)",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # map inputs
                              #pittsburgh.fields.load
                              radioButtons(inputId = "showFields",
                                           label = "Show Fields",
                                           choices = c("With Lights" = 1, "Without Lights" = 0, "Both" = 2),
                                           selected = c("With Lights" = 1)),
                              pickerInput(inputId = "neighborhood",
                                          label = "Neighborhood",
                                          choices = unique(sort(pittsburgh.neighborhoods.load$hood)),
                                          options = list('actions-box' = TRUE),
                                          multiple = TRUE,
                                          # selected = c("Bloomfield", "Central Business District", "Central Lawrenceville", 
                                          #              "Central Oakland", "Crawford-Roberts", "East Hills", "East Liberty",
                                          #              "Friendship", "Garfield", "Glen Hazel", "Greenfield", "Hazelwood",
                                          #              "Highland Park", "Homewood North", "Homewood South", "Homewood West",
                                          #              "Larimer", "Lincoln-Lemington-Belmar", "Lower Lawrenceville", 
                                          #              "Middle Hill", "Morningside", "North Oakland", "Point Breeze", 
                                          #              "Point Breeze North", "Polish Hill", "Regent Square", "Shadyside",
                                          #              "South Oakland", "Squirrel Hill North", "Squirrel Hill South", 
                                          #              "Stanton Heights", "Strip District", "Swisshelm Park", "Upper Hill",
                                          #              "Upper Lawrenceville", "West Oakland")
                                          selected = c("South Oakland", "Squirrel Hill North", "Squirrel Hill South")
                                          ),
                              pickerInput(inputId = "activity",
                                          label = "Activity",
                                          choices = unique(pittsburgh.activities.load$type),
                                          options = list('actions-box' = TRUE),
                                          multiple = TRUE,
                                          selected = unique(pittsburgh.activities.load$type)),
                              actionButton("run", "Show Crime", icon = icon("play"))
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
                          fluidPage(
                            # Graph Outputs
                            plotlyOutput("rec_per_neighborhood"),
                            plotlyOutput("crime_per_neighborhood")
                          )
                 ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            #Change this later away from crime data
                            wellPanel(DT::dataTableOutput("results"))
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Get crime data via API Call to WPRDC
  crimeData <- eventReactive(input$neighborhood, {
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
    #View(crime.SP)
    crime.SP <- st_filter(crime.SP, neighborhoods())
    return(crime.SP)
  })
  
  # Filter fields based on user input (lights/no lights/all) - need to add this back in
  fieldInputs <- reactive({
    fields_in_neighborhoods <- st_filter(pittsburgh.fields.load, neighborhoods())
    return(fields_in_neighborhoods)
  })
  
  neighborhoods <- reactive({
    #print(input$neighborhood)
    neighborhoods <- pittsburgh.neighborhoods.load
    if (length(input$neighborhood) > 0) {
      neighborhoods <- neighborhoods %>% filter(hood %in% input$neighborhood)
    } else {
      # Default neighborhood == Shadyside
      neighborhoods <- neighborhoods %>% filter(hood == "Shadyside")
    }
    return (neighborhoods)
  })
  
  playgrounds <- reactive({
    # https://stackoverflow.com/questions/67724714/finding-points-with-polygon-in-sf-package
    playgrounds_in_neighborhoods <-  st_filter(pittsburgh.playground.load, neighborhoods())
    return (playgrounds_in_neighborhoods)
  })
  
  activities <- reactive({
    activities_in_neighborhoods <- st_filter(pittsburgh.activities.load, neighborhoods())
    if (length(input$activity) > 0) {
      activities_filtered <- activities_in_neighborhoods %>% filter(type %in% input$activity)
    } else {
      # Default sport = Dek Hockey
      activities_filtered <- activities_in_neighborhoods %>% filter(type == "Basketball")
    }
    
    activities_filtered$type <- unlist(activities_filtered$type)
    return (activities_filtered)
  })
  
  allRec <- reactive({
    fields <- st_join(fieldInputs(), neighborhoods(), join = st_contains, left = TRUE, largest = TRUE)
    playgrounds <- st_join(playgrounds(), neighborhoods(), join = st_contains, left = TRUE, largest = TRUE)
    #View(playgrounds)
    activities <- st_join(activities(), neighborhoods(), join = st_contains, left = TRUE, largest = TRUE)
    #View(activities)

    
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
    return(result)
  })
  
  neighborhood_crime <- reactive({
    hood_crime <- st_intersection(neighborhoods(), crimeData()) #%>%
      #group_by(hood) %>%
      #count()
    #View(hood_crime)
    return (hood_crime)
  })
  
  output$crime_per_neighborhood <- renderPlotly({
    ggplot(data = neighborhood_crime(), aes(x = hood, fill = CLEAREDFLAG)) +
      geom_bar() + 
      ggtitle("Crime by Neighborhood") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Neighborhood") +
      ylab("Crime")
  })
  
  output$rec_per_neighborhood <- renderPlotly({
    ggplot(data = allRec(), aes(x = Neighborhood, fill = Type)) +
      geom_bar() + 
      ggtitle("Recreation by Neighborhood") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Neighborhood") +
      ylab("# Recreational Facilities")
  })
  

  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      #addProviderTiles("Jawg.Matrix", group = "Matrix") %>%
      setView(-79.94073, 40.448544, 13) %>%
      addLayersControl(baseGroups = c("Google", "Satellite"),
                       overlayGroups = c("Crime", "Fields", "Playgrounds", "Activities"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Crime")
  })
  
  observe({
    active <- activities()
    leafletProxy("leaflet", data = active) %>%
      clearGroup(group = "Activities") %>%
      ## Add more info to label later, also make clickable in center
      addPolygons(popup = ~paste0("<b>", name, "</b>"), group = "Activities", layerId = ~id, fill = TRUE, color = "orange") %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = ~icons[type], popup = ~paste0("<b>", name, "</b>", "<br>", "Surface Material:", surface_material), 
                        group = "Activities", layerId = ~id)
  })
  
  observe({
    fields <- fieldInputs()
    leafletProxy("leaflet", data = fields) %>%
      clearGroup(group = "Fields") %>%
      ## Add more info to label later, also make clickable in center
      addPolygons(popup = ~paste0("<b>", park, "</b>"), group = "Fields", layerId = ~id, fill = TRUE, color = "green") %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = ~icons[has_lights], popup = ~paste0("<b>", park, "</b>", "<br>", "Fields:", field_usage), group = "Fields", layerId = ~id)
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
      clearGroup(group = "Playgrounds") %>%
      addAwesomeMarkers(icon = ~icons[ada_accessible], popup = ~paste0("<b>", name, "</b>", "<br>", "Equipment:", equipment_type), group = "Playgrounds", layerId = ~id)
  })
  
  observe({
    crime <- crimeData()
    leafletProxy("leaflet", data = crime) %>%
      clearGroup(group = "Crime") %>%
      addHeatmap(group ="Crime")
  })
  
  # Subset to data Only on screen - will need to extract lat and lon from playground dataset later 
  onScreen <- reactive({
    #req(input$leaflet_bounds)
    #bounds <- input$leaflet_bounds
    #latRng <- range(bounds$north, bounds$south)
    #lngRng <- range(bounds$east, bounds$west)
    
    #subset(greenInfInputs(), latitude >= latRng[1] & latitude <= latRng[2] & longitude >= lngRng[1] & longitude <= lngRng[2])
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
