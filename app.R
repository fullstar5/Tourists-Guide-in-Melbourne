# ------------------------------- Introduction ------------------------------- #
# Group ID:
# Group Member: (Name - Student ID - Email?)
#   Student 1: Ruotong Zhao - 1076714
#   Student 2: YiFei ZHANG - 1174267
#   Student 3: Fan Pu - 1372387
#   Student 4:
# Subject: Information Visualisation (GEOM90007_2023_SM2)
# Project introduction: Assignment3

# ------------------------------ Import Library ------------------------------ #
library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet) # Map
library(dplyr)
library(leaflet.extras)
library(httr)
library(jsonlite)
library(purrr)
library(sf)
library(htmltools)
source('tableau-in-shiny-v1.0.R')

# ---------------------------- Variable Definition --------------------------- #
# read csv file
bar_data <- read.csv("new_data/bars-and-pubs-with-patron-capacity.csv")
# filter csv
bar_filtered_data <- bar_data %>%
  filter(!is.na(Business_address) &          # Business address
           !is.na(Longitude) &                   # Longitude
           !is.na(Latitude) &                    # Latitude
           Census_year > 2012 &               # Census year
           Census_year < 2023 &               # Census year
           grepl("bar", Trading_name, ignore.case = TRUE))  # Trading name

# ---------------------------- restaurant_data --------------------------- #
restaurant_data <- read.csv("new_data/cafes-and-restaurants-with-seating-capacity.csv")
# filter csv
restaurant_filtered_data <- restaurant_data %>%
  filter(!is.na(Trading_name) &          # Business address
           !is.na(Longitude) &                   # Longitude 
           !is.na(Latitude)                     # Latitude
  )

## Land Marks
landmarks_data <- read.csv("new_data/melbourne_city_landmarks(new).csv")

## Hotel & Dwelling & Coworking data
hotel_data <- read.csv("new_data/melbourne_and_metropolitan_hotels_pubs_and_publicans(new).csv")
dwelling_data <- read.csv("new_data/residential-dwellings.csv")
coworking_data <- read.csv("new_data/coworking-spaces.csv")

first_50_landmarks <- head(landmarks_data, 50)
first_50_bars <- head(bar_filtered_data, 50)
first_50_restaurants <- head(restaurant_filtered_data, 500)
first_500_hotels <- head(hotel_data, 100)
first_50_dwellings <- head(dwelling_data, 50)
first_50_coworkings <- head(coworking_data, 50)

## Page layout Settings
# Bootstrap Theme and dark mode (https://rstudio.github.io/bslib/articles/theming/index.html?q=dark%20mode#dynamic)
# Other theme: lumen,  Demo: https://testing-apps.shinyapps.io/themer-demo/
theme <- "lumen"
light <- bs_theme(bootswatch = theme)
dark <- bs_theme(bootswatch = theme, bg = "#212121", fg = "white")

# ------------------------------ Weather Extractor --------------------------- #
# Fetch weather data from the API
fetch_weather_data <- function(lat, lon, api_key) {
  # Construct the URL
  url <- paste0("https://api.openweathermap.org/data/2.5/weather?lat=", 
                lat, "&lon=", lon, "&appid=", api_key,"&units=metric")
  
  # Make the GET request
  response <- GET(url)
  
  # Parse the JSON content from the response
  content <- content(response, "text")
  data <- fromJSON(content, flatten = TRUE)
  
  return(data)
}

# Extract relevant information
get_main_weather_data <- function(lat, lon, api_key) {
  data <- fetch_weather_data(lat, lon, api_key)
  main_data <- data$main
  return(main_data)
}

# ------------------------------ USER INTERFACE ------------------------------ #
## Page components
# Left Page
map <- tabPanel(
  "Map",
  leafletOutput("map", height = "90vh")
)

userGuide <- tabPanel(
  "User Guide",
  # Accordion: https://rstudio.github.io/bslib/articles/sidebars/index.html#accordions
  accordion(
    accordion_panel("Introduction",
                    "Melbourne, the coastal capital of the southeastern Australian state of Victoria, is renowned for its rich cultural heritage, vibrant arts scene, and iconic landmarks. Recognizing the city's allure to global travelers, a dedicated team of data enthusiasts and developers from the the University of Melbourne embarked on a project to create a comprehensive digital guide for tourists. This Shiny application, developed with cutting-edge tools and libraries in R, offers an interactive map interface that showcases key attractions, accommodations, and leisure spots in Melbourne. Beyond just pinpointing locations, it integrates real-time weather data, ensuring visitors can plan their activities based on current conditions. Whether you're a leisure traveler keen to explore Melbourne's landmarks or a business visitor looking for coworking spaces, this app promises to be your indispensable companion. With its user-friendly design and wealth of information, tourists can now navigate the city with ease and make the most of their Melbourne experience."),
    accordion_panel(title = "User Guide for Tourist Melbourne",
                    tags$div(tags$b("Navigating the Map:"), 
                             tags$br(),
                             "1. Zooming In and Out:",
                             " Use the `+` and `-` buttons located on the top left corner of the map to zoom in or out. This allows you to view specific areas in greater detail or to get a broader view of the region.",
                             tags$br(),
                             "2. Panning the Map:",
                             " Click and drag the map to move around and explore different areas.",
                             tags$br(),
                             tags$b("Selecting Location Types:"),
                             tags$br(),
                             "1. Location Categories:",
                             " On the right side of the page, you'll find buttons representing different location categories such as 'Coworking', 'Hotel', 'Bar', etc.",
                             tags$br(),
                             "2. Displaying/Hiding Locations:",
                             " Clicking on these buttons will either display or hide the corresponding locations on the map. For example, if you only want to see 'Hotels', simply click on the 'Hotel' button.",
                             tags$br(),
                             tags$b("Detailed Selection:"),
                             tags$br(),
                             "1. Dropdown Lists:",
                             " Beneath the category buttons, there are dropdown lists that allow you to filter specific types of locations.",
                             tags$br(),
                             "2. Multiple Selections:",
                             " Multiple categories or specific types can be selected at once, and the map will display locations for all selected categories simultaneously.",
                             tags$br(),
                             tags$b("Location Information:"),
                             tags$br(),
                             "1. Accessing Information:",
                             " Upon clicking on any marker on the map, a popup will appear, displaying detailed information about that location.",
                             tags$br(),
                             "2. Navigation to Google Maps:",
                             " Within this popup, a navigation button is available. Clicking on it will redirect you to Google Maps, offering directions to the location.",
                             tags$br(),
                             tags$b("Current Weather Information:"),
                             tags$br(),
                             "1. Weather Data:",
                             " The bottom right corner of the map displays the current temperature, as well as the lowest and highest temperatures for the day, keeping you informed about the present weather conditions in Melbourne.",
                             tags$br(),
                             tags$b("Mini-Map Feature:"),
                             tags$br(),
                             "1. Overview of Melbourne:",
                             " Located at the bottom left corner of the main map is a mini-map that provides an overview of Melbourne.",
                             tags$br(),
                             "2. Jump to Melbourne:",
                             " Clicking on the 'Jump to Melbourne' button will center the main map to Melbourne, allowing for easy navigation back to the city if you've panned to a different area."
                    )
    ),
    accordion_panel("Data Source",
                    fluidRow(column(12, "melbourne_city_landmarks.csv ---- Melbourne landmarks coordinates, street and description（https://github.com/statelibraryvic/opendata/blob/master/melbourne_city_landmarks.csv")),
                    fluidRow(column(12, "melbourne_and_metropolitan_hotels_pubs_and_publicans(new) - https://discover.data.vic.gov.au/dataset/melbourne-and-metropolitan-hotels-pubs-and-publicans")),
                    fluidRow(column(12, "residential-dwellings.csv - https://data.melbourne.vic.gov.au/explore/dataset/residential-dwellings/export/")),
                    fluidRow(column(12, "coworking-spaces.csv - https://data.melbourne.vic.gov.au/explore/dataset/coworking-spaces/export/")),
                    fluidRow(column(12, "tram-tracks.geojson - https://data.melbourne.vic.gov.au/explore/dataset/tram-tracks/export/?location=12,-37.81397,144.95275&basemap=mbs-7a7333")),
                    fluidRow(column(12, "pedestrian-network.json - https://data.melbourne.vic.gov.au/explore/dataset/pedestrian-network/export/?location=16,-37.81111,144.95343&basemap=mbs-7a7333")),
                    fluidRow(column(12, "bars-and-pubs-with-patron-capacity - https://discover.data.vic.gov.au/dataset/bar-tavern-pub-patron-capacity")),
                    fluidRow(column(12, "cafes-and-restaurants-with-seating-capacity - https://discover.data.vic.gov.au/dataset/cafe-restaurant-bistro-seats")),
    ),
    accordion_panel("Bug Report",
                    fluidRow(column(12, "When click new marker after hiding information penal, the penal doesn't update anymore -- Fixing")),br(),
                    fluidRow(column(12, "Display all markers on map when app initiate, but it shouldn't -- Fixing")),br(),
    )
  )
)

tableau1 <- tableauPublicViz(
  id = "tableau1",
  url = "https://public.tableau.com/views/accidents_analysis/pie?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
  height = "79vh"
)
tableau2 <- tableauPublicViz(
  id = "tableau2",
  url = "https://public.tableau.com/views/1_16978614079580/1?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link",
  height = "79vh"
)
# ----------------------------------- UI ------------------------------------- #
## UI
ui <- navbarPage(
  "Tourist Melbourne",
  theme = light,
  
  # Set Style
  tags$head(
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css"),  # 添加Font Awesome
    
    tags$style("#draggable {
                  width: 45vh;
                  height: auto;
                  cursor: move;
                  position: absolute;
                  top: 10vh;
                  right: 2vh;
                  background-color: rgba(255, 255, 255, 0.8);
                  color: black;
                  padding: 0.5vh;
                  }
                
                #draggable2 {
                  width: 40vh;
                  height: auto;
                  cursor: move;
                  position: absolute;
                  top: 10vh;
                  right: 47vh;
                  background-color: rgba(255, 255, 255, 0.8);
                  color: black;
                  padding: 0.5vh;
                }

                #draggable h4 {
                  color: white;
                  background-color: black;
                  height: 4vh;
                  padding: 0.5vh;
                }
                
               #draggable2 h4 {
                  color: white;  
                  background-color: black;
                  height: 4vh;
                  padding: 0.5vh;
               }
                
               .custom-slider-container {
                  color: black;
                  display: flex;
                  margin-left: 1.5vh;
                  align-items: center;
                }
              .custom-slider-labels {
                  color: black;
                  margin: 0 1vh;
                  width: 70%;
               }"
    ),
    
    tags$script('$(document).ready(function() {
                   $( "#draggable" ).draggable();
                 });'),
    tags$script('$(document).ready(function() {
                   $( "#draggable2" ).draggable();
                 });'),
    tags$script(HTML("$(document).ready(function(){
                        $('#toggle-icon').click(function(){
                          $('#content-area').toggle();});
                      });")),
    tags$script(HTML("$(document).ready(function(){
                        $('#toggle-icon2').click(function(){
                          $('#content-area2').toggle();});
                      });")),
  ),
  
  # Direction
  tags$head(
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css"),
    tags$script(HTML("
    $(document).on('click', '.navIcon', function(){
      var coords = $(this).data('coords').split(',');
      var lat = coords[0];
      var lng = coords[1];
      var url = 'https://www.google.com/maps?q=' + lat + ',' + lng;
      window.open(url, '_blank');
    });
  "))
  ),
  
  # Page 1
  tabPanel("Map",
           map,
           tags$div(id = "draggable",
                    tags$h4("Map/Page Setting", 
                            tags$i(id="toggle-icon", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area",
                             tabsetPanel(id = "map",
                               tabPanel("Map Setting",
                                        #actionButton("jump_to_melbourne", "Back to Melbourne Area",
                                        #             style = "margin-top: 1vh; background-color: #F9F200; color: black;"), br(),
                                        actionButton("show_coworkings", "Coworking",
                                                     style = "margin-top: 1vh; background-color: purple; color: white; width: 14vh;"),
                                        actionButton("show_hotels", "Hotel",
                                                     style = "margin-top: 1vh; background-color: cadetblue; color: #333333; width: 14vh;"),
                                        actionButton("show_bars", "Bar",
                                                     style = "margin-top: 1vh; background-color: red; color: white; width: 14vh;"), 
                                        actionButton("show_landmarks", "Landmark",
                                                     style = "margin-top: 1vh; background-color: #0163FA; color: white; width: 14vh;"), 
                                        actionButton("show_dwellings", "Dwelling",
                                                     style = "margin-top: 1vh;  background-color: #4CAF50; color: white; width: 14vh;"),
                                        actionButton("show_restaurant", "Restaurant",
                                                     style = "margin-top: 1vh; background-color: orange; color: white; width: 14vh;"), 
                                        actionButton("toggle_tram_routes", "Tram",
                                                     style = "margin-top: 1vh; background-color: green; color: white; width: 43vh;"),
                                        br(),
                                        fluidRow(
                                          column(6, selectInput("choose_coworking", "Find Coworkings", choices = c("All Coworking", unique(first_50_coworkings$Organisation))), class = "select"),
                                          column(6, selectInput("choose_hotels", "Find Hotels", choices = c("All Hotels", unique(first_500_hotels$Title))), class = "select"),
                                        ),
                                        fluidRow(
                                          column(6, selectInput("choose_bars", "Find Bars", choices = c("All Bars", unique(first_50_bars$Trading_name))), class = "select"),
                                          column(6, selectInput("choose_landmark", "Find Landmarks", choices = c("All Landmarks", unique(first_50_landmarks$Title))), class = "select"),
                                        ),
                                        fluidRow(
                                          column(6, selectInput("choose_dwellings", "Dwelling type", choices = c("All Dwellings", unique(first_50_dwellings$Dwelling.type))), class = "select"),
                                          column(6, selectInput("choose_restaurants", "Find Restaurants", choices = c("All Restaurants", unique(first_50_restaurants$Trading_name))), class = "select"),)
                               ),
                               tabPanel("Page Setting", 
                                        div(class = "custom-slider-container",
                                            span("Light"),
                                            div(class = "custom-slider-labels",
                                                sliderInput("theme_slider", "", min = 0, max = 1, value = 0, step = 0.01)
                                            ),
                                            span("Dark")
                                        ),
                                        fluidRow(
                                          column(6, actionButton("light_mode", " Light Model", icon("sun"), 
                                                                 style = "color: black; background-color: #E8E8E8; margin-top: 1vh;")),
                                          column(6, actionButton("dark_mode", " Dark Model", icon("moon"), 
                                                                 style = "color: white; background-color: #212121; margin-top: 1vh;")),
                                        )
                                        ,
                               ),
                             ),
                    )
           ),
           tags$div(id = "draggable2",
                    tags$h4("Location Information", 
                            tags$i(id="toggle-icon2", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area2",
                             tabPanel("More Information",  uiOutput("more_information_content"),
                                      style = "margin-left: 1vh; margin-top: 1vh; margin-bottom: 1vh;"),
                    )
           ),
  ),
  
  header = setUpTableauInShiny(),
  tags$style(HTML("
    .custom-box {
      border: 2px solid #000;  /* black margin */
      background-color: #f9f9f9; /* light gray background */
    }
    .custom-plot {
      border: 2px solid black;  /* orange margin */
    }
    .tableau-plot{
      height: 80vh;
    }
  ")),
  tabPanel("Hotzones",
           fluidRow(
             column(width = 1), 
             column(width = 4,
                    box(title = "Traffic accident area", 
                        "South-east Melbourne tend to occur most accidnet",
                        class = "custom-box", width = "100%") 
             ),
             column(width = 2),
             column(width = 4,
                    box(title = "Pedestrian density", 
                        "People prefer to walk in Central and North Melbourne",
                        class = "custom-box", width = "100%")
             ),
             column(width = 1)
           ),
           fluidRow(column(width = 12, tags$div("", style = "height: 20px;"))),
           fluidRow(class = "tableau-plot",
                    column(width = 6, tableau1, class = "custom-plot"),
                    column(width = 6, tableau2, class = "custom-plot")
           )),
  tabPanel("About This Site", userGuide),
)


# ------------------------------- SHINY SERVER ------------------------------- #
server <- function(input, output, session) {
  
  # Define Awesome Icons
  bar_icon <- makeAwesomeIcon(icon = 'glass', markerColor = 'red', iconColor = 'white')
  restaurant_icon <- makeAwesomeIcon(icon = 'cutlery', markerColor = 'orange', iconColor = 'white')
  dwelling_icon <- makeAwesomeIcon(icon = 'home', markerColor = 'green', iconColor = 'white')
  coworking_icon <- makeAwesomeIcon(icon = 'briefcase', markerColor = "darkpurple", iconColor = 'white')
  landmark_icon <- makeAwesomeIcon(icon = 'map-marker', markerColor = 'blue', iconColor = 'white')
  hotel_icon <- makeAwesomeIcon(icon = 'bed', markerColor = 'cadetblue', iconColor = 'white')
  icon_html <- as.character(tags$a(id="navIcon", href="#", class="fas fa-map-signs"))
  
  # Melbourne coordinates
  lat <- -37.8136
  lon <- 144.9631
  # extract weather, temperature, and icon
  api_key <- "c0a65383e451468df23eb4bbedb94cb2"
  api_data <- fetch_weather_data(lat, lon, api_key)
  # extract temprature, weather and weather icon
  weather_data <- api_data$main
  weather_description <- api_data$weather$description[1]
  weather_icon_id <- api_data$weather$icon[1]
  
  
  #  https://rstudio.github.io/bslib/articles/theming/index.html?q=dark%20mode#dynamic
  # dynamic theme
  observeEvent(input$light_mode, {
    session$setCurrentTheme(light)
  })
  observeEvent(input$dark_mode, {
    session$setCurrentTheme(dark)
  })
  
  observeEvent(input$theme_slider, {
    slider_value <- input$theme_slider
    # background
    interpolate_color <- colorRamp(c("white", "black"))
    new_bg <- rgb(interpolate_color(slider_value), maxColorValue = 255)
    
    new_fg <- ifelse(slider_value > 0.5, "white", "black")
    
    new_theme <- bs_theme(bootswatch = "lumen", bg = new_bg, fg = new_fg)
    session$setCurrentTheme(new_theme)
  })
  
  
  # Selector
  ### coworking ---------------------------------------------------------------
  # Used to track button status
  coworking_visible <- reactiveVal(FALSE)
  observeEvent(input$show_coworkings, {
    proxy <- leafletProxy("map")
    # Hide data if it is currently visible
    if (coworking_visible()) {
      for (i in 1:nrow(first_50_coworkings)) {
        proxy <- removeMarker(proxy, layerId = paste0("coworking_", i))
      }
      coworking_visible(FALSE)
    } else {  # Displays if the data is currently hidden
      for (i in 1:nrow(first_50_coworkings)) {
        # Create the popup HTML content
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_coworkings[i, "Address"],
          first_50_coworkings[i, "latitude"],
          first_50_coworkings[i, "longitude"]
        )
        
        # Use this popup_content for your addAwesomeMarkers function
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_coworkings[i, "longitude"],
          lat = first_50_coworkings[i, "latitude"],
          icon = coworking_icon,
          popup = popup_content,  # Use the created popup content
          layerId = paste0("coworking_", i)
        )
      }
      coworking_visible(TRUE)
    }
  })
  
  observe({
    # if (!coworking_visible()) {
    #  return()
    #}
    
    selected_coworking <- input$choose_coworking
    proxy <- leafletProxy("map")
    
    # Remove all markder first
    for (i in 1:nrow(first_50_coworkings)) {
      proxy <- removeMarker(proxy, layerId = paste0("coworking_", i))
    }
    
    if (selected_coworking == "All Coworking") {
      # Add all coworking marker
      for (i in 1:nrow(first_50_coworkings)) {
        # Create the popup HTML content
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_coworkings[i, "Address"],
          first_50_coworkings[i, "latitude"],
          first_50_coworkings[i, "longitude"]
        )
        
        # Use this popup_content for your addAwesomeMarkers function
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_coworkings[i, "longitude"],
          lat = first_50_coworkings[i, "latitude"],
          icon = coworking_icon,
          popup = popup_content,  # Use the created popup content
          layerId = paste0("coworking_", i)
        )
      }
      coworking_visible(TRUE)
    } else {
      # Add a specific coworking marker
      selected_row <- first_50_coworkings[first_50_coworkings$Organisation == selected_coworking, ]
      proxy <- addAwesomeMarkers(
        proxy,
        lng = selected_row$longitude,
        lat = selected_row$latitude,
        icon = coworking_icon,
        popup = selected_row$Address,
        layerId = paste0("coworking_", which(first_50_coworkings$Organisation == selected_coworking))
      )
      coworking_visible(FALSE)  # Set to hidden because we only show one coworking
    }
  })
  
  ### hotels ---------------------------------------------------------------
  # same structure as before
  hotels_visible <- reactiveVal(FALSE)
  observeEvent(input$show_hotels, {
    proxy <- leafletProxy("map")
    if (hotels_visible()) {
      for (i in 1:nrow(first_500_hotels)) {
        proxy <- removeMarker(proxy, layerId = paste0("hotel_", i))
      }
      hotels_visible(FALSE)
    } else {  
      for (i in 1:nrow(first_500_hotels)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_500_hotels[i, "Location"],
          first_500_hotels[i, "latitude"],
          first_500_hotels[i, "longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_500_hotels[i, "longitude"],
          lat = first_500_hotels[i, "latitude"],
          icon = hotel_icon,
          popup = popup_content,
          layerId = paste0("hotel_", i)
        )
      }
      hotels_visible(TRUE)
    }
  })
  
  observe({
    selected_hotel <- input$choose_hotels
    proxy <- leafletProxy("map")
    for (i in 1:nrow(first_500_hotels)) {
      proxy <- removeMarker(proxy, layerId = paste0("hotel_", i))
    }
    
    if (selected_hotel == "All Hotels") {
      for (i in 1:nrow(first_500_hotels)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_500_hotels[i, "Location"],
          first_500_hotels[i, "latitude"],
          first_500_hotels[i, "longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_500_hotels[i, "longitude"],
          lat = first_500_hotels[i, "latitude"],
          icon = hotel_icon,
          popup = popup_content,
          layerId = paste0("hotel_", i)
        )
      }
      hotels_visible(TRUE)
    } else {
      selected_row <- first_500_hotels[first_500_hotels$Title == selected_hotel, ]
      proxy <- addAwesomeMarkers(
        proxy,
        lng = selected_row$longitude,
        lat = selected_row$latitude,
        icon = hotel_icon,
        popup = selected_row$Location,
        layerId = paste0("hotel_", which(first_500_hotels$Title == selected_hotel))
      )
      hotels_visible(FALSE)
    }
  })
  
  ### Bars ---------------------------------------------------------------
  bars_visible <- reactiveVal(FALSE)
  
  observeEvent(input$show_bars, {
    proxy <- leafletProxy("map")
    if (bars_visible()) {
      for (i in 1:nrow(first_50_bars)) {
        proxy <- removeMarker(proxy, layerId = paste0("bar_", i))
      }
      bars_visible(FALSE)
    } else {  
      for (i in 1:nrow(first_50_bars)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_bars[i, "Business_address"],
          first_50_bars[i, "Latitude"],
          first_50_bars[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_bars[i, "Longitude"],
          lat = first_50_bars[i, "Latitude"],
          icon = bar_icon,
          popup = popup_content,
          layerId = paste0("bar_", i)
        )
      }
      bars_visible(TRUE)
    }
  })
  
  observe({
    selected_bar <- input$choose_bars
    proxy <- leafletProxy("map")
    for (i in 1:nrow(first_50_bars)) {
      proxy <- removeMarker(proxy, layerId = paste0("bar_", i))
    }
    if (selected_bar == "All Bars") {
      for (i in 1:nrow(first_50_bars)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_bars[i, "Business_address"],
          first_50_bars[i, "Latitude"],
          first_50_bars[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_bars[i, "Longitude"],
          lat = first_50_bars[i, "Latitude"],
          icon = bar_icon,
          popup = popup_content,
          layerId = paste0("bar_", i)
        )
      }
      bars_visible(TRUE)
    } else {
      selected_row <- first_50_bars[first_50_bars$Trading_name == selected_bar, ]
      proxy <- addAwesomeMarkers(
        proxy,
        lng = selected_row$Longitude,
        lat = selected_row$Latitude,
        icon = bar_icon,
        popup = selected_row$Business_address,
        layerId = paste0("bar_", which(first_50_bars$Trading_name == selected_bar))
      )
      bars_visible(FALSE) 
    }
  })
  
  
  ### Restaurants ---------------------------------------------------------------
  restaurants_visible <- reactiveVal(FALSE)
  
  observeEvent(input$show_restaurant, {
    proxy <- leafletProxy("map")
    if (restaurants_visible()) {
      for (i in 1:nrow(first_50_restaurants)) {
        proxy <- removeMarker(proxy, layerId = paste0("restaurant_", i))
      }
      restaurants_visible(FALSE)
    } else {  
      for (i in 1:nrow(first_50_restaurants)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_restaurants[i, "Business_address"],
          first_50_restaurants[i, "Latitude"],
          first_50_restaurants[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_restaurants[i, "Longitude"],
          lat = first_50_restaurants[i, "Latitude"],
          icon = restaurant_icon,
          popup = popup_content,
          layerId = paste0("restaurant_", i)
        )
      }
      restaurants_visible(TRUE)
    }
  })
  
  observe({
    selected_restaurant <- input$choose_restaurants
    proxy <- leafletProxy("map")
    for (i in 1:nrow(first_50_restaurants)) {
      proxy <- removeMarker(proxy, layerId = paste0("restaurant_", i))
    }
    if (selected_restaurant == "All Restaurants") {
      for (i in 1:nrow(first_50_restaurants)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_restaurants[i, "Business_address"],
          first_50_restaurants[i, "Latitude"],
          first_50_restaurants[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_restaurants[i, "Longitude"],
          lat = first_50_restaurants[i, "Latitude"],
          icon = restaurant_icon,
          popup = popup_content,
          layerId = paste0("restaurant_", i)
        )
      }
      restaurants_visible(TRUE)
    } else {
      selected_row <- first_50_restaurants[first_50_restaurants$Trading_name == selected_restaurant, ]
      proxy <- addAwesomeMarkers(
        proxy,
        lng = selected_row$Longitude,
        lat = selected_row$Latitude,
        icon = restaurant_icon,
        popup = selected_row$Business_address,
        layerId = paste0("restaurant_", which(first_50_restaurants$Trading_name == selected_restaurant))
      )
      restaurants_visible(FALSE) 
    }
  })
  
  ### dwellings ---------------------------------------------------------------
  dwellings_visible <- reactiveVal(FALSE)
  
  observeEvent(input$show_dwellings, {
    proxy <- leafletProxy("map")
    if (dwellings_visible()) {
      for (i in 1:nrow(first_50_dwellings)) {
        proxy <- removeMarker(proxy, layerId = paste0("dwelling_", i))
      }
      dwellings_visible(FALSE)
    } else {
      for (i in 1:nrow(first_50_dwellings)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_dwellings[i, "Address"],
          first_50_dwellings[i, "Latitude"],
          first_50_dwellings[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_dwellings[i, "Longitude"],
          lat = first_50_dwellings[i, "Latitude"],
          icon = dwelling_icon,
          popup = popup_content,
          layerId = paste0("dwelling_", i)
        )
      }
      dwellings_visible(TRUE)
    }
  })
  
  observe({
    selected_dwelling <- input$choose_dwellings
    proxy <- leafletProxy("map")
    for (i in 1:nrow(first_50_dwellings)) {
      proxy <- removeMarker(proxy, layerId = paste0("dwelling_", i))
    }
    
    if (selected_dwelling == "All Dwellings") {
      for (i in 1:nrow(first_50_dwellings)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_dwellings[i, "Address"],
          first_50_dwellings[i, "Latitude"],
          first_50_dwellings[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_dwellings[i, "Longitude"],
          lat = first_50_dwellings[i, "Latitude"],
          icon = dwelling_icon,
          popup = popup_content,
          layerId = paste0("dwelling_", i)
        )
      }
      dwellings_visible(TRUE)
    } else {
      selected_rows <- first_50_dwellings[first_50_dwellings$Dwelling.type == selected_dwelling, ]
      for (i in 1:nrow(selected_rows)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_dwellings[i, "Address"],
          first_50_dwellings[i, "Latitude"],
          first_50_dwellings[i, "Longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_dwellings[i, "Longitude"],
          lat = first_50_dwellings[i, "Latitude"],
          icon = dwelling_icon,
          popup = popup_content,
          layerId = paste0("dwelling_", i)
        )
      }
      dwellings_visible(FALSE)
    }
  })
  
  
  
  ### landmarks -----------------------------------------------------------
  landmarks_visible <- reactiveVal(FALSE)
  
  observeEvent(input$show_landmarks, {
    proxy <- leafletProxy("map")
    if (landmarks_visible()) {
      for (i in 1:nrow(first_50_landmarks)) {
        proxy <- removeMarker(proxy, layerId = paste0("landmark_", i))
      }
      landmarks_visible(FALSE)
    } else {  
      for (i in 1:nrow(first_50_landmarks)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_landmarks[i, "Title"],
          first_50_landmarks[i, "latitude"],
          first_50_landmarks[i, "longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_landmarks[i, "longitude"],
          lat = first_50_landmarks[i, "latitude"],
          icon = landmark_icon,
          popup = popup_content,
          layerId = paste0("landmark_", i)
        )
      }
      landmarks_visible(TRUE)
    }
  })
  
  observe({
    selected_landmark <- input$choose_landmark
    proxy <- leafletProxy("map")
    for (i in 1:nrow(first_50_landmarks)) {
      proxy <- removeMarker(proxy, layerId = paste0("landmark_", i))
    }
    if (selected_landmark == "All Landmarks") {
      for (i in 1:nrow(first_50_landmarks)) {
        popup_content <- sprintf(
          '<div>%s</div><a href="#" class="navIcon" data-coords="%s,%s"><i class="fas fa-map-signs"></i></a>',
          first_50_landmarks[i, "Title"],
          first_50_landmarks[i, "latitude"],
          first_50_landmarks[i, "longitude"]
        )
        
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_landmarks[i, "longitude"],
          lat = first_50_landmarks[i, "latitude"],
          icon = landmark_icon,
          popup = popup_content,
          layerId = paste0("landmark_", i)
        )
      }
      landmarks_visible(TRUE)
    } else {
      selected_row <- first_50_landmarks[first_50_landmarks$Title == selected_landmark, ]
      proxy <- addAwesomeMarkers(
        proxy,
        lng = selected_row$longitude,
        lat = selected_row$latitude,
        icon = landmark_icon,
        popup = selected_row$Title,
        layerId = paste0("landmark_", which(first_50_landmarks$Title == selected_landmark))
      )
      landmarks_visible(FALSE) 
    }
  })
  
 
  ## Map
  output$map <- renderLeaflet({
    # Create Leaflet Map
    m <- leaflet() %>%
      addTiles() %>%
      # Mini Map
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        toggleDisplay = TRUE, position = "bottomleft") %>%
      # zoom map to Melbourne
      setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
      addControl(html = "<button id='jumpButton'>Jump to Melbourne</button>", position = "bottomleft")
    
    
    # react to click
    address_info <- reactiveVal("")
    # listen to click
    observeEvent(input$map_marker_click, {
      event <- input$map_marker_click
      if (is.null(event)) {
        return() 
      }
      marker_id <- event$id
      marker_type <- gsub("_[0-9]+$", "", marker_id)  # Extracting the marker type
      index <- as.numeric(gsub("^[a-z_]+_", "", marker_id))  # Extracting the index
      
      # recognise click data type
      if (marker_type == "bar") {
        if (!is.na(index) && index >= 1 && index <= nrow(bar_filtered_data)) {
          address <- bar_filtered_data[index, "Business_address"]
          trading_name <- bar_filtered_data[index, "Trading_name"]
        }
      } else if (marker_type == "coworking") {
        if (!is.na(index) && index >= 1 && index <= nrow(coworking_data)) {
          address <- coworking_data[index, "Address"]
        }
      } else if (marker_type == "hotel") {
        if (!is.na(index) && index >= 1 && index <= nrow(hotel_data)) {
          address <- hotel_data[index, "Location"]
        }
      } else if (marker_type == "landmark") {
        if (!is.na(index) && index >= 1 && index <= nrow(landmarks_data)) {
          address <- landmarks_data[index, "Title"]
          trading_name <- trimws(unlist(strsplit(landmarks_data[index, "Title"], ","))[1])
        }
      } else if (marker_type == "dwelling") {
        if (!is.na(index) && index >= 1 && index <= nrow(dwelling_data)) {
          address <- dwelling_data[index, "Address"]
        }
      } else if (marker_type == "restaurant") {
        if (!is.na(index) && index >= 1 && index <= nrow(restaurant_filtered_data)) {
          address <- restaurant_filtered_data[index, "Business_address"]
          trading_name <- restaurant_filtered_data[index, "Trading_name"]
          seats <- restaurant_filtered_data[index, "seats"]
        }
      }
      # Construct business hours information
      days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      hours <- "7:00 to 18:00"
      business_hours_info <- paste(days, hours, sep=": ", collapse="<br>")
      
      # Append business hours to the fetched address
      if (marker_type == "bar" || marker_type == "landmark") {
        full_info <- paste("<strong>Location Type:</strong><br>", marker_type, "<br><br><strong>Location Name:</strong><br>", trading_name, "<br><br><strong>Address:</strong><br>", address, "<br><br><strong>Business Hours:</strong><br>", business_hours_info, sep="")
      }  else if(marker_type == "restaurant") {
        full_info <- paste("<strong>Location Type:</strong><br>", marker_type, "<br><br><strong>Location Name:</strong><br>", trading_name, "<br><br><strong>Address:</strong><br>", address, "<br><br><strong>Number of Seats:</strong><br>", seats, "<br><br><strong>Business Hours:</strong><br>", business_hours_info, sep="")
      } else if(marker_type == "dwelling") {
        full_info <- paste("<strong>Location Type:</strong><br>", marker_type, "<br><br><strong>Address:</strong><br>", address)
      } else {
        full_info <- paste("<strong>Location Type:</strong><br>", marker_type, "<br><br><strong>Address:</strong><br>", address, "<br><br><strong>Business Hours:</strong><br>", business_hours_info, sep="")
      }
      # update information on click
      address_info(full_info)
    })
    # display information
    output$more_information_content <- renderText({
      address_info()
    })
    
    
    # Weather info as HTML
    weather_icon_url <- paste0("http://openweathermap.org/img/wn/", weather_icon_id, ".png")
    weather_info <- paste0("<div id='weatherControl'><img src='", weather_icon_url, 
                           "' alt='Weather icon' class='weather-icon'><br>",
                           "<strong>Current Temperature:</strong> ", weather_data$temp, " °C<br>",
                           "<strong>Lowest Temperature:</strong> ", weather_data$temp_min, " °C<br>",
                           "<strong>Highest Temperature:</strong> ", weather_data$temp_max, " °C</div>")
    # Add custom control to display weather info, this is toggle-able
    m <- addControl(m, html=weather_info, position="bottomright")
    m %>% addEasyButton(easyButton(
      icon = "fa-sun", title = "Toggle Weather Info",
      onClick = JS("function(btn, map){
                     var control = document.getElementById('weatherControl');
                     if (control.style.display === 'none' || control.style.display === '') {
                       control.style.display = 'block';
                     } else {
                       control.style.display = 'none';
                     }
                   }")))
  })
  
  # When the "Jump to Melbourne" button is clicked, update the map view
  observe({
    # Use shinyjs to detect when the button is clicked
    shinyjs::onclick("jumpButton", {
      leafletProxy("map") %>%
        setView(lng=lon, lat=lat, zoom=14)  # Update view to Melbourne
    })
  })
  
  #Tram Line
  data_sf <- st_read("new_data/tram-tracks.geojson", quiet = TRUE)
  
  # Tram routes visibility flag
  tram_routes_visible <- reactiveVal(FALSE)  # Assuming tram routes are hidden by default
  
  observeEvent(input$toggle_tram_routes, {
    tram_routes_visible(!tram_routes_visible())  
    proxy <- leafletProxy("map")
    
    if (tram_routes_visible()) {
      # Display each tram line in green color
      proxy %>% 
        addPolygons(data = data_sf, color = "green", weight = 2, opacity = 1)
    } else {
      proxy %>% clearShapes()
    }
  })
  
}


# -------------------------------- RUN SHINY --------------------------------- #
shinyApp(ui, server, options = list(launch.browser = TRUE))
