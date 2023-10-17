# ------------------------------- Introduction ------------------------------- #
# Group ID:
# Group Member: (Name - Student ID - Email?)
#   Student 1: Ruotong Zhao - 1076714
#   Student 2:
#   Student 3:
#   Student 4:
# Subject: Information Visualisation (GEOM90007_2023_SM2)
# Project introduction: Assignment3

# ------------------------------ Import Library ------------------------------ #
library("shiny")
library(shinydashboard)
library(bslib)
library(leaflet) # Map
library(dplyr)
library(leaflet.extras)
library(httr)
library(jsonlite)

# ---------------------------- Variable Definition --------------------------- #
## Bar display
# 读取CSV数据集
bar_data <- read.csv("new_data/bars-and-pubs-with-patron-capacity.csv")
# 过滤数据集
bar_filtered_data <- bar_data %>%
  filter(!is.na(Business_address) &          # Business address 不为空
           !is.na(Longitude) &                   # Longitude 不为空
           !is.na(Latitude) &                    # Latitude 不为空
           Census_year > 2012 &               # Census year 大于 2022
           Census_year < 2023 &               # Census year 小于 2023
           grepl("bar", Trading_name, ignore.case = TRUE))  # Trading name 包含 "bar"（不区分大小写）

## Land Marks
landmarks_data <- read.csv("new_data/melbourne_city_landmarks(new).csv")

## Hotel & Dwelling & Coworking data
hotel_data <- read.csv("dataset/melbourne_and_metropolitan_hotels_pubs_and_publicans(new).csv")
dwelling_data <- read.csv("dataset/residential-dwellings.csv")
coworking_data <- read.csv("dataset/coworking-spaces.csv")

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
                    "This project ..."),
    accordion_panel("Data Source",
                    "Data Source Describe")
  )
)
# ----------------------------------- UI ------------------------------------- #
## UI
ui <- navbarPage(
  "Tourist Melbourne",
  theme = bs_theme(bootswatch = "sketchy"),
  
  # Set Style
  tags$head(
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css"),  # 添加Font Awesome

    tags$style("#draggable {
                  width: 45vh;
                  height: auto;  # 改为自动，以适应内容
                  cursor: move;
                  position: absolute;
                  top: 90px;
                  right: 30px;
                }

                #draggable h3 {
                  background-color: #B6E13D;  # 标题的背景颜色
                  height: 8vh;
                  padding: 10px;  # 添加内边距
                }"),
    
    tags$script('$(document).ready(function() {
                   $( "#draggable" ).draggable();
                 });'),
    tags$script(HTML("$(document).ready(function(){
                        $('#toggle-icon').click(function(){
                          $('#content-area').toggle();});
                      });"))
  ),
  
  # Page 1
  tabPanel("Map",
           map,
           tags$div(id = "draggable",
                    style = "background-color: rgba(255, 255, 255, 0.6);",
                    tags$h3("Title", 
                            tags$i(id="toggle-icon", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area",
                      tabsetPanel(
                        tabPanel("Button",
                                 actionButton("jump_to_melbourne", "Jump to Melbourne",
                                               style = "margin-top: 10px; background-color: #F9F200; color: black;"), br(),
                                 actionButton("show_landmarks", "Show Landmarks",
                                               style = "margin-top: 10px; background-color: #0163FA; color: white;"), br(),
                                 actionButton("show_bars", "Show Bars",
                                               style = "margin-top: 10px; background-color: #9711FA; color: white;"), br(),
                                 actionButton("show_hotels", "Show hotels",
                                               style = "margin-top: 10px; background-color: #9711FA; color: white;"), br(),
                                 actionButton("show_dwellings", "Show dwellings",
                                               style = "margin-top: 10px; background-color: #9711FA; color: white;"), br(),
                                 actionButton("show_coworkings", "Show coworkings",
                                              style = "margin-top: 10px; background-color: #9711FA; color: white;"),  br(),),
                        tabPanel("2", "22"),
                      ),
                    )
           ),
  ),
  tabPanel("Plot2", "Plot2"),
  tabPanel("User Guide", userGuide),
)


# ------------------------------- SHINY SERVER ------------------------------- #
server <- function(input, output, session) {
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
  
  first_50_landmarks <- head(landmarks_data, 50)
  first_50_bars <- head(bar_filtered_data, 50)
  first_50_hotels <- head(hotel_data, 50)
  first_50_dwellings <- head(dwelling_data, 50)
  first_50_coworkings <- head(coworking_data, 50)
  
  #  https://rstudio.github.io/bslib/articles/theming/index.html?q=dark%20mode#dynamic
  observeEvent(input$light_mode, {
    session$setCurrentTheme(light)
  })
  observeEvent(input$dark_mode, {
    session$setCurrentTheme(dark)
  })
  
  # display landmarks
  landmarks_visible <- reactiveVal(FALSE)
  observeEvent(input$show_landmarks, {
    landmarks_visible(!landmarks_visible())  # Toggle the value
    if (landmarks_visible()) {
      landmark_icon <- makeIcon(iconUrl = "icons/landmark_icon.png", iconWidth = 30, iconHeight = 30)
      proxy <- leafletProxy("map")
      proxy %>% clearMarkers()  # Clear old markers
      for (i in 1:nrow(first_50_landmarks)) {
        proxy <- addMarkers(
          proxy,
          lng = first_50_landmarks[i, "longitude"],
          lat = first_50_landmarks[i, "latitude"],
          icon = landmark_icon,
          popup = first_50_landmarks[i, "Title"],
          layerId = paste0("landmark_", i)
        )
      }
    } else {
      proxy <- leafletProxy("map")
      for (i in 1:nrow(first_50_landmarks)) {
        proxy <- removeMarker(proxy, layerId = paste0("landmark_", i))
      }
    }
  })
  
  # Display bars
  bars_visible <- reactiveVal(FALSE)  # Assuming bars are visible by default
  observeEvent(input$show_bars, {
    bars_visible(!bars_visible())  # Toggle the value
    proxy <- leafletProxy("map")
    if (bars_visible()) {
      bar_icon <- makeIcon(iconUrl = "icons/bar_icon.png", iconWidth = 30, iconHeight = 30)
      for (i in 1:nrow(first_50_bars)) {
        proxy <- addMarkers(
          proxy,
          lng = first_50_bars[i, "Longitude"],
          lat = first_50_bars[i, "Latitude"],
          icon = bar_icon,
          popup = first_50_bars[i, "Business_address"],
          layerId = paste0("bar_", i)
        )
      }
    } else {
      for (i in 1:nrow(first_50_bars)) {
        proxy <- removeMarker(proxy, layerId = paste0("bar_", i))
      }
    }
  })
  
  
  # Display hotels
  hotels_visible <- reactiveVal(FALSE)
  observeEvent(input$show_hotels, {
    hotels_visible(!hotels_visible())  # Toggle the value
    proxy <- leafletProxy("map")
    if (hotels_visible()) {
      hotel_icon <- makeIcon(iconUrl = "hotel_icon.png", iconWidth = 30, iconHeight = 30)
      for (i in 1:nrow(first_50_hotels)) {
        proxy <- addMarkers(
          proxy,
          lng = first_50_hotels[i, "longitude"],
          lat = first_50_hotels[i, "latitude"],
          icon = hotel_icon,
          popup = first_50_hotels[i, "Location"],
          layerId = paste0("hotel_", i)
        )
      }
    } else {
      for (i in 1:nrow(first_50_hotels)) {
        proxy <- removeMarker(proxy, layerId = paste0("hotel_", i))
      }
    }
  })
  
  
  # Display dwellings
  dwellings_visible <- reactiveVal(FALSE)
  observeEvent(input$show_dwellings, {
    dwellings_visible(!dwellings_visible())  # Toggle the value
    proxy <- leafletProxy("map")
    if (dwellings_visible()) {
      dwelling_icon <- makeIcon(iconUrl = "dwelling-icon.png", iconWidth = 30, iconHeight = 30)
      for (i in 1:nrow(first_50_dwellings)) {
        proxy <- addMarkers(
          proxy,
          lng = first_50_dwellings[i, "Longitude"],
          lat = first_50_dwellings[i, "Latitude"],
          icon = dwelling_icon,
          popup = first_50_dwellings[i, "Building_address"],
          layerId = paste0("dwelling_", i)
        )
      }
    } else {
      for (i in 1:nrow(first_50_dwellings)) {
        proxy <- removeMarker(proxy, layerId = paste0("dwelling_", i))
      }
    }
  })
  
  # Display coworking spaces
  coworking_visible <- reactiveVal(FALSE)
  observeEvent(input$show_coworkings, {
    coworking_visible(!coworking_visible())  # Toggle the value
    proxy <- leafletProxy("map")
    if (coworking_visible()) {
      coworking_icon <- makeIcon(iconUrl = "coworking-icon.png", iconWidth = 30, iconHeight = 30)
      for (i in 1:nrow(first_50_coworkings)) {
        proxy <- addMarkers(
          proxy,
          lng = first_50_coworkings[i, "longitude"],
          lat = first_50_coworkings[i, "latitude"],
          icon = coworking_icon,
          popup = first_50_coworkings[i, "Address"],
          layerId = paste0("coworking_", i)
        )
      }
    } else {
      for (i in 1:nrow(first_50_coworkings)) {
        proxy <- removeMarker(proxy, layerId = paste0("coworking_", i))
      }
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
      # 将地图聚焦在墨尔本区域
      setView(lng = 144.9631, lat = -37.8136, zoom = 12)
    
    # if (input$show_bar_icons) {
    #   # Add bar icons to the map if the checkbox is checked
    #   for (i in 1:nrow(bar_filtered_data)) {
    #     bar_icon <- makeIcon(iconUrl = "bar_icon.png", iconWidth = 30, iconHeight = 30)
    #     m <- addMarkers(
    #       m,
    #       lng = bar_filtered_data[i, "Longitude"],
    #       lat = bar_filtered_data[i, "Latitude"],
    #       icon = bar_icon,
    #       popup = bar_filtered_data[i, "Business_address"],
    #       layerId = paste0("bar_", i)
    #     )
    #   }
    # }
    
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
  observeEvent(input$jump_to_melbourne, {
    leafletProxy("map") %>%
      setView(lng=lon, lat=lat, zoom=12)  # Update view to Melbourne
  })
  
  
}


# -------------------------------- RUN SHINY --------------------------------- #
shinyApp(ui, server)
