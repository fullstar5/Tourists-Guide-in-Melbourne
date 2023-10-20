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
library(purrr)
library(sf)
library(shinyjs)

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
hotel_data <- read.csv("new_data/melbourne_and_metropolitan_hotels_pubs_and_publicans(new).csv")
dwelling_data <- read.csv("new_data/residential-dwellings.csv")
coworking_data <- read.csv("new_data/coworking-spaces.csv")

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
                    "This project ..."),
    accordion_panel("Data Source",
                    "Data Source Describe")
  )
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
                  height: auto;  # 改为自动，以适应内容
                  cursor: move;
                  position: absolute;
                  top: 12vh;
                  right: 3vh;
                  background-color: rgba(255, 255, 255, 0.8);
                  color: black;
                  }
                
                #draggable2 {
                  width: 45vh;
                  height: auto;  # 改为自动，以适应内容
                  cursor: move;
                  position: absolute;
                  top: 45vh;
                  right: 3vh;
                  background-color: rgba(255, 255, 255, 0.8);
                  color: black;
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
                  margin-left: 15px;
                  align-items: center;
                }
              .custom-slider-labels {
                  color: black;
                  margin: 0 10px;
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
  
  # Page 1
  tabPanel("Map",
           useShinyjs(),
           map,
           tags$div(id = "draggable",
                    tags$h4("Title", 
                            tags$i(id="toggle-icon", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area",
                             tabsetPanel(
                               tabPanel("Map Setting",
                                        actionButton("jump_to_melbourne", "Back to Melbourne Area",
                                                     style = "margin-left: 10px; margin-top: 10px; background-color: #F9F200; color: black;"), br(),
                                        actionButton("show_coworkings", "Coworking",
                                                     style = "margin-left: 10px; margin-top: 10px; background-color: purple; color: white; width: 14vh;"),
                                        actionButton("show_hotels", "Hotel",
                                                     style = "margin-top: 10px; background-color: #FFD740; color: #333333; width: 14vh;"),
                                        actionButton("show_bars", "Bar",
                                                     style = "margin-top: 10px; background-color: red; color: white; width: 14vh;"), 
                                        actionButton("show_landmarks", "Landmark",
                                                     style = "margin-left: 10px; margin-top: 10px; margin-bottom: 10px; background-color: #0163FA; color: white; width: 14vh;"), 
                                        actionButton("show_dwellings", "Dwelling",
                                                     style = "margin-top: 10px;  margin-bottom: 10px; background-color: #4CAF50; color: white; width: 14vh;"), 
                                        actionButton("toggle_tram_routes", "Tram",
                                                     style = "margin-top: 10px;  margin-bottom: 10px; background-color: green; color: white; width: 14vh;")
                                        ),
                               tabPanel("Page Setting", 
                                        div(class = "custom-slider-container",
                                            span("Light"),
                                            div(class = "custom-slider-labels",
                                                sliderInput("theme_slider", "", min = 0, max = 1, value = 0, step = 0.01)
                                            ),
                                            span("Dark")
                                        ),
                                        actionButton("light_mode", " Light Model", icon("sun"), 
                                                     style = "margin-left: 20px; margin-top: 10px; margin-bottom: 10px; color: black; background-color: #E8E8E8; margin-top: 10px;"),
                                        actionButton("dark_mode", " Dark Model", icon("moon"), 
                                                     style = "margin-left: 100px; margin-top: 10px; margin-bottom: 10px; color: white; background-color: #212121; margin-top: 10px;"),),
                             ),
                    )
           ),
           tags$div(id = "draggable2",
                    tags$h4("Information", 
                            tags$i(id="toggle-icon2", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area2",
                             uiOutput("draggable2_content")
                    )
           ),
  ),
  tabPanel("Plot2", "Plot2"),
  tabPanel("User Guide", userGuide),
)


# ------------------------------- SHINY SERVER ------------------------------- #
server <- function(input, output, session) {
  
  # Define Awesome Icons
  bar_icon <- makeAwesomeIcon(icon = 'glass', markerColor = 'red', iconColor = 'white')
  dwelling_icon <- makeAwesomeIcon(icon = 'home', markerColor = 'green', iconColor = 'white')
  coworking_icon <- makeAwesomeIcon(icon = 'briefcase', markerColor = "darkpurple", iconColor = 'white')
  
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
  first_500_hotels <- head(hotel_data, 100)
  first_50_dwellings <- head(dwelling_data, 50)
  first_50_coworkings <- head(coworking_data, 50)
  
  #  https://rstudio.github.io/bslib/articles/theming/index.html?q=dark%20mode#dynamic
  observeEvent(input$light_mode, {
    session$setCurrentTheme(light)
  })
  observeEvent(input$dark_mode, {
    session$setCurrentTheme(dark)
  })
  
  observeEvent(input$theme_slider, {
    slider_value <- input$theme_slider
    # 插值背景颜色
    interpolate_color <- colorRamp(c("white", "black"))
    new_bg <- rgb(interpolate_color(slider_value), maxColorValue = 255)
    
    # 根据背景亮度选择前景颜色
    new_fg <- ifelse(slider_value > 0.5, "white", "black")
    
    new_theme <- bs_theme(bootswatch = "lumen", bg = new_bg, fg = new_fg)
    session$setCurrentTheme(new_theme)
  })
  
  
  # display landmarks
  landmarks_visible <- reactiveVal(FALSE)
  observeEvent(input$show_landmarks, {
    landmarks_visible(!landmarks_visible())  # Toggle the value
    if (landmarks_visible()) {
      landmark_icon <- makeAwesomeIcon(icon = 'map-marker', markerColor = 'blue', iconColor = 'white')
      proxy <- leafletProxy("map")
      proxy %>% clearMarkers()  # Clear old markers
      for (i in 1:nrow(first_50_landmarks)) {
        proxy <- addAwesomeMarkers(
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
      for (i in 1:nrow(first_50_bars)) {
        proxy <- addAwesomeMarkers(
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
      hotel_icon <- makeAwesomeIcon(icon = 'bed', markerColor = 'orange', iconColor = 'white')
      for (i in 1:nrow(first_500_hotels)) {
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_500_hotels[i, "longitude"],
          lat = first_500_hotels[i, "latitude"],
          icon = hotel_icon,
          popup = first_500_hotels[i, "Location"],
          layerId = paste0("hotel_", i)
        )
      }
    } else {
      for (i in 1:nrow(first_500_hotels)) {
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
      for (i in 1:nrow(first_50_dwellings)) {
        proxy <- addAwesomeMarkers(
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
      for (i in 1:nrow(first_50_coworkings)) {
        proxy <- addAwesomeMarkers(
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
      setView(lng = 144.9631, lat = -37.8136, zoom = 14)
    
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
      setView(lng=lon, lat=lat, zoom=14)  # Update view to Melbourne
  })
  
  
  # Display information Penal
  output$draggable2_content <- renderUI({
    # Detect marker click
    click <- input$map_marker_click
    if (is.null(click)) return()
    layer_id <- as.character(click$layerId)
    
    # Determine the dataset based on layerId
    dataset <- NULL
    if (startsWith(layer_id, "landmark_")) {
      dataset <- "landmark"
      data <- first_50_landmarks
      idx <- as.numeric(sub("landmark_", "", layer_id))
    } else if (startsWith(layer_id, "bar_")) {
      dataset <- "bar"
      data <- first_50_bars
      idx <- as.numeric(sub("bar_", "", layer_id))
    } else if (startsWith(layer_id, "hotel_")) {
      dataset <- "hotel"
      data <- first_500_hotels
      idx <- as.numeric(sub("hotel_", "", layer_id))
    } else if (startsWith(layer_id, "dwelling_")) {
      dataset <- "dwelling"
      data <- first_50_dwellings
      idx <- as.numeric(sub("dwelling_", "", layer_id))
    } else if (startsWith(layer_id, "coworking_")) {
      dataset <- "coworking"
      data <- first_50_coworkings
      idx <- as.numeric(sub("coworking_", "", layer_id))
    }
    
    # Extract and format the information
    address <- switch(dataset,
                      landmark = data$Title[idx],
                      bar = data$Business_address[idx],
                      hotel = data$Location[idx],
                      dwelling = data$Building_address[idx],
                      coworking = data$Address[idx])
    
    description <- ifelse(is.na(data$Description[idx]), "None", data$Description[idx])
    
    business_hours <- "7:00am to 18:00pm for all days from Monday to Friday"
    types <- dataset
    
    if(is.null(dataset) || is.null(data) || is.null(idx)) {
      stop(paste("Unable to identify data for layerId:", layer_id))
    }
    
    # Return the UI elements to display in the #draggable2 floating window
    tagList(
      tags$h4("Information"),
      tags$p(paste("Address:", address)),
      tags$p(paste("Description:", description)),
      tags$p(paste("Business Hours:", business_hours)),
      tags$p(paste("Types:", types))
    )
  })
  
  
  

  
  #实现电车路线
  data_sf <- st_read("tram-tracks.geojson", quiet = TRUE)

  # 计算独特的线路数量
  num_unique_routes <- length(unique(data_sf$name))

  # 生成颜色映射
  colors <- colorFactor(rainbow(num_unique_routes), data_sf$name)

  tram_routes_visible <- reactiveVal(FALSE)  # Assuming tram routes are hidden by default

  # 为每条路线生成一个颜色
  colors <- colorFactor(rainbow(length(unique(data_sf$name))), data_sf$name)

  observeEvent(input$toggle_tram_routes, {
    tram_routes_visible(!tram_routes_visible())  # 切换值
    proxy <- leafletProxy("map")

    if (tram_routes_visible()) {
      # 对每一条线路循环，添加到地图上
      for(route in unique(data_sf$name)) {
        route_data <- data_sf[data_sf$name == route, ]
        proxy %>%
          addPolygons(data = route_data, color = colors(route), weight = 2, opacity = 1)
      }
    } else {
      proxy %>% clearShapes()
    }
  })
  
}


# -------------------------------- RUN SHINY --------------------------------- #
shinyApp(ui, server)
