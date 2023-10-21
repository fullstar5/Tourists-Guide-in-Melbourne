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
source('tableau-in-shiny-v1.0.R')

# ---------------------------- Variable Definition --------------------------- #
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

first_50_landmarks <- head(landmarks_data, 50)
first_50_bars <- head(bar_filtered_data, 50)
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
                    "This project ..."),
    accordion_panel("Data Source",
                    "Data Source Describe")
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
  height = "500px"
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
                  width: 50vh;
                  height: auto;  # 改为自动，以适应内容
                  cursor: move;
                  position: absolute;
                  top: 10vh;
                  right: 3vh;
                  background-color: rgba(255, 255, 255, 0.8);
                  color: black;
                  padding: 0.5vh;
                  }
                
                #draggable2 {
                  width: 50vh;
                  height: auto;  # 改为自动，以适应内容
                  cursor: move;
                  position: absolute;
                  top: 58vh;
                  right: 3vh;
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
                  margin-left: 15px;
                  align-items: center;
                }
              .custom-slider-labels {
                  color: black;
                  margin: 0 10px;
                  width: 70%;
               }"
    ),
    
    #tags$script('$(document).ready(function() {
    #               $( "#draggable" ).draggable();
    #             });'),
    #tags$script('$(document).ready(function() {
    #               $( "#draggable2" ).draggable();
    #             });'),
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
           map,
           tags$div(id = "draggable",
                    tags$h4("Title", 
                            tags$i(id="toggle-icon", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area",
                             tabsetPanel(
                               tabPanel("Map Setting",
                                        #actionButton("jump_to_melbourne", "Back to Melbourne Area",
                                        #             style = "margin-top: 1vh; background-color: #F9F200; color: black;"), br(),
                                        actionButton("show_coworkings", "Coworking",
                                                     style = "margin-top: 1vh; background-color: purple; color: white; width: 16vh;"),
                                        actionButton("show_hotels", "Hotel",
                                                     style = "margin-top: 1vh; background-color: #FFD740; color: #333333; width: 16vh;"),
                                        actionButton("show_bars", "Bar",
                                                     style = "margin-top: 1vh; background-color: red; color: white; width: 16vh;"), 
                                        actionButton("show_landmarks", "Landmark",
                                                     style = "margin-top: 1vh; background-color: #0163FA; color: white; width: 16vh;"), 
                                        actionButton("show_dwellings", "Dwelling",
                                                     style = "margin-top: 1vh;  background-color: #4CAF50; color: white; width: 16vh;"),
                                        actionButton("toggle_tram_routes", "Tram",
                                                     style = "margin-top: 1vh; background-color: green; color: white; width: 16vh;"),
                                        br(),
                                        fluidRow(
                                          column(4, selectInput("choose_coworking", "Find Coworkings", choices = c("All Company", unique(first_50_coworkings$Organisation))), class = "select"),
                                          column(4, selectInput("choose_hotels", "Find Hotels", choices = c("All Hotels", unique(first_500_hotels$Title))), class = "select"),
                                          column(4, selectInput("choose_bars", "Find Bars", choices = c("All Bars", unique(first_50_bars$Trading_name))), class = "select"),),
                                        fluidRow(
                                          column(8, selectInput("choose_landmark", "Find Landmarks", choices = c("All Landmarks", unique(first_50_landmarks$Title))), class = "select"),
                                          column(4, selectInput("choose_dwellings", "Dwelling type", choices = c("All Dwellings", unique(first_50_dwellings$Dwelling.type))), class = "select"),)
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
                                                       style = "color: black; background-color: #E8E8E8; margin-top: 10px;")),
                                          column(6, actionButton("dark_mode", " Dark Model", icon("moon"), 
                                                       style = "color: white; background-color: #212121; margin-top: 10px;")),
                                        )
                                        ,
                               ),
                             ),
                    )
           ),
           tags$div(id = "draggable2",
                    tags$h4("Title", 
                            tags$i(id="toggle-icon2", class="fas fa-chevron-down", style="float: right;")
                    ),
                    tags$div(id="content-area2",
                             tabPanel("More Information",  uiOutput("more_information_content"),
                                      style = "margin-left: 10px; margin-top: 10px; margin-bottom: 10px;"),
                    )
           ),
  ),
  
  header = setUpTableauInShiny(),
  tags$style(HTML("
    .custom-box {
      border: 2px solid #000;  /* 黑色边框 */
      background-color: #f9f9f9; /* 浅灰色背景 */
    }
    .custom-plot {
      border: 2px solid black;  /* 橙色边框 */
    }
    .tableau-plot{
      height: 80vh;
    }
  ")),
  tabPanel("Plot2",
           fluidRow(
             column(width = 4, box(title = "Card 1", "Content for card 1"), class = "custom-box"),
             column(width = 4, box(title = "Card 2", "Content for card 1"), class = "custom-box"),
             column(width = 4, box(title = "Card 3", "Content for card 1"), class = "custom-box"),
           ),
           fluidRow(class = "tableau-plot",
             column(width = 6, tableau1, class = "custom-plot"),
             column(width = 6, tableau2, class = "custom-plot"),
           )
  ),
  tabPanel("User Guide", userGuide),
)


# ------------------------------- SHINY SERVER ------------------------------- #
server <- function(input, output, session) {
  
  # Define Awesome Icons
  bar_icon <- makeAwesomeIcon(icon = 'glass', markerColor = 'red', iconColor = 'white')
  dwelling_icon <- makeAwesomeIcon(icon = 'home', markerColor = 'green', iconColor = 'white')
  coworking_icon <- makeAwesomeIcon(icon = 'briefcase', markerColor = "darkpurple", iconColor = 'white')
  landmark_icon <- makeAwesomeIcon(icon = 'map-marker', markerColor = 'blue', iconColor = 'white')
  hotel_icon <- makeAwesomeIcon(icon = 'bed', markerColor = 'orange', iconColor = 'white')
  
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_coworkings[i, "longitude"],
          lat = first_50_coworkings[i, "latitude"],
          icon = coworking_icon,
          popup = first_50_coworkings[i, "Address"],
          layerId = paste0("coworking_", i)
        )
      }
      coworking_visible(TRUE)
    }
  })
  
  observe({
    selected_coworking <- input$choose_coworking
    proxy <- leafletProxy("map")
    
    # Remove all markder first
    for (i in 1:nrow(first_50_coworkings)) {
      proxy <- removeMarker(proxy, layerId = paste0("coworking_", i))
    }
    
    if (selected_coworking == "All Company") {
      # Add all coworking marker
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_500_hotels[i, "longitude"],
          lat = first_500_hotels[i, "latitude"],
          icon = hotel_icon,
          popup = first_500_hotels[i, "Location"],
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_500_hotels[i, "longitude"],
          lat = first_500_hotels[i, "latitude"],
          icon = hotel_icon,
          popup = first_500_hotels[i, "Location"],
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_bars[i, "Longitude"],
          lat = first_50_bars[i, "Latitude"],
          icon = bar_icon,
          popup = first_50_bars[i, "Business_address"],
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_bars[i, "Longitude"],
          lat = first_50_bars[i, "Latitude"],
          icon = bar_icon,
          popup = first_50_bars[i, "Business_address"],
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_dwellings[i, "Longitude"],
          lat = first_50_dwellings[i, "Latitude"],
          icon = dwelling_icon,
          popup = first_50_dwellings[i, "Address"],
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
        proxy <- addAwesomeMarkers(
          proxy,
          lng = first_50_dwellings[i, "Longitude"],
          lat = first_50_dwellings[i, "Latitude"],
          icon = dwelling_icon,
          popup = first_50_dwellings[i, "Address"],
          layerId = paste0("dwelling_", i)
        )
      }
      dwellings_visible(TRUE)
    } else {
      selected_rows <- first_50_dwellings[first_50_dwellings$Dwelling.type == selected_dwelling, ]
      for (i in 1:nrow(selected_rows)) {
        proxy <- addAwesomeMarkers(
          proxy,
          lng = selected_rows[i, "Longitude"],
          lat = selected_rows[i, "Latitude"],
          icon = dwelling_icon,
          popup = selected_rows[i, "Address"],
          layerId = paste0("dwelling_", which(first_50_dwellings$Dwelling.type == selected_dwelling)[i])
        )
      }
      dwellings_visible(FALSE)
    }
  })
  
  
  
  ### landmarks -----------------------------------------------------------
  # display landmarks
  observe({
    selected_landmark <- input$choose_landmark
    proxy <- leafletProxy("map")
    
    # Remove all existing landmark markers first
    for (i in 1:nrow(first_50_landmarks)) {
      proxy <- removeMarker(proxy, layerId = paste0("landmark_", i))
    }
    
    # If "All Landmarks" is selected, show all markers
    if (selected_landmark == "All Landmarks") {
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
      # Otherwise, only show markers for the selected landmark
      filtered_landmarks <- first_50_landmarks[first_50_landmarks$Title == selected_landmark, ]
      for (i in 1:nrow(filtered_landmarks)) {
        proxy <- addAwesomeMarkers(
          proxy,
          lng = filtered_landmarks[i, "longitude"],
          lat = filtered_landmarks[i, "latitude"],
          icon = landmark_icon,
          popup = filtered_landmarks[i, "Title"],
          layerId = paste0("landmark_", i)
        )
      }
    }
  })
  
  # Toggle visibility for all landmarks using the action button
  landmarks_visible <- reactiveVal(FALSE)
  observeEvent(input$show_landmarks, {
    landmarks_visible(!landmarks_visible())
    
    proxy <- leafletProxy("map")
    if (landmarks_visible()) {
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
      for (i in 1:nrow(first_50_landmarks)) {
        proxy <- removeMarker(proxy, layerId = paste0("landmark_", i))
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
      setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
      addControl(html = "<button id='jumpButton'>Jump to Melbourne</button>", position = "bottomleft")
    
    
    # 创建用于显示 Business address 的响应性值
    address_info <- reactiveVal("")
    # 监听图标的点击事件
    observeEvent(input$map_marker_click, {
      event <- input$map_marker_click
      if (is.null(event)) {
        return()  # 如果没有点击事件，不执行任何操作
      }
      # 获取点击的图标的 ID
      marker_id <- event$id
      marker_type <- gsub("_[0-9]+$", "", marker_id)  # Extracting the marker type
      index <- as.numeric(gsub("^[a-z_]+_", "", marker_id))  # Extracting the index
      
      # 根据图标类型获取对应的地址
      if (marker_type == "bar") {
        if (!is.na(index) && index >= 1 && index <= nrow(bar_filtered_data)) {
          address <- bar_filtered_data[index, "Business_address"]
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
        }
      } else if (marker_type == "dwelling") {
        if (!is.na(index) && index >= 1 && index <= nrow(dwelling_data)) {
          address <- dwelling_data[index, "Address"]
        }
      }
      # Construct business hours information
      days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      hours <- "7:00 to 18:00"
      business_hours_info <- paste(days, hours, sep=": ", collapse="<br>")
      
      # Append business hours to the fetched address
      full_info <- paste("<strong>Location Type:</strong><br>", marker_type, "<br><br><strong>Address:</strong><br>", address, "<br><br><strong>Business Hours:</strong><br>", business_hours_info, sep="")
      # 更新响应性值，以便在 UI 中显示地址
      address_info(full_info)
    })
    # 在 "More Information" tabPanel 中显示地址
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
shinyApp(ui, server, options = list(launch.browser = TRUE))
