# ----------------------------- Introduction ----------------------------- #
# Group ID:
# Group Member: (Name - Student ID - Email?)
#   Student 1:
#   Student 2:
#   Student 3:
#   Student 4:
# Subject: Information Visualisation (GEOM90007_2023_SM2)
# Project introduction: Assignment3

# ------------------------------ Import Library ------------------------------ #
library("shiny")
library(shinydashboard)
library(bslib)      # Bootstrap Theme is used, in order to make the page more aesthetically pleasing
library(leaflet)    # Map
library(dplyr)
library(leaflet.extras)
library(httr)
library(jsonlite)

# ---------------------------- Variable Definition --------------------------- #
## Dataset

## Page layout Settings
# Theme
# Bootstrap Theme and dark mode (https://rstudio.github.io/bslib/articles/theming/index.html?q=dark%20mode#dynamic)
# Other theme: "sketchy", “cosmo”. Demo: https://testing-apps.shinyapps.io/themer-demo/
theme <- "sketchy"
light <- bs_theme(bootswatch = theme)
dark <- bs_theme(bootswatch = theme, bg = "#212121", fg = "white")

# Add borders to tabs -> visualize actions to make the page look better
tab <- function(...) {
  shiny::tabPanel(..., class = "p-3 border border-top-0 rounded-bottom")
}


# ------------------------------ Weather Extractor ------------------------------ #
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
map <- tab(
  "Map",
  # To adjust the style to make the map fill the entire tab
  tags$style(HTML("{width: 100%;}")),
  leafletOutput("map")
)

# Right Page
tab1 <- tab(tabName = "tab1",
            "Tab1",
            "Content for Tab 1")

tab2 <- tab(tabName = "tab2",
            "Tab 2",
            "Content for Tab 2")

# Bottom
userGuide <- tab(
  "User Guide",
  # Accordion: https://rstudio.github.io/bslib/articles/sidebars/index.html#accordions
  accordion(
    accordion_panel("Introduction",
                    "This project ..."),
    accordion_panel("Data Source",
                    "Data Source Describe")
  )
)

# -------------------------------- Bar display --------------------------------- #

# 读取CSV数据集
bar_data <- read.csv("bars-and-pubs-with-patron-capacity.csv")

# 过滤数据集
bar_filtered_data <- bar_data %>%
  filter(!is.na(Business_address) &          # Business address 不为空
           !is.na(Longitude) &                   # Longitude 不为空
           !is.na(Latitude) &                    # Latitude 不为空
           Census_year > 2012 &               # Census year 大于 2022
           Census_year < 2023 &               # Census year 小于 2023
           grepl("bar", Trading_name, ignore.case = TRUE))  # Trading name 包含 "bar"（不区分大小写）

# -------------------------------- UI --------------------------------- #

## UI
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "sketchy"),
  title = "IV Assignment3",
  
  # slidebr set
  sidebar = sidebar(
    title = h4("Filter Or Other setting"),
    
    h4("Page Setting"),
    actionButton("light_mode", " Light Model", icon("sun"), style = "color: black; background-color: #E8E8E8;"),
    actionButton("dark_mode", " Dark Model", icon("moon"), style = "color: white; background-color: #212121;"),
    
    # New Button
    br(), # Adding a line break for better spacing
    actionButton("jump_to_melbourne", "Jump to Melbourne"),
  ),
  
  dashboardBody(
    # Left and right layout (Above)
    fluidRow(
      # Left Page
      column(8, tabsetPanel(id = "left_tabs", 
                            map)),
      # Right Page
      column(4, tabsetPanel(id = "right_tabs",
                            tab1, tab2)),
      # -------------------------------- Bar display --------------------------------- #
      # Checkbox to control bar icon visibility
      checkboxInput("show_bar_icons", "Show Bar Icons", value = TRUE)  
    ),
    
    
    # Below
    tabsetPanel(userGuide)
  )
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
  
  ## Dynamic theming
  #  https://rstudio.github.io/bslib/articles/theming/index.html?q=dark%20mode#dynamic
  observeEvent(input$light_mode, {
    session$setCurrentTheme(light)
  })
  observeEvent(input$dark_mode, {
    session$setCurrentTheme(dark)
  })
  
  ## Map
  output$map <- renderLeaflet({
    # Create Leaflet Map
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # 可以选择其他的主题，有个黑白的很酷
      # Mini Map
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        toggleDisplay = TRUE, position = "bottomleft") %>%
      # 将地图聚焦在墨尔本区域
      setView(lng = 144.9631, lat = -37.8136, zoom = 10)
    
    # -------------------------------- Bar display --------------------------------- #
    if (input$show_bar_icons) {
      # Add bar icons to the map if the checkbox is checked
      for (i in 1:nrow(bar_filtered_data)) {
        bar_icon <- makeIcon(iconUrl = "bar_icon.png", iconWidth = 30, iconHeight = 30)
        m <- addMarkers(
          m,
          lng = bar_filtered_data[i, "Longitude"],
          lat = bar_filtered_data[i, "Latitude"],
          icon = bar_icon,
          popup = bar_filtered_data[i, "Business_address"]
        )
      }
    }
    
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
      setView(lng=lon, lat=lat, zoom=10)  # Update view to Melbourne
  })
  
  
}


# -------------------------------- RUN SHINY --------------------------------- #
shinyApp(ui, server)