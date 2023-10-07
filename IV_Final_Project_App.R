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
                    "Data Source Describe"),
  )
)

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
  ),
  
  dashboardBody(
    # Left and right layout (Above)
    fluidRow(
      # Left Page
      column(8, tabsetPanel(id = "left_tabs", 
                            map)),
      # Right Page
      column(4, tabsetPanel(id = "right_tabs",
                            tab1, tab2))
    ),
    
    # Below
    tabsetPanel(userGuide)
  )
)

# ------------------------------- SHINY SERVER ------------------------------- #
server <- function(input, output, session) { 
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
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # 可以选择其他的主题，有个黑白的很酷
    # Mini Map 
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      toggleDisplay = TRUE, position = "bottomleft") %>%
    # EasyButton: zoom = 1
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }")))
    # addMarkers()
  })
}

# -------------------------------- RUN SHINY --------------------------------- #
shinyApp(ui, server)