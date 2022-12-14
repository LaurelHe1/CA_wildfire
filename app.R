library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(shiny)
library(sf)
library(tigris)
library(mapview)
library(leaflet)

fire = read.csv(file = "data/California_Fire_Incidents.csv")
myvars = c('AcresBurned', 'Counties', 'Extinguished', 'Latitude', 'Longitude', 'MajorIncident', 'Started', 'Name')
fire_clean = fire[myvars]
fire_clean = fire_clean %>% drop_na() %>% filter(!grepl('1969', Started)) %>% filter(Latitude != 0 & Longitude != 0)
start_DT = ymd_hms(fire_clean$Started)
end_DT = ymd_hms(fire_clean$Extinguished)
duration = difftime(end_DT, start_DT, units = "hours")
new_fire = cbind(fire_clean, duration_hrs = as.numeric(duration), start_DT)
new_fire$year = year(start_DT)
new_fire = new_fire %>% drop_na() %>% filter(duration_hrs > 0 & AcresBurned > 0)
ca_counties <- counties("CA", cb = T, progress_bar = F)

# Define UI for application
ui <- fluidPage(

    # Application title
    headerPanel("California Wildfire 2013-2019"),

    fluidRow(
      column(7, 
             tabsetPanel(id = "ui_tab",
                tabPanel("Map",
                      column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(outputId ="map", height="600px"),size=2, color="#0080b7"))
             ),
                tabPanel("Table",
                      column(12, div(DT::dataTableOutput("table_input"), style = "font-size:70%"))
             )
             )),
      column(5, 
            tabsetPanel(id = "plot_tabs",
                tabPanel("Time series",
                      fluidRow(column(8,
                            uiOutput("date_select"),
                            selectizeInput(inputId = 'year',
                                           label = 'Wildfire Start Year',
                                           choices = unique(new_fire$year))
                ))
            ))  
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
  # Create map
  map = createLeafletMap(session, 'map')
  session$onFlushed(once=T, function(){
    output$map = renderLeaflet({
      leaflet(new_fire %>% filter(year == input$year)) %>%
        addTiles() %>%
        addPolygons(
          data = ca_counties
        ) %>%
        addCircleMarkers(
          lat = ~Latitude, lng = ~Longitude, color = "red", radius = 4
        )
    })
  })  
  
  # Map marker click popup to show site info
  observe({
    click <- input$map_marker_click
    if (is.null(click))
      return()
    data = new_fire %>% filter(year == input$year & Latitude == click$lat & Longitude == click$lng)
    content <- as.character(tagList(
          sprintf("Fire Name: %s", data$Name), tags$br(),
          sprintf("Fire Duration: %s hours", as.integer(data$duration_hrs)), tags$br(),
          sprintf("Acres Burned: %s acres", data$AcresBurned), tags$br(),
          sprintf("Major Incident: %s", data$MajorIncident)
        ))
    leafletProxy(mapId = "map") %>% 
      addPopups(lat = click$lat, lng = click$lng, popup = content)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
