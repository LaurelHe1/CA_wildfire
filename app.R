library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(shiny)
library(sf)
library(tigris)
library(mapview)
library(leaflet)

fire = read.csv(file = "data/mapdataall.csv")
myvars = c('incident_name', 'incident_county', 'incident_acres_burned', 'incident_longitude', 'incident_latitude', 'incident_id', 'incident_date_extinguished', 'incident_date_created', 'calfire_incident')
fire_clean = fire[myvars]
fire_clean = fire_clean %>% drop_na() %>% filter(!grepl('1969', incident_date_created)) %>% filter(!grepl('2009', incident_date_created)) %>% filter(incident_latitude != 0 & incident_longitude != 0)

start_DT = ymd_hms(fire_clean$incident_date_created)
end_DT = ymd_hms(fire_clean$incident_date_extinguished)
duration = round(difftime(end_DT, start_DT, units = "hours"), 2)
new_fire = cbind(fire_clean, duration_hrs = as.numeric(duration), start_DT)
new_fire$year = year(start_DT)
new_fire = new_fire %>% drop_na() %>% filter(duration_hrs > 0 & incident_acres_burned > 0)

ca_counties <- counties("CA", cb = T, progress_bar = F)

# Define UI for application
ui <- fluidPage(

    # Application title
    headerPanel("California Wildfire 2013-2022"),

    fluidRow(
      column(7, 
             tabsetPanel(id = "ui_tab",
                tabPanel("Map",
                      column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(outputId ="map", height="600px"),size=2, color="#0080b7"))
             )
             )),
      column(5, 
            tabsetPanel(id = "plot_tabs",
                tabPanel("Time series",
                      fluidRow(column(4, 
                            selectizeInput(inputId = 'year',
                                           label = 'Wildfire Start Year',
                                           choices = sort(unique(new_fire$year)))
                      )),
                      fluidRow(column(12, 
                                      h5("Profile data"),div(DT::dataTableOutput("profile_table"), style = "font-size:80%")      
                                      ))
            ),
            tabPanel("Individual Profiles",
                     fluidRow(column(12,
                              radioButtons(inputId = "plot_type", "Plot type:", 
                                           choices = list("Count", "Major Incident", "Acres Burned", "County", "Duration"), inline=T),

                              conditionalPanel(condition="input.plot_type == 'Count'",
                                               selectInput(inputId = 'time1',
                                                              label = 'Time Resolution',
                                                              choices = c("Year", "Month", "Hour"))
                              ),
                              conditionalPanel(condition="input.plot_type == 'Major Incident'",
                                               selectInput(inputId = 'time2',
                                                              label = 'Time Resolution',
                                                              choices = c("Year", "Month", "Hour")),
                              ),
                              conditionalPanel(condition="input.plot_type == 'Acres Burned'",
                                               selectInput(inputId = 'time3',
                                                              label = 'Time Resolution',
                                                              choices = c("Year", "Month", "Hour")),
                              ),
                              conditionalPanel(condition="input.plot_type == 'Count'",
                                               plotOutput("count")
                              ),
                              conditionalPanel(condition="input.plot_type == 'Major Incident'",
                                               plotOutput("major_incident")
                              ),
                              conditionalPanel(condition="input.plot_type == 'Acres Burned'",
                                               plotOutput("acres_burned")
                              ),
                              conditionalPanel(condition="input.plot_type == 'County'",
                                               plotOutput("county")
                              ),
                              conditionalPanel(condition="input.plot_type == 'Duration'",
                                               plotOutput("duration")
                              )
                     ))
            )
            )  
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
          lat = ~incident_latitude, lng = ~incident_longitude, color = "red", radius = 4
        )
    })
  })  
  
  # Map marker click popup to show site info
  observe({
    click <- input$map_marker_click
    if (is.null(click))
      return()
    data = new_fire %>% filter(year == input$year & incident_latitude == click$lat & incident_longitude == click$lng)
    content <- as.character(tagList(
          sprintf("Fire Name: %s", data$incident_name), tags$br(),
          sprintf("Fire Duration: %s hours", as.integer(data$duration_hrs)), tags$br(),
          sprintf("Acres Burned: %s acres", data$incident_acres_burned), tags$br(),
          sprintf("Major Incident: %s", data$calfire_incident)
        ))
    leafletProxy(mapId = "map") %>% 
      addPopups(lat = click$lat, lng = click$lng, popup = content)
  })
  
  # Count plot output
  output$count = renderPlot({
    if (input$plot_type == "Count") {
      if (input$time1 == "Year") {
        new_fire %>% group_by(year) %>% summarise(count = n()) %>% 
          ggplot(aes(x=as.character(year), y=count)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Year") + ylab("Number of Wild Fires") 
      }else if (input$time1 == "Month") {
          new_fire %>% group_by(month = as.integer(month(start_DT))) %>% summarise(count=n()) %>%
          ggplot(aes(x=month, y=count)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Time of the Year (Month)") + ylab("Number of Wild Fires") + 
          scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
      }else if (input$time1 == "Hour") {
          new_fire %>% group_by(hour = hour(start_DT)) %>% summarise(count=n()) %>%
          ggplot(aes(x=hour, y=count)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Hour of the Day") + ylab("Number of Wild Fires") + 
          scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
      }
    }
  })
  
  # Major Incident plot output
  output$major_incident = renderPlot({
    if (input$plot_type == "Major Incident"){
      if (input$time2 == "Year") {
          new_fire %>% group_by(year) %>% filter(calfire_incident == "TRUE") %>%
          summarise(count = n()) %>% ggplot(aes(x=as.character(year), y=count)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Year") + ylab("Number of Major Incidents") 
      }else if (input$time2 == "Month") {
        new_fire %>% group_by(month = as.integer(month(start_DT))) %>% filter(calfire_incident == "TRUE") %>%
          summarise(count = n()) %>% ggplot(aes(x=month, y=count)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Time of the Year (Month)") + ylab("Number of Major Incidents") + 
          scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
      }else if (input$time2 == "Hour") {
        new_fire %>% group_by(hour = hour(start_DT)) %>% filter(calfire_incident == "TRUE") %>%
          summarise(count = n()) %>% ggplot(aes(x=hour, y=count)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Hour of the Day") + ylab("Number of Major Incidents") + 
          scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
       }
    } 
  })
  
  # Total Acres Burned plot output
  output$acres_burned = renderPlot({
    if (input$plot_type == "Acres Burned"){
      if (input$time3 == "Year") {
        new_fire %>% group_by(year) %>% summarise(totalAcres = sum(incident_acres_burned)) %>% 
          ggplot(aes(x=as.character(year), y=totalAcres)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Year") + ylab("Total Acres Burned") 
      }else if (input$time3 == "Month") {
        new_fire %>% group_by(month = as.integer(month(start_DT))) %>% summarise(totalAcres = sum(incident_acres_burned)) %>% 
          ggplot(aes(x=month, y=totalAcres)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Time of the Year (Month)") + ylab("Total Acres Burned") +
          scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
      }else if (input$time3 == "Hour") {
      new_fire %>% group_by(hour = hour(start_DT)) %>% summarise(totalAcres = sum(incident_acres_burned)) %>% 
          ggplot(aes(x=hour, y=totalAcres)) + geom_bar(stat = 'identity', fill = '#56B4E9') + 
          xlab("Hour of the Day") + ylab("Total Acres Burned") + 
          scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
      }
    }
  })
  # Counties plot output
  output$county = renderPlot({
    if (input$plot_type == "County"){
      fire_county = new_fire %>% group_by(incident_county) %>% summarise(totalAcres = sum(incident_acres_burned)) %>% arrange(desc(totalAcres))
      fire_county = fire_county[!(fire_county$incident_county==""), ] %>% head(40)
      fire_county %>% ggplot(aes(x=reorder(incident_county, totalAcres), y=totalAcres)) + geom_bar(stat = 'identity', fill = '#56B4E9') + coord_flip() + xlab("County") + ylab("Total Acres Burned")
    }
  })
  # Duration plot output
  output$duration = renderPlot({
    if (input$plot_type == "Duration"){
      new_fire %>% ggplot(aes(as.character(year), duration_hrs)) + geom_boxplot(color = "#56B4E9") + 
        xlab("Year") + ylab("Duration (hours)")
    }
  })
  # Data table output
  output$profile_table = DT::renderDataTable({
    DT::datatable(new_fire %>% filter(year == input$year), selection='multiple',
                  options = list(scrollY = '500px', paging = FALSE, scrollX = TRUE, searching=F)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
