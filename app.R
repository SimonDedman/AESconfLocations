library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidygeocoder)
library(ggrepel)

# Source helper functions
source("R/data_processing.R")
source("R/visualization.R")

# Load and process data
meetings <- read_and_process_meetings()
meetings_geocoded <- geocode_meetings(meetings)
meetings_jittered <- create_jittered_coordinates(meetings_geocoded)
state_counts <- create_state_visit_counts(meetings_jittered)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "ASIH Conference Locations (1916-2025)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Static Visualization", tabName = "static", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Summary Stats", tabName = "stats", icon = icon("bar-chart"))
    ),
    
    hr(),
    
    # Year range selector
    sliderInput("year_range", 
                "Select Year Range:",
                min = min(meetings_jittered$year),
                max = max(meetings_jittered$year),
                value = c(min(meetings_jittered$year), max(meetings_jittered$year)),
                step = 1,
                sep = ""),
    
    # Country filter
    checkboxGroupInput("countries",
                      "Select Countries:",
                      choices = unique(meetings_jittered$country),
                      selected = unique(meetings_jittered$country)),
    
    # Show connecting lines option
    checkboxInput("show_lines", "Show Chronological Lines", value = FALSE)
  ),
  
  dashboardBody(
    tabItems(
      # Interactive Map Tab
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "Interactive Conference Locations Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            leafletOutput("interactive_map", height = "550px")
          )
        ),
        fluidRow(
          box(
            title = "Map Controls",
            status = "info",
            width = 4,
            radioButtons("map_type", "Map Type:",
                        choices = list("Terrain" = "terrain",
                                     "Satellite" = "satellite", 
                                     "Street" = "street"),
                        selected = "terrain")
          ),
          box(
            title = "Legend",
            status = "info", 
            width = 8,
            p("• Point colors represent the year of the meeting (darker = older, brighter = newer)"),
            p("• Point sizes may indicate multiple meetings in the same location"),
            p("• Click on points for meeting details"),
            p("• International locations are shown at their actual coordinates")
          )
        )
      ),
      
      # Static Visualization Tab  
      tabItem(tabName = "static",
        fluidRow(
          box(
            title = "Static Conference Map Visualization",
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = "700px",
            plotOutput("static_plot", height = "650px")
          )
        )
      ),
      
      # Data Table Tab
      tabItem(tabName = "table",
        fluidRow(
          box(
            title = "Conference Meetings Data",
            status = "primary",
            solidHeader = TRUE, 
            width = 12,
            DTOutput("meetings_table")
          )
        )
      ),
      
      # Summary Stats Tab
      tabItem(tabName = "stats",
        fluidRow(
          valueBoxOutput("total_meetings"),
          valueBoxOutput("years_span"),
          valueBoxOutput("states_visited")
        ),
        fluidRow(
          box(
            title = "Most Visited Cities",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("city_counts_table")
          ),
          box(
            title = "Meetings by Decade",
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            plotOutput("decade_plot")
          )
        ),
        fluidRow(
          box(
            title = "International vs Domestic Meetings",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("country_pie")
          ),
          box(
            title = "Timeline of Meetings",
            status = "primary",
            solidHeader = TRUE, 
            width = 6,
            plotOutput("timeline_plot")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_meetings <- reactive({
    meetings_jittered %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      filter(country %in% input$countries)
  })
  
  # Interactive leaflet map
  output$interactive_map <- renderLeaflet({
    data <- filtered_meetings()
    
    # Create color palette for years
    pal <- colorNumeric(palette = viridis::viridis(100), domain = data$year)
    
    # Base map
    m <- leaflet(data) %>%
      setView(lng = -95, lat = 40, zoom = 4)
    
    # Add appropriate tiles
    if(input$map_type == "terrain") {
      m <- m %>% addProviderTiles(providers$Stamen.Terrain)
    } else if(input$map_type == "satellite") {
      m <- m %>% addProviderTiles(providers$Esri.WorldImagery)  
    } else {
      m <- m %>% addTiles()
    }
    
    # Add meeting points
    m <- m %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = ~pal(year),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0("<b>", year, "</b><br>",
                       location, "<br>",
                       "Country: ", country)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~year,
        title = "Meeting Year",
        opacity = 0.8
      )
    
    return(m)
  })
  
  # Static plot
  output$static_plot <- renderPlot({
    data <- filtered_meetings()
    state_counts_filtered <- create_state_visit_counts(data)
    
    plot <- create_conference_map(data, state_counts_filtered)
    
    if(input$show_lines && nrow(data) > 1) {
      plot <- add_chronological_lines(plot, data)
    }
    
    return(plot)
  })
  
  # Data table
  output$meetings_table <- renderDT({
    filtered_meetings() %>%
      select(year, location, city, state_province, country) %>%
      arrange(desc(year))
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Value boxes
  output$total_meetings <- renderValueBox({
    valueBox(
      value = nrow(filtered_meetings()),
      subtitle = "Total Meetings",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$years_span <- renderValueBox({
    data <- filtered_meetings()
    span <- max(data$year) - min(data$year) + 1
    valueBox(
      value = span,
      subtitle = "Years Covered", 
      icon = icon("clock"),
      color = "green"
    )
  })
  
  output$states_visited <- renderValueBox({
    states <- filtered_meetings() %>%
      filter(country == "USA") %>%
      distinct(state_province) %>%
      nrow()
    
    valueBox(
      value = states,
      subtitle = "US States Visited",
      icon = icon("map-marker"),
      color = "orange"
    )
  })
  
  # City counts table
  output$city_counts_table <- renderDT({
    filtered_meetings() %>%
      count(location, name = "Meetings") %>%
      arrange(desc(Meetings)) %>%
      head(15)
  }, options = list(pageLength = 15, dom = 't'))
  
  # Decade plot
  output$decade_plot <- renderPlot({
    filtered_meetings() %>%
      mutate(decade = floor(year / 10) * 10) %>%
      count(decade) %>%
      ggplot(aes(x = decade, y = n)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      labs(x = "Decade", y = "Number of Meetings",
           title = "Meetings by Decade") +
      theme_minimal()
  })
  
  # Country pie chart
  output$country_pie <- renderPlot({
    filtered_meetings() %>%
      count(country) %>%
      mutate(
        country = factor(country),
        percentage = n / sum(n) * 100
      ) %>%
      ggplot(aes(x = "", y = n, fill = country)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Meetings by Country", fill = "Country") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlot({
    filtered_meetings() %>%
      ggplot(aes(x = year, y = 1)) +
      geom_point(aes(color = country), size = 3, alpha = 0.7) +
      scale_color_viridis_d() +
      labs(x = "Year", y = "", title = "Meeting Timeline",
           color = "Country") +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)