# Load necessary libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stringr)
library(readr)
library(maps)







# Load map data
map_data <- ne_countries(scale = "medium", returnclass = "sf") 
saveRDS(map_data, "ne_countries.rds") 

map_data <- readRDS("ne_countries.rds") 
aruba_boundary <- map_data %>%  # 、
  filter(admin == "Aruba")

# Load air quality data
air_quality_data <- read_csv("surfside_PM1-0_2022-08-01_2024-10-08.csv", show_col_types = FALSE)
air_quality_data$timestamp <- ymd_hms(air_quality_data$timestamp, tz = "UTC")
air_quality_data <- air_quality_data %>% mutate(aruba_time = with_tz(timestamp, tzone = "America/Aruba"))

sensor1_data <- air_quality_data %>% filter(str_detect(sensor, "PMS 1")) %>%
  arrange(aruba_time) %>%
  mutate(moving_avg = rollmean(value, 24, fill = NA, align = "right"))

sensor2_data <- air_quality_data %>% filter(str_detect(sensor, "PMS 2")) %>%
  arrange(aruba_time) %>%
  mutate(moving_avg = rollmean(value, 24, fill = NA, align = "right"))

# Load GDP and population data
gdp_data <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_2.csv", skip = 4, show_col_types = FALSE)
population_data <- read_csv("API_SP.POP.TOTL_DS2_en_csv_v2_56.csv", skip = 4, show_col_types = FALSE)

# Filter Aruba and neighboring countries
target_countries <- c("Antigua and Barbuda", "Barbados", "Dominica", "Grenada")  # Replace with new countries

island_gdp <- gdp_data %>%
  filter(`Country Name` %in% c("Aruba", target_countries)) %>%  # Include Aruba for comparison
  gather(key = "Year", value = "GDP", starts_with("19"), starts_with("20")) %>%
  mutate(Year = as.numeric(Year)) %>%
  replace_na(list(GDP = NA))

island_population <- population_data %>%
  filter(`Country Name` %in% c("Aruba", target_countries)) %>%  # Include Aruba for comparison
  gather(key = "Year", value = "Population", starts_with("19"), starts_with("20")) %>%
  mutate(Year = as.numeric(Year)) %>%
  replace_na(list(Population = NA))

# Extract Aruba GDP data
aruba_gdp <- island_gdp %>%
  filter(`Country Name` == "Aruba") %>%
  select(Year, GDP)

# Extract Aruba Population data
aruba_population <- island_population %>%
  filter(`Country Name` == "Aruba") %>%
  select(Year, Population)

# Determine year ranges
min_year <- ifelse(is.finite(min(island_gdp$Year, na.rm = TRUE)), 
                   min(island_gdp$Year, na.rm = TRUE), 2000) # Default to 2000
max_year <- ifelse(is.finite(max(island_gdp$Year, na.rm = TRUE)), 
                   max(island_gdp$Year, na.rm = TRUE), 2020) # Default to 2020

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Aruba Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Location", tabName = "location", icon = icon("map")),
      menuItem("Aruba Island", tabName = "island", icon = icon("globe")),
      menuItem("Air Quality", icon = icon("wind"), startExpanded = TRUE,
               menuSubItem("Air Quality Image", tabName = "airquality_image", icon = icon("image")),
               menuSubItem("PM1.0 Trends", tabName = "airquality_trends", icon = icon("chart-line"))
      ),
      menuItem("Data Trends", icon = icon("chart-line"), startExpanded = TRUE,
               menuSubItem("GDP Trends", tabName = "gdp_trends", icon = icon("chart-bar")),
               menuSubItem("Population Trends", tabName = "population_trends", icon = icon("users"))
      ),
      menuItem("Country Comparison", tabName = "country_comparison", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(
        tabName = "introduction",
        fluidRow(
          box(
            title = "Welcome to Aruba",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            img(src = "WechatIMG1254.jpg", width = "100%", height = "auto") # Ensure the image is in the www folder
          )
        )
      ),
      
      # Location Tab
      tabItem(
        tabName = "location",
        fluidRow(
          box(
            title = "World Map",
            width = 12,
            plotOutput("world_map")
          )
        )
      ),
      
      # Aruba Island Tab
      tabItem(
        tabName = "island",
        fluidRow(
          box(
            title = "Aruba Island Map",
            width = 12,
            leafletOutput("aruba_map", height = "800px")
          )
        )
      ),
      # Air Quality Image Tab
      tabItem(
        tabName = "airquality_image",
        fluidRow(
          box(
            title = "Air Quality Overview",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            img(src = "WechatIMG1255.jpg", width = "100%", height = "auto") # 显示空气质量相关图片
          )
        )
      ),
      
      # Air Quality Trends Tab
      tabItem(
        tabName = "airquality_trends",
        fluidRow(
          box(
            title = "PM1.0 Trends",
            width = 12,
            dateInput(
              inputId = "date_trend", 
              label = "Select Date:", 
              value = Sys.Date(), 
              min = "2022-08-01",   
              max = "2024-10-08"    
            ),
            plotOutput("trend")
          )
        )
      ),
      
      # GDP Trends Tab
      tabItem(
        tabName = "gdp_trends",
        fluidRow(
          box(
            title = "GDP Trends",
            width = 12,
            sliderInput(
              inputId = "gdp_year_range",
              label = "Select Year Range:",
              min = min_year,
              max = max_year,
              value = c(min_year, max_year),
              step = 1,
              sep = ""
            ),
            plotOutput("gdp_trend")
          )
        )
      ),
      
      # Population Trends Tab
      tabItem(
        tabName = "population_trends",
        fluidRow(
          box(
            title = "Population Trends",
            width = 12,
            sliderInput(
              inputId = "population_year_range",
              label = "Select Year Range:",
              min = min_year,
              max = max_year,
              value = c(min_year, max_year),
              step = 1,
              sep = ""
            ),
            plotOutput("population_trend")
          )
        )
      ),
      
      # Country Comparison Tab
      tabItem(
        tabName = "country_comparison",
        fluidRow(
          box(
            title = "Select Options",
            solidHeader = TRUE,
            selectInput("selected_country", "Select Country:", 
                        choices = target_countries,  # Updated countries
                        selected = "Antigua and Barbuda"),
            sliderInput("comparison_year_range", "Select Year Range:", 
                        min = min_year, max = max_year, 
                        value = c(min_year, max_year), step = 1)
          )
        ),
        fluidRow(
          box(
            title = "GDP Comparison",
            width = 12,
            plotOutput("gdp_comparison_plot")
          ),
          box(
            title = "Population Comparison",
            width = 12,
            plotOutput("population_comparison_plot")
          )
        )
      )
    )
  )
)

# Server


server <- function(input, output, session) {
  
  output$gdp_trend <- renderPlot({
    filtered_gdp <- aruba_gdp %>%
      filter(Year >= input$gdp_year_range[1], Year <= input$gdp_year_range[2])
    
    ggplot(filtered_gdp, aes(x = Year, y = GDP)) +
      geom_line(color = "blue", linewidth = 1.2, na.rm= TRUE) +
      theme_minimal() +
      labs(
        title = "Aruba GDP Trends",
        x = "Year",
        y = "GDP (US Dollars)"
      )
  })
  
  output$population_trend <- renderPlot({
    filtered_population <- aruba_population %>%
      filter(Year >= input$population_year_range[1], Year <= input$population_year_range[2])
    
    ggplot(filtered_population, aes(x = Year, y = Population)) +
      geom_line(color = "green", linewidth = 1.2) +
      theme_minimal() +
      labs(
        title = "Aruba Population Trends",
        x = "Year",
        y = "Population"
      )
  })
  
  output$world_map <- renderPlot({
    world_map <- map_data("world")
    ggplot(world_map, aes(long, lat, group = group)) +
      geom_polygon(fill = "gray90", color = "white") +
      geom_point(aes(x = -69.9787, y = 12.5167), color = "red", size = 3) +
      coord_quickmap() +
      labs(title = "Aruba on World Map", x = "Longitude", y = "Latitude")
  })
  
  output$aruba_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = aruba_boundary,
        color = "transparent",
        fill = FALSE,
        label = "Aruba"
      )
  })
  
  output$trend <- renderPlot({
    selected_date <- as.Date(input$date_trend)
    
    # 确保 sensor1_data 和 sensor2_data 中有符合条件的数据
    sensor1_day <- sensor1_data %>% filter(as.Date(aruba_time) == selected_date)
    sensor2_day <- sensor2_data %>% filter(as.Date(aruba_time) == selected_date)
    
    validate(
      need(nrow(sensor1_day) > 0 || nrow(sensor2_day) > 0, 
           paste("No data available for", selected_date))
    )
    
    ggplot() +
      geom_line(data = sensor1_day, aes(x = aruba_time, y = moving_avg, color = "Sensor 1"), size = 1) +
      geom_line(data = sensor2_day, aes(x = aruba_time, y = moving_avg, color = "Sensor 2"), size = 1) +
      scale_color_manual(
        name = "Sensor",
        values = c("Sensor 1" = "blue", "Sensor 2" = "green")
      ) +
      labs(
        title = paste("PM1.0 Daily Moving Average (24-hour Period) -", selected_date),
        x = "Time",
        y = "PM1.0 Value (\u00b5g/m3)"
      ) +
      theme_minimal() +
      scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")
  })
  
    
  # World Map
  output$world_map <- renderPlot({
    world_map <- map_data("world")
    ggplot(world_map, aes(long, lat, group = group)) +
      geom_polygon(fill = "gray90", color = "white") +
      geom_point(aes(x = -69.9787, y = 12.5167), color = "red", size = 3) +
      coord_quickmap() +
      labs(title = "Aruba on World Map", x = "Longitude", y = "Latitude")
  })
  
  # Aruba Map
  output$aruba_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = aruba_boundary,
        color = "transparent",
        fill = FALSE,
        label = "Aruba"
      )
  })
  
  # GDP Comparison Plot
  output$gdp_comparison_plot <- renderPlot({
    filtered_gdp <- island_gdp %>%
      filter(`Country Name` %in% c("Aruba", input$selected_country),
             Year >= input$comparison_year_range[1],
             Year <= input$comparison_year_range[2])
    
    validate(
      need(nrow(filtered_gdp) > 0, "Data for the selected year range is missing.")
    )
    
    ggplot(filtered_gdp, aes(x = Year, y = GDP, color = `Country Name`)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      theme_minimal() +
      labs(
        title = paste("GDP Comparison: Aruba vs", input$selected_country),
        x = "Year",
        y = "GDP (US Dollars)",
        color = "Country"
      )
  })
  
  # Population Comparison Plot
  output$population_comparison_plot <- renderPlot({
    filtered_population <- island_population %>%
      filter(`Country Name` %in% c("Aruba", input$selected_country),
             Year >= input$comparison_year_range[1],
             Year <= input$comparison_year_range[2])
    
    validate(
      need(nrow(filtered_population) > 0, "Data for the selected year range is missing.")
    )
    
    ggplot(filtered_population, aes(x = Year, y = Population, color = `Country Name`)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      theme_minimal() +
      labs(
        title = paste("Population Comparison: Aruba vs", input$selected_country),
        x = "Year",
        y = "Population",
        color = "Country"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

