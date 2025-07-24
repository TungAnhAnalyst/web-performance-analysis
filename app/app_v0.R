library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(arrow)

# === Load and preprocess data ===
full_df <- open_dataset(here("data"), format = "parquet", partitioning = c("year", "month"))
df <- full_df %>% collect()

# Clean and fix types
df$total_pageviews <- as.numeric(as.character(df$total_pageviews))
df$total_timeOnsite <- as.numeric(as.character(df$total_timeOnsite))
df$total_bounces <- as.character(df$total_bounces)
df$total_bounces <- ifelse(is.na(df$total_bounces), "0", df$total_bounces) 
df$total_bounces <- ifelse(df$total_bounces == "1", "1", "0")
df$total_bounces <- as.numeric(df$total_bounces)

df$total_newVisits <- as.character(df$total_newVisits)

df$type_visiteur <- recode(df$total_newVisits,
                           "1" = "Nouveau",
                           .missing = "Ancienne")

df <- df %>%
  mutate(
    date = ymd(date),
    visitorType = as.factor(type_visiteur),
    device_deviceCategory = as.factor(device_deviceCategory),
    country = geoNetwork_country,
    continent = countrycode(country, origin = "country.name", destination = "continent"),
    visitId = as.character(visitId)
  )

# === Metrics calculation for time plot ===
daily_metrics <- df %>%
  group_by(date) %>%
  summarise(
    sessions = n_distinct(visitId),
    avg_pageviews = sum(total_pageviews, na.rm = TRUE) / n_distinct(visitId),
    avg_bounce = sum(total_bounces, na.rm = TRUE) / n_distinct(visitId)
  ) %>%
  ungroup()

# === UI ===
ui <- dashboardPage(
  dashboardHeader(title = "User Engagement Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("By Geography", tabName = "geo", icon = icon("globe")),
      menuItem("By Device", tabName = "device", icon = icon("mobile-alt")),
      menuItem("New vs Returning", tabName = "visitor", icon = icon("user-friends"))
    ),
    radioButtons("metric", "Select Metric:",
                 choices = c("Total Sessions" = "sessions",
                             "Avg Pageviews/Session" = "avg_pageviews",
                             "Bounce Rate" = "avg_bounce"),
                 selected = "sessions")
  ),
  dashboardBody(
    tabItems(
      # GEO TAB
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 12,
                    title = "Metric Over Time", status = "primary", solidHeader = TRUE,
                    plotlyOutput("time_plot")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "World Map by Bounce Rate", status = "primary", solidHeader = TRUE,
                    plotlyOutput("map_plot")
                )
              )
      ),
      
      # DEVICE TAB
      tabItem(tabName = "device",
              fluidRow(
                box(width = 12,
                    title = "Metric by Device Category", status = "primary", solidHeader = TRUE,
                    plotlyOutput("device_plot")
                )
              )
      ),
      
      # VISITOR TAB
      tabItem(tabName = "visitor",
              fluidRow(
                box(width = 12,
                    title = "Metric by Visitor Type", status = "primary", solidHeader = TRUE,
                    plotlyOutput("visitor_plot")
                )
              )
      )
    )
  )
)

# === SERVER ===
server <- function(input, output) {
  
  output$time_plot <- renderPlotly({
    p <- ggplot(daily_metrics, aes(x = date, y = .data[[input$metric]])) +
      geom_line(color = "#1f77b4") +
      labs(title = paste("Daily", input$metric), x = "Date", y = input$metric) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$map_plot <- renderPlotly({
    bounce_by_country <- df %>%
      group_by(country) %>%
      summarise(
        bounces = sum(total_bounces, na.rm = TRUE),
        visits = n_distinct(visitId)
      ) %>%
      mutate(
        bounce_rate = bounces / visits,
        iso3 = countrycode(country, origin = "country.name", destination = "iso3c")
      ) %>%
      filter(!is.na(iso3))
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    map_data <- world %>%
      left_join(bounce_by_country, by = c("iso_a3" = "iso3")) %>%
      mutate(
        bounce_rate_clipped = pmin(pmax(bounce_rate, 0.2), 0.8),
        tooltip = paste0(name_long, "\nBounce Rate: ",
                         ifelse(is.na(bounce_rate), "N/A", scales::percent(bounce_rate, accuracy = 0.1)))
      )
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = bounce_rate_clipped, text = tooltip), color = "white") +
      scale_fill_gradientn(
        colours = c("#1a9641", "#ffffbf", "#d7191c"),
        limits = c(0.2, 0.8),
        labels = scales::percent_format(accuracy = 1),
        na.value = "grey90"
      ) +
      labs(title = "Bounce Rate by Country", fill = "Bounce Rate") +
      theme_void()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$device_plot <- renderPlotly({
    data <- df %>%
      group_by(deviceCategory) %>%
      summarise(
        sessions = n_distinct(visitId),
        avg_pageviews = sum(total_pageviews, na.rm = TRUE) / n_distinct(visitId),
        avg_bounce = sum(total_bounces, na.rm = TRUE) / n_distinct(visitId)
      )
    
    p <- ggplot(data, aes(x = deviceCategory, y = .data[[input$metric]], fill = deviceCategory)) +
      geom_col() +
      labs(title = paste(input$metric, "by Device"), x = "Device", y = input$metric) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$visitor_plot <- renderPlotly({
    data <- df %>%
      group_by(visitorType) %>%
      summarise(
        sessions = n_distinct(visitId),
        avg_pageviews = sum(total_pageviews, na.rm = TRUE) / n_distinct(visitId),
        avg_bounce = sum(total_bounces, na.rm = TRUE) / n_distinct(visitId)
      )
    
    p <- ggplot(data, aes(x = visitorType, y = .data[[input$metric]], fill = visitorType)) +
      geom_col() +
      labs(title = paste(input$metric, "by Visitor Type"), x = "Visitor Type", y = input$metric) +
      theme_minimal()
    
    ggplotly(p)
  })
}

# === Run App ===
shinyApp(ui, server)
