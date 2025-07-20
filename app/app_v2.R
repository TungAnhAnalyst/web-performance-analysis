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
library(scales)
library(arrow)
library(here)

# Load arrow dataset (lazy, no collect yet)
full_df <- open_dataset(here("data"), format = "parquet", partitioning = c("year", "month"))

ui <- dashboardPage(
  dashboardHeader(title = "Tableau de Bord Engagement Utilisateur"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global", tabName = "global", icon = icon("chart-line")),
      menuItem("Par Géographie", tabName = "geo", icon = icon("globe")),
      menuItem("Par Appareil", tabName = "device", icon = icon("mobile-alt")),
      menuItem("Nouveaux vs Retournants", tabName = "visitor", icon = icon("user-friends"))
    ),
    radioButtons("metric", "Choisir la métrique :",
                 choices = c("Sessions Totales" = "sessions",
                             "Pages Moyennes par Session" = "avg_pageviews",
                             "Taux de Rebond" = "avg_bounce"),
                 selected = "sessions")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "global",
              fluidRow(
                box(width = 12,
                    title = "Série Temporelle Globale pour la métrique choisie", status = "primary", solidHeader = TRUE,
                    plotlyOutput("time_plot")
                )
              )
      ),
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 12,
                    title = "Carte mondiale par métrique", status = "primary", solidHeader = TRUE,
                    plotlyOutput("map_plot")
                )
              )
      ),
      tabItem(tabName = "device",
              fluidRow(
                box(width = 12,
                    title = "Métrique par catégorie d’appareil", status = "primary", solidHeader = TRUE,
                    plotlyOutput("device_plot")
                )
              )
      ),
      tabItem(tabName = "visitor",
              fluidRow(
                box(width = 12,
                    title = "Métrique par type de visiteur", status = "primary", solidHeader = TRUE,
                    plotlyOutput("visitor_plot")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Prepare daily metrics (collect + mutate complex stuff in R)
  daily_metrics <- reactive({
    df_sum <- full_df %>%
      group_by(date) %>%
      summarise(
        sessions = n_distinct(visitId),
        total_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE),
        total_bounces = sum(as.numeric(total_bounces), na.rm = TRUE),
        total_newVisits = sum(ifelse(total_newVisits == "1", 1, 0), na.rm = TRUE) # just example, avoid if too complex
      ) %>%
      collect()
    
    df_sum %>%
      mutate(
        date = ymd(date),
        avg_pageviews = total_pageviews / sessions,
        avg_bounce = total_bounces / sessions
      )
  })
  
  # Prepare country-level summary
  country_metrics <- reactive({
    df_country <- full_df %>%
      group_by(geoNetwork_country) %>%
      summarise(
        sessions = n_distinct(visitId),
        total_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE),
        total_bounces = sum(as.numeric(total_bounces), na.rm = TRUE)
      ) %>%
      collect()
    
    df_country %>%
      mutate(
        avg_pageviews = total_pageviews / sessions,
        avg_bounce = total_bounces / sessions,
        iso3 = countrycode(geoNetwork_country, origin = "country.name", destination = "iso3c")
      ) %>%
      filter(!is.na(iso3))
  })
  
  # Prepare device-level summary
  device_metrics <- reactive({
    df_device <- full_df %>%
      group_by(device_deviceCategory) %>%
      summarise(
        sessions = n_distinct(visitId),
        total_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE),
        total_bounces = sum(as.numeric(total_bounces), na.rm = TRUE)
      ) %>%
      collect()
    
    df_device %>%
      mutate(
        avg_pageviews = total_pageviews / sessions,
        avg_bounce = total_bounces / sessions
      )
  })
  
  # Prepare visitor type summary (New vs Returning)
  visitor_metrics <- reactive({
    df_visitors <- full_df %>%
      collect() %>%  # collect first because recode not supported in Arrow
      mutate(
        total_newVisits = as.character(total_newVisits),
        visitorType = ifelse(is.na(total_newVisits), "Ancien",
                             ifelse(total_newVisits == "1", "Nouveau", "Ancien"))
      ) %>%
      group_by(visitorType) %>%
      summarise(
        sessions = n_distinct(visitId),
        avg_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE) / sessions,
        avg_bounce = sum(as.numeric(total_bounces), na.rm = TRUE) / sessions
      )
    df_visitors
  })
  
  # Outputs ------------------------------------------------------------------
  
  output$time_plot <- renderPlotly({
    df <- daily_metrics()
    p <- ggplot(df, aes(x = date, y = .data[[input$metric]])) +
      geom_line(color = "#1f77b4") +
      labs(
        title = paste("Evolution quotidienne des", switch(input$metric,
                                                          sessions = "Sessions Totales",
                                                          avg_pageviews = "Pages Moyennes par Session",
                                                          avg_bounce = "Taux de Rebond")),
        x = "Date",
        y = switch(input$metric,
                   sessions = "Sessions Totales",
                   avg_pageviews = "Pages Moyennes/Session",
                   avg_bounce = "Taux de Rebond")
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$map_plot <- renderPlotly({
    df <- country_metrics()
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    map_data <- left_join(world, df, by = c("iso_a3" = "iso3"))
    
    fill_vals <- map_data[[input$metric]]
    
    tooltip_text <- paste0(
      map_data$name_long, "\n",
      switch(input$metric,
             sessions = paste0("Sessions Totales : ", scales::comma(fill_vals)),
             avg_pageviews = paste0("Pages Moyennes par Session : ", round(fill_vals, 2)),
             avg_bounce = paste0("Taux de Rebond : ", scales::percent(fill_vals, accuracy = 0.1))
      )
    )
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = fill_vals, text = tooltip_text), color = "white") +
      scale_fill_viridis_c(
        option = "plasma",
        na.value = "grey90",
        labels = if(input$metric == "avg_bounce") {
          scales::percent_format(accuracy = 1)
        } else {
          scales::label_number(big.mark = ",")
        }
      ) +
      labs(
        title = paste("Carte des", switch(input$metric,
                                          sessions = "Sessions Totales",
                                          avg_pageviews = "Pages Moyennes par Session",
                                          avg_bounce = "Taux de Rebond"),
                      "par Pays"),
        fill = switch(input$metric,
                      sessions = "Sessions Totales",
                      avg_pageviews = "Pages Moyennes/Session",
                      avg_bounce = "Taux de Rebond")
      ) +
      theme_void()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$device_plot <- renderPlotly({
    df <- device_metrics()
    p <- ggplot(df, aes(x = device_deviceCategory, y = .data[[input$metric]], fill = device_deviceCategory)) +
      geom_col() +
      labs(title = paste(input$metric, "par Appareil"), x = "Appareil", y = input$metric) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$visitor_plot <- renderPlotly({
    df <- visitor_metrics()
    p <- ggplot(df, aes(x = visitorType, y = .data[[input$metric]], fill = visitorType)) +
      geom_col() +
      labs(title = paste(input$metric, "par Type de Visiteur"), x = "Type de Visiteur", y = input$metric) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
}

shinyApp(ui, server)
