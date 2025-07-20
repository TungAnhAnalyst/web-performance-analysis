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

# Chargement dataset Arrow (lazy, pas encore collect)
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
  
  # Agrégation journalière — tout en Arrow, puis collect final
  daily_metrics <- reactive({
    full_df %>%
      select(date, visitId, total_pageviews, total_bounces) %>%
      group_by(date) %>%
      summarise(
        sessions = n_distinct(visitId),
        total_pageviews = sum(as.integer(total_pageviews), na.rm = TRUE),
        total_bounces = sum(as.integer(total_bounces), na.rm = TRUE)
      ) %>%
      collect() %>%
      mutate(
        date = ymd(date),
        avg_pageviews = total_pageviews / sessions,
        avg_bounce = total_bounces / sessions
      )
  })
  
  # Agrégation par pays — Arrow + collect final, conversion iso3 dans R
  country_metrics <- reactive({
    df_country <- full_df %>%
      select(geoNetwork_country, visitId, total_pageviews, total_bounces) %>%
      group_by(geoNetwork_country) %>%
      summarise(
        sessions = n_distinct(visitId),
        total_pageviews = sum(as.integer(total_pageviews), na.rm = TRUE),
        total_bounces = sum(as.integer(total_bounces), na.rm = TRUE)
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
  
  # Agrégation par device — Arrow + collect final
  device_metrics <- reactive({
    full_df %>%
      select(device_deviceCategory, visitId, total_pageviews, total_bounces) %>%
      group_by(device_deviceCategory) %>%
      summarise(
        sessions = n_distinct(visitId),
        total_pageviews = sum(as.integer(total_pageviews), na.rm = TRUE),
        total_bounces = sum(as.integer(total_bounces), na.rm = TRUE)
      ) %>%
      collect() %>%
      mutate(
        avg_pageviews = total_pageviews / sessions,
        avg_bounce = total_bounces / sessions
      )
  })
  
  # Agrégation visiteurs (Nouveau vs Ancien) — nécessite collect avant mutation car condition complexe
  visitor_metrics <- reactive({
    full_df %>%
      select(total_newVisits, visitId, total_pageviews, total_bounces) %>%
      collect() %>%
      mutate(
        visitorType = ifelse(is.na(total_newVisits), "Ancien",
                             ifelse(total_newVisits == "1", "Nouveau", "Ancien"))
      ) %>%
      group_by(visitorType) %>%
      summarise(
        sessions = n_distinct(visitId),
        avg_pageviews = sum(as.integer(total_pageviews), na.rm = TRUE) / sessions,
        avg_bounce = sum(as.integer(total_bounces), na.rm = TRUE) / sessions
      )
  })
  
  # --- Outputs ---
  
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
