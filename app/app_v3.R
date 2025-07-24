# Chargement de bibliothèques nécessaires
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

# Interface utilisateur avec shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de Bord Engagement Utilisateur"),
  dashboardSidebar(
    sidebarMenu(
      # Menu principal avec 4 onglets
      menuItem("Global", tabName = "global", icon = icon("chart-line")),
      menuItem("Par Géographie", tabName = "geo", icon = icon("globe")),
      menuItem("Par Appareil", tabName = "device", icon = icon("mobile-alt")),
      menuItem("Nouveaux vs Retournants", tabName = "visitor", icon = icon("user-friends"))
    ),
    # Choix de la métrique à afficher (sessions, pages moyennes, taux de rebond)
    radioButtons("metric", "Choisir la métrique :",
                 choices = c("Sessions Totales" = "sessions",
                             "Pages Moyennes par Session" = "avg_pageviews",
                             "Taux de Rebond" = "avg_bounce"),
                 selected = "sessions")
  ),
  dashboardBody(
    tabItems(
      # Onglet global : graphique temps
      tabItem(tabName = "global",
              fluidRow(
                box(width = 12,
                    title = "Série Temporelle Globale pour la métrique choisie", status = "primary", solidHeader = TRUE,
                    plotlyOutput("time_plot")
                )
              )
      ),
      # Onglet géographie : carte mondiale
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 12,
                    title = "Carte mondiale par métrique", status = "primary", solidHeader = TRUE,
                    plotlyOutput("map_plot")
                )
              )
      ),
      # Onglet appareil : graphique par device
      tabItem(tabName = "device",
              fluidRow(
                box(width = 12,
                    title = "Métrique par catégorie d’appareil", status = "primary", solidHeader = TRUE,
                    plotlyOutput("device_plot")
                )
              )
      ),
      # Onglet visiteur : nouveau vs ancien
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
  
  # Agrégation journalière en base Arrow (lazy evaluation) puis collecte finale en R
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
        avg_pageviews = total_pageviews / sessions,  # calcul pages moyennes/session
        avg_bounce = total_bounces / sessions        # calcul taux de rebond moyen
      )
  })
  
  # Agrégation par pays, conversion des noms pays en ISO3 pour jointure géographique
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
        iso3 = countrycode(geoNetwork_country, origin = "country.name", destination = "iso3c") # conversion nom pays en iso3
      ) %>%
      filter(!is.na(iso3))  # filtre pays sans code iso3 valide
  })
  
  # Agrégation par type d'appareil (desktop, mobile, etc.)
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
  
  # Agrégation par type de visiteur (nouveau vs ancien)
  # Collecte nécessaire avant mutation à cause de la condition sur total_newVisits
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
  
  # --- Outputs graphiques ---
  
  # Graphique série temporelle globale selon métrique choisie
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
  
  # Carte mondiale colorée selon métrique par pays
  output$map_plot <- renderPlotly({
    df <- country_metrics()
    
    world <- ne_countries(scale = "medium", returnclass = "sf") # carte du monde (sf)
    map_data <- left_join(world, df, by = c("iso_a3" = "iso3")) # jointure données
    
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
  
  # Histogramme métrique par catégorie d’appareil
  output$device_plot <- renderPlotly({
    df <- device_metrics()
    p <- ggplot(df, aes(x = device_deviceCategory, y = .data[[input$metric]], fill = device_deviceCategory)) +
      geom_col() +
      labs(title = paste(input$metric, "par Appareil"), x = "Appareil", y = input$metric) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Histogramme métrique par type de visiteur (nouveau vs ancien)
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

# Lancement de l'application Shiny
shinyApp(ui, server)
