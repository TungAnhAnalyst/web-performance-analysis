# Chargement des librairies nécessaires
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

# Chargement des données pré-agrégées depuis des fichiers CSV locaux
daily_summary <- read.csv("daily_summary.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))  # Conversion de la colonne date en format Date
country_summary <- read.csv("country_summary.csv", stringsAsFactors = FALSE) # Résumé par pays
device_summary <- read.csv("device_summary.csv", stringsAsFactors = FALSE)   # Résumé par appareil
visitor_summary <- read.csv("visitor_summary.csv", stringsAsFactors = FALSE) # Résumé par type de visiteur

# Chargement des données géographiques du monde sous forme spatiale (sf)
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Définition de l'interface utilisateur du dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de Bord Engagement Utilisateur"), # Titre du dashboard
  dashboardSidebar(  # Barre latérale avec menu et options
    # sidebar avec 4 onglets
    sidebarMenu(
      menuItem("Global", tabName = "global", icon = icon("chart-line")),
      menuItem("Par Géographie", tabName = "geo", icon = icon("globe")),       
      menuItem("Par Appareil", tabName = "device", icon = icon("mobile-alt")),
      menuItem("Nouveaux vs Retournants", tabName = "visitor", icon = icon("user-friends"))
    ),
    # Choix de la métrique à afficher via un bouton radio (3 métriques à choisir)
    radioButtons("metric", "Choisir la métrique :",
                 choices = c("Sessions Totales" = "sessions",
                             "Pages Moyennes par Session" = "avg_pageviews",
                             "Taux de Rebond" = "avg_bounce"),
                 selected = "sessions")
  ),
  dashboardBody(
    tabItems(
      # Contenu onglet Global : série temporelle de la métrique choisie
      tabItem(tabName = "global",
              fluidRow(
                box(width = 12, title = "Série Temporelle Globale pour la métrique choisie",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("time_plot"))  # Graphique interactif temps vs métrique
              )),
      # Contenu onglet Géographie : carte du monde colorée selon la métrique
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 12, title = "Carte mondiale par métrique",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("map_plot"))  # Carte interactive
              )),
      # Contenu onglet Appareil : graphique par catégorie d'appareil
      tabItem(tabName = "device",
              fluidRow(
                box(width = 12, title = "Métrique par catégorie d’appareil",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("device_plot"))  # Histogramme par appareil
              )),
      # Contenu onglet Visiteur : graphique par type de visiteur (nouveau ou retournant)
      tabItem(tabName = "visitor",
              fluidRow(
                box(width = 12, title = "Métrique par type de visiteur",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("visitor_plot"))  # Histogramme visiteurs
              ))
    )
  )
)

# Serveur : logique de calcul et rendu des graphiques
server <- function(input, output, session) {
  
  # Graphique temps : évolution quotidienne de la métrique sélectionnée
  output$time_plot <- renderPlotly({
    df <- daily_summary
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
    ggplotly(p)  # Conversion en graphique interactif Plotly
  })
  
  # Carte mondiale avec la métrique par pays
  output$map_plot <- renderPlotly({
    df <- country_summary
    # Fusion des données pays avec données géographiques
    map_data <- left_join(world_sf, df, by = c("iso_a3" = "iso3"))
    fill_vals <- map_data[[input$metric]]
    
    # Texte à afficher au survol (tooltip)
    tooltip_text <- paste0(
      map_data$name_long, "\n",
      switch(input$metric,
             sessions = paste0("Sessions Totales : ", scales::comma(fill_vals)),
             avg_pageviews = paste0("Pages Moyennes par Session : ", round(fill_vals, 2)),
             avg_bounce = paste0("Taux de Rebond : ", scales::percent(fill_vals, accuracy = 0.1))
      )
    )
    
    # Création de la carte avec couleur selon la métrique
    p <- ggplot(map_data) +
      geom_sf(aes(fill = fill_vals, text = tooltip_text), color = "white") +  # Polygones pays
      scale_fill_viridis_c(
        option = "plasma",
        na.value = "grey90",  # Couleur si pas de données
        labels = if(input$metric == "avg_bounce") scales::percent_format(accuracy = 1) else scales::label_number(big.mark = ",")
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
    ggplotly(p, tooltip = "text")  # Carte interactive avec tooltips
  })
  
  # Graphique par appareil
  output$device_plot <- renderPlotly({
    df <- device_summary
    p <- ggplot(df, aes(x = device_deviceCategory, y = .data[[input$metric]], fill = device_deviceCategory)) +
      geom_col() +
      labs(title = paste(input$metric, "par Appareil"), x = "Appareil", y = input$metric) +
      theme_minimal() +
      theme(legend.position = "none")  # Pas de légende
    ggplotly(p)
  })
  
  # Graphique par type de visiteur (nouveau ou retournant)
  output$visitor_plot <- renderPlotly({
    df <- visitor_summary
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
