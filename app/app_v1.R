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
library(rsconnect)
library(fansi)

# Chargement et préparation des données
full_df <- open_dataset(here("data"), format = "parquet", partitioning = c("year", "month"))
df <- full_df %>% collect()

# Correction temporaires des données, à supprimer une fois gesnéralisé
df$total_pageviews <- as.numeric(as.character(df$total_pageviews))
df$total_timeOnsite <- as.numeric(as.character(df$total_timeOnsite))
df$total_bounces <- as.character(df$total_bounces)
df$total_bounces <- ifelse(is.na(df$total_bounces), "0", df$total_bounces)  
df$total_bounces <- ifelse(df$total_bounces == "1", "1", "0")  
df$total_bounces <- as.numeric(df$total_bounces) 
df$total_newVisits <- as.character(df$total_newVisits)

df$type_visiteur <- recode(df$total_newVisits,
                           "1" = "Nouveau",
                           .missing = "Ancien")

df <- df %>%
  mutate(
    date = ymd(date),
    visitorType = as.factor(type_visiteur),
    deviceCategory = as.factor(device_deviceCategory),
    country = geoNetwork_country,
    continent = countrycode(country, origin = "country.name", destination = "continent"),
    visitId = as.character(visitId)
  )

daily_metrics <- df %>%
  group_by(date) %>%
  summarise(
    sessions = n_distinct(visitId),
    avg_pageviews = sum(total_pageviews, na.rm = TRUE) / n_distinct(visitId),
    avg_bounce = sum(total_bounces, na.rm = TRUE) / n_distinct(visitId)
  ) %>%
  ungroup()

# Interface utilisateur
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
      # Onglet global : graphique en série temporelle
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
      
      # Onglet appareil
      tabItem(tabName = "device",
              fluidRow(
                box(width = 12,
                    title = "Métrique par catégorie d’appareil", status = "primary", solidHeader = TRUE,
                    plotlyOutput("device_plot")
                )
              )
      ),
      
      # Onglet visiteurs
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

# Serveur
server <- function(input, output) {
  
  output$map_plot <- renderPlotly({
    country_metrics <- df %>%
      group_by(country) %>%
      summarise(
        sessions = n_distinct(visitId),
        avg_pageviews = sum(total_pageviews, na.rm = TRUE) / n_distinct(visitId),
        avg_bounce = sum(total_bounces, na.rm = TRUE) / n_distinct(visitId)
      ) %>%
      mutate(
        iso3 = countrycode(country, origin = "country.name", destination = "iso3c")
      ) %>%
      filter(!is.na(iso3))
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    map_data <- world %>%
      left_join(country_metrics, by = c("iso_a3" = "iso3"))
    
    fill_values <- map_data[[input$metric]]
    
    format_number <- label_number(accuracy = 1, big.mark = ",", decimal.mark = ".")
    tooltip_text <- paste0(
      map_data$name_long, "\n",
      switch(input$metric,
             sessions = paste0("Sessions Totales : ", format_number(fill_values)),
             avg_pageviews = paste0("Pages Moyennes par Session : ", format_number(fill_values)),
             avg_bounce = paste0("Taux de Rebond : ", scales::percent(fill_values, accuracy = 0.1))
      )
    )
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = fill_values, text = tooltip_text), color = "white") +
      scale_fill_viridis_c(
        option = "plasma",
        na.value = "grey90",
        labels = if(input$metric == "avg_bounce") {
          scales::percent_format(accuracy = 1)
        } else {
          scales::label_number(big.mark = ",")
        }
      ) +
      labs(title = paste("Carte des", switch(input$metric,
                                             sessions = "Sessions Totales",
                                             avg_pageviews = "Pages Moyennes par Session",
                                             avg_bounce = "Taux de Rebond"
      ), "par Pays"),
      fill = switch(input$metric,
                    sessions = "Sessions Totales",
                    avg_pageviews = "Pages Moyennes/Session",
                    avg_bounce = "Taux de Rebond")
      ) +
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
      labs(title = paste(input$metric, "par Appareil"), x = "Appareil", y = input$metric) +
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
      labs(title = paste(input$metric, "par Type de Visiteur"), x = "Type de Visiteur", y = input$metric) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$time_plot <- renderPlotly({
    p <- ggplot(daily_metrics, aes(x = date, y = .data[[input$metric]])) +
      geom_line(color = "#1f77b4") +
      labs(title = paste("Evolution quotidienne des", switch(input$metric,
                                                             sessions = "Sessions Totales",
                                                             avg_pageviews = "Pages Moyennes par Session",
                                                             avg_bounce = "Taux de Rebond")),
           x = "Date", y = switch(input$metric,
                                  sessions = "Sessions Totales",
                                  avg_pageviews = "Pages Moyennes/Session",
                                  avg_bounce = "Taux de Rebond")) +
      theme_minimal()
    ggplotly(p)
  })
}

# Lancement de l'application
shinyApp(ui, server)
