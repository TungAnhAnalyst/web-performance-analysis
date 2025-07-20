# Projet de l'Analyse de la Performance Web avec Google Analytics Samples  

## Présentation

Ce projet montre comment accéder aux données Google Analytics via l'API Google BigQuery, sauvegarder les données efficacement au format Parquet, analyser la performance web, et présenter les résultats de manière interactive via un tableau de bord Shiny.

---

## Structure du projet

- **`code/get_data_script.Rmd`**  
  Script R Markdown expliquant comment se connecter à l'API Google BigQuery, télécharger l’échantillon de données Google Analytics, et sauvegarder localement au format Parquet sur **`data/`**. Les données sont partitionnées par année et mois pour une gestion optimale.

- **`code/analysis.Rmd`**  
  Script R Markdown réalisant et visualisant une analyse des métriques de performance web sur les données téléchargées. Le résultat est un rapport HTML présentant les insights et tendances.

- **`app/`**  
  Contient une application Shiny construisant un tableau de bord interactif pour explorer les données de performance web. L’application est publiée en ligne et accessible via [ce lien ShinyApps.io](https://dy08vl-dinh0tung0anh-tran.shinyapps.io/web-performance-analysis/).

- **`output/`**  
  Contient les livrables produits par les deux fichiers `get_data_script.Rmd` et `analysis.Rmd`.

