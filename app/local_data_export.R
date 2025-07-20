# Local files for dashboard deployment

library(dplyr)
library(arrow)
library(here)
library(countrycode)
library(lubridate)

full_df <- open_dataset(here("data"), format = "parquet", partitioning = c("year", "month"))

daily <- full_df %>%
  group_by(date) %>%
  summarise(
    sessions = n_distinct(visitId),
    total_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE),
    total_bounces = sum(as.numeric(total_bounces), na.rm = TRUE)
  ) %>%
  collect() %>%
  mutate(
    date = ymd(as.character(date)),
    avg_pageviews = total_pageviews / sessions,
    avg_bounce = total_bounces / sessions
  )

write.csv(daily, "daily_summary.csv", row.names = FALSE)


country <- full_df %>%
  group_by(geoNetwork_country) %>%
  summarise(
    sessions = n_distinct(visitId),
    total_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE),
    total_bounces = sum(as.numeric(total_bounces), na.rm = TRUE)
  ) %>%
  collect() %>%
  mutate(
    geoNetwork_country = as.character(geoNetwork_country),
    avg_pageviews = total_pageviews / sessions,
    avg_bounce = total_bounces / sessions,
    iso3 = countrycode(geoNetwork_country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(!is.na(iso3))

write.csv(country, "country_summary.csv", row.names = FALSE)


device <- full_df %>%
  group_by(device_deviceCategory) %>%
  summarise(
    sessions = n_distinct(visitId),
    total_pageviews = sum(as.numeric(total_pageviews), na.rm = TRUE),
    total_bounces = sum(as.numeric(total_bounces), na.rm = TRUE)
  ) %>%
  collect() %>%
  mutate(
    device_deviceCategory = as.character(device_deviceCategory),
    avg_pageviews = total_pageviews / sessions,
    avg_bounce = total_bounces / sessions
  )

write.csv(device, "device_summary.csv", row.names = FALSE)


visitor <- full_df %>%
  select(total_newVisits, visitId, total_pageviews, total_bounces) %>%
  collect() %>%
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

write.csv(visitor, "visitor_summary.csv", row.names = FALSE)
