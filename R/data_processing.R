library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidygeocoder)

read_and_process_meetings <- function() {
  meetings <- read_csv("data/asih_meetings.csv")
  
  # Clean and standardize location names
  meetings <- meetings %>%
    mutate(
      city = str_extract(location, "^[^,]+"),
      state_province = str_extract(location, "(?<=, ).+"),
      country = case_when(
        state_province %in% c("ON", "BC", "PQ", "AB") ~ "Canada",
        state_province == "Costa Rica" ~ "Costa Rica",
        state_province == "Mexico" ~ "Mexico", 
        state_province == "Brazil" ~ "Brazil",
        TRUE ~ "USA"
      )
    ) %>%
    # Handle special cases
    mutate(
      state_province = case_when(
        location == "San Jose, Costa Rica" ~ "Costa Rica",
        location == "La Paz, Mexico" ~ "Mexico",
        location == "Manaus, Brazil" ~ "Brazil",
        TRUE ~ state_province
      )
    )
  
  return(meetings)
}

geocode_meetings <- function(meetings_df) {
  # Geocode using tidygeocoder
  meetings_geocoded <- meetings_df %>%
    mutate(
      full_address = paste(city, state_province, country, sep = ", ")
    ) %>%
    geocode(full_address, method = 'osm', lat = latitude, long = longitude)
  
  # Manual coordinates for problematic locations
  manual_coords <- tribble(
    ~location, ~latitude, ~longitude,
    "Higgins Lake, MI", 44.4167, -84.6833,
    "University Park, PA", 40.7982, -77.8599,
    "Champaign-Urbana, IL", 40.1164, -88.2434,
    "Snowbird, UT", 40.5807, -111.6540,
    "San Jose, Costa Rica", 9.9281, -84.0907,
    "La Paz, Mexico", 24.1426, -110.3128,
    "Manaus, Brazil", -3.1190, -60.0217
  )
  
  # Update missing coordinates with manual ones
  meetings_geocoded <- meetings_geocoded %>%
    left_join(manual_coords, by = "location", suffix = c("", "_manual")) %>%
    mutate(
      latitude = coalesce(latitude, latitude_manual),
      longitude = coalesce(longitude, longitude_manual)
    ) %>%
    select(-latitude_manual, -longitude_manual)
  
  return(meetings_geocoded)
}

create_state_visit_counts <- function(meetings_df) {
  # Count visits by state for US locations only
  state_counts <- meetings_df %>%
    filter(country == "USA") %>%
    count(state_province, name = "visit_count") %>%
    rename(state = state_province)
  
  return(state_counts)
}

create_jittered_coordinates <- function(meetings_df) {
  # Group by location and add jitter for overlapping points
  meetings_jittered <- meetings_df %>%
    group_by(location) %>%
    mutate(
      n_meetings = n(),
      jitter_offset = if_else(n_meetings > 1, 
                             seq(-0.1, 0.1, length.out = n_meetings),
                             0)
    ) %>%
    mutate(
      latitude_jittered = latitude + jitter_offset,
      longitude_jittered = longitude + jitter_offset * 0.5  # Less jitter on longitude
    ) %>%
    ungroup()
  
  return(meetings_jittered)
}