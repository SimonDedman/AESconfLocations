library(ggplot2)
library(sf)
library(maps)
library(viridis)
library(dplyr)
library(ggrepel)

# Function to create fake states for international locations
create_fake_international_states <- function(meetings_df) {
  international_meetings <- meetings_df %>%
    filter(country != "USA") %>%
    distinct(country, .keep_all = TRUE)
  
  # Define positions for fake states around the US
  fake_states <- tibble(
    country = c("Canada", "Costa Rica", "Mexico", "Brazil"),
    fake_lat = c(52, 10, 25, -10),
    fake_lon = c(-105, -84, -110, -55),
    state_size = c(8, 2, 5, 8)  # Size factor for fake states
  )
  
  # Create simplified country outlines (simplified polygons)
  # For demo purposes, creating simple rectangles
  fake_state_polygons <- fake_states %>%
    mutate(
      min_lat = fake_lat - state_size/2,
      max_lat = fake_lat + state_size/2,
      min_lon = fake_lon - state_size/2,
      max_lon = fake_lon + state_size/2
    )
  
  return(list(fake_states = fake_states, polygons = fake_state_polygons))
}

# Main visualization function
create_conference_map <- function(meetings_df, state_counts) {
  # Get US state boundaries
  us_states <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
  
  # Prepare state names for joining
  us_states <- us_states %>%
    mutate(state_name = tools::toTitleCase(ID)) %>%
    mutate(
      state_abbr = case_when(
        state_name == "Alabama" ~ "AL",
        state_name == "Alaska" ~ "AK", 
        state_name == "Arizona" ~ "AZ",
        state_name == "Arkansas" ~ "AR",
        state_name == "California" ~ "CA",
        state_name == "Colorado" ~ "CO",
        state_name == "Connecticut" ~ "CT",
        state_name == "Delaware" ~ "DE",
        state_name == "Florida" ~ "FL",
        state_name == "Georgia" ~ "GA",
        state_name == "Hawaii" ~ "HI",
        state_name == "Idaho" ~ "ID",
        state_name == "Illinois" ~ "IL",
        state_name == "Indiana" ~ "IN",
        state_name == "Iowa" ~ "IA",
        state_name == "Kansas" ~ "KS",
        state_name == "Kentucky" ~ "KY",
        state_name == "Louisiana" ~ "LA",
        state_name == "Maine" ~ "ME",
        state_name == "Maryland" ~ "MD",
        state_name == "Massachusetts" ~ "MA",
        state_name == "Michigan" ~ "MI",
        state_name == "Minnesota" ~ "MN",
        state_name == "Mississippi" ~ "MS",
        state_name == "Missouri" ~ "MO",
        state_name == "Montana" ~ "MT",
        state_name == "Nebraska" ~ "NE",
        state_name == "Nevada" ~ "NV",
        state_name == "New Hampshire" ~ "NH",
        state_name == "New Jersey" ~ "NJ",
        state_name == "New Mexico" ~ "NM",
        state_name == "New York" ~ "NY",
        state_name == "North Carolina" ~ "NC",
        state_name == "North Dakota" ~ "ND",
        state_name == "Ohio" ~ "OH",
        state_name == "Oklahoma" ~ "OK",
        state_name == "Oregon" ~ "OR",
        state_name == "Pennsylvania" ~ "PA",
        state_name == "Rhode Island" ~ "RI",
        state_name == "South Carolina" ~ "SC",
        state_name == "South Dakota" ~ "SD",
        state_name == "Tennessee" ~ "TN",
        state_name == "Texas" ~ "TX",
        state_name == "Utah" ~ "UT",
        state_name == "Vermont" ~ "VT",
        state_name == "Virginia" ~ "VA",
        state_name == "Washington" ~ "WA",
        state_name == "West Virginia" ~ "WV",
        state_name == "Wisconsin" ~ "WI",
        state_name == "Wyoming" ~ "WY",
        TRUE ~ ID
      )
    )
  
  # Join with state visit counts
  us_states_with_counts <- us_states %>%
    left_join(state_counts, by = c("state_abbr" = "state")) %>%
    mutate(visit_count = replace_na(visit_count, 0))
  
  # Create fake international states
  fake_intl <- create_fake_international_states(meetings_df)
  
  # Filter meetings for US only for the main map
  us_meetings <- meetings_df %>% filter(country == "USA")
  intl_meetings <- meetings_df %>% filter(country != "USA")
  
  # Create base map
  p <- ggplot() +
    # US states with heatmap coloring
    geom_sf(data = us_states_with_counts, 
            aes(fill = visit_count), 
            color = "white", 
            size = 0.3) +
    scale_fill_gradient(low = "white", high = "red", name = "Total Visits") +
    
    # Add US meeting points
    geom_point(data = us_meetings, 
               aes(x = longitude_jittered, 
                   y = latitude_jittered, 
                   color = year),
               size = 2, alpha = 0.8) +
    
    # Add international meeting points on fake states
    geom_point(data = intl_meetings %>% 
                 left_join(fake_intl$fake_states, by = "country"), 
               aes(x = fake_lon, y = fake_lat, color = year),
               size = 3, alpha = 0.8) +
    
    # Color scale for temporal progression
    scale_color_viridis_c(name = "Year", option = "viridis") +
    
    # Labels for years
    geom_text_repel(data = us_meetings, 
                    aes(x = longitude_jittered, 
                        y = latitude_jittered, 
                        label = year),
                    size = 2, alpha = 0.7) +
    
    # Coordinate system
    coord_sf(xlim = c(-125, -65), ylim = c(20, 50)) +
    
    # Theming
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    
    labs(
      title = "ASIH Annual Meeting Locations (1916-2025)",
      subtitle = "Points colored by year (viridis), states colored by total visits (white to red)"
    )
  
  return(p)
}

# Function to add chronological connecting lines
add_chronological_lines <- function(plot, meetings_df) {
  # Sort meetings by year
  meetings_sorted <- meetings_df %>%
    arrange(year) %>%
    mutate(
      next_lat = lead(latitude_jittered),
      next_lon = lead(longitude_jittered)
    ) %>%
    filter(!is.na(next_lat))
  
  # Add connecting lines with arrows
  plot_with_lines <- plot +
    geom_segment(data = meetings_sorted,
                 aes(x = longitude_jittered, y = latitude_jittered,
                     xend = next_lon, yend = next_lat),
                 arrow = arrow(length = unit(0.02, "npc"), 
                              ends = "last", type = "closed"),
                 alpha = 0.3, color = "gray50", size = 0.5)
  
  return(plot_with_lines)
}