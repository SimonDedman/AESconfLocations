# Improved ASIH Conference Visualization
# With proper boundaries, labels, and positioning

library(ggplot2)
library(ggrepel)

# Install maps package if needed for boundaries
install_maps_if_needed <- function() {
  if (!require("maps", quietly = TRUE)) {
    cat("Installing maps package for state boundaries...\n")
    install.packages("maps")
    library(maps)
  }
}

# Create comprehensive visualization with all requested features
create_comprehensive_map <- function() {
  # Install maps package if needed
  install_maps_if_needed()
  
  # Load data
  meetings <- read.csv("../data/asih_meetings.csv", stringsAsFactors = FALSE)
  
  # Manual geocoding with comprehensive coordinates
  city_coords <- data.frame(
    location = c(
      "New York, NY", "Philadelphia, PA", "Cambridge, MA", "Brooklyn, NY",
      "Washington, DC", "Chicago, IL", "Ann Arbor, MI", "Pittsburgh, PA",
      "Berkeley, CA", "Toronto, ON", "Gainesville, FL", "Higgins Lake, MI",
      "New Orleans, LA", "Salt Lake City, UT", "Austin, TX", "San Francisco, CA",
      "San Diego, CA", "Bloomington, IN", "Vancouver, BC", "Morehead City, NC",
      "Lawrence, KS", "Miami, FL", "Los Angeles, CA", "Boston, MA",
      "San Jose, Costa Rica", "Ottawa, ON", "Williamsburg, VA", "Fairbanks, AK",
      "Tempe, AZ", "Orono, ME", "Fort Worth, TX", "Corvallis, OR",
      "DeKalb, IL", "Tallahassee, FL", "Norman, OK", "Knoxville, TN",
      "Victoria, BC", "Albany, NY", "Charleston, SC", "Champaign-Urbana, IL",
      "Edmonton, AB", "Seattle, WA", "Guelph, ON", "University Park, PA",
      "La Paz, Mexico", "Kansas City, MO", "Manaus, Brazil", "Tampa, FL",
      "St. Louis, MO", "Montreal, PQ", "Portland, OR", "Providence, RI",
      "Minneapolis, MN", "Albuquerque, NM", "Chattanooga, TN", "Reno, NV",
      "Rochester, NY", "Snowbird, UT", "Phoenix, AZ", "Spokane, WA",
      "Norfolk, VA", "St. Paul, MN", "Northhampton, MA"
    ),
    lat = c(
      40.7128, 39.9526, 42.3601, 40.6782, 38.9072, 41.8781, 42.2808, 40.4406,
      37.8715, 43.6532, 29.6516, 44.4167, 29.9511, 40.7608, 30.2672, 37.7749,
      32.7157, 39.1654, 49.2827, 34.7199, 38.9717, 25.7617, 34.0522, 42.3601,
      9.9281, 45.4215, 37.2707, 64.8378, 33.4484, 44.8758, 32.7767, 44.5646,
      41.9301, 30.4518, 35.2271, 35.9606, 48.4284, 42.6526, 32.7765, 40.1020,
      53.5461, 47.6062, 43.5460, 40.7982, 24.1426, 39.0997, -3.1190, 27.9506,
      38.6270, 45.5017, 45.5152, 41.8240, 44.9778, 35.0844, 35.0456, 39.5296,
      43.1566, 40.5807, 33.4484, 47.6587, 36.8508, 44.9537, 42.3185
    ),
    lon = c(
      -74.0060, -75.1652, -71.0589, -73.9442, -77.0369, -87.6298, -83.7430, -79.9959,
      -122.2730, -79.3832, -82.3248, -84.6833, -90.0715, -111.8910, -97.7431, -122.4194,
      -117.1611, -86.5264, -123.1207, -76.7282, -95.2353, -80.1918, -118.2437, -71.0589,
      -84.0907, -75.6972, -76.7075, -147.7164, -111.9260, -68.6614, -97.3308, -123.2620,
      -88.7506, -84.2807, -97.4395, -83.9207, -123.3656, -73.8370, -79.9311, -88.2272,
      -113.4909, -122.3321, -80.2482, -77.8599, -110.3128, -94.5786, -60.0217, -82.4572,
      -90.1994, -73.5673, -122.6784, -71.4128, -93.2650, -106.6504, -85.3097, -119.7674,
      -77.6088, -111.6540, -112.0740, -117.4260, -76.2859, -93.0900, -72.5301
    ),
    stringsAsFactors = FALSE
  )
  
  # Merge with meetings data
  meetings_geo <- merge(meetings, city_coords, by = "location", all.x = TRUE)
  
  # Handle missing coordinates with approximate US center
  meetings_geo$lat[is.na(meetings_geo$lat)] <- 39.8283
  meetings_geo$lon[is.na(meetings_geo$lon)] <- -98.5795
  
  # USA centroid for international positioning calculations
  usa_center_lat <- 39.8283
  usa_center_lon <- -98.5795
  
  # Add country classification
  meetings_geo$country <- "USA"
  meetings_geo$country[grepl("ON|BC|PQ|AB", meetings_geo$location)] <- "Canada"
  meetings_geo$country[grepl("Costa Rica", meetings_geo$location)] <- "Costa Rica"
  meetings_geo$country[grepl("Mexico", meetings_geo$location)] <- "Mexico"
  meetings_geo$country[grepl("Brazil", meetings_geo$location)] <- "Brazil"
  
  # Position non-North American international meetings off borders
  # (Canada will stay in its proper geographic position)
  usa_center_lat <- 39.8283
  usa_center_lon <- -98.5795
  
  for (i in 1:nrow(meetings_geo)) {
    if (meetings_geo$country[i] == "Costa Rica") {
      meetings_geo$lat[i] <- 15  # South of US
      meetings_geo$lon[i] <- -85
    } else if (meetings_geo$country[i] == "Mexico") {
      meetings_geo$lat[i] <- 20  # Southwest of US
      meetings_geo$lon[i] <- -105
    } else if (meetings_geo$country[i] == "Brazil") {
      meetings_geo$lat[i] <- 12  # Southeast of US
      meetings_geo$lon[i] <- -55
    }
    # Canada stays in proper geographic position (no adjustment needed)
  }
  
  # Advanced jittering with better separation
  location_counts <- table(meetings_geo$location)
  meetings_geo$lat_jittered <- meetings_geo$lat
  meetings_geo$lon_jittered <- meetings_geo$lon
  meetings_geo$label_lat <- meetings_geo$lat
  meetings_geo$label_lon <- meetings_geo$lon
  
  for (loc in names(location_counts)) {
    if (location_counts[loc] > 1) {
      indices <- which(meetings_geo$location == loc)
      n_meetings <- length(indices)
      
      # Create circular arrangement for multiple meetings
      for (i in 1:n_meetings) {
        idx <- indices[i]
        angle <- (i - 1) * (2 * pi / n_meetings)
        radius <- 0.8
        
        meetings_geo$lat_jittered[idx] <- meetings_geo$lat[idx] + radius * sin(angle)
        meetings_geo$lon_jittered[idx] <- meetings_geo$lon[idx] + radius * cos(angle)
        
        # Position labels further out
        label_radius <- radius + 1.2
        meetings_geo$label_lat[idx] <- meetings_geo$lat[idx] + label_radius * sin(angle)
        meetings_geo$label_lon[idx] <- meetings_geo$lon[idx] + label_radius * cos(angle)
      }
    }
  }
  
  # Extract city names (remove state/country)
  meetings_geo$city <- gsub(",.*", "", meetings_geo$location)
  
  # Get North American boundaries (US states and Canadian provinces)
  us_states <- map_data("state")
  canada_provinces <- map_data("world", region = "Canada")  
  world_map <- map_data("world")
  
  # Count visits by state/province for heatmap
  us_meetings <- meetings_geo[meetings_geo$country == "USA", ]
  canada_meetings <- meetings_geo[meetings_geo$country == "Canada", ]
  
  # US state abbreviations to full names
  state_abbr_to_full <- c(
    "AL" = "alabama", "AK" = "alaska", "AZ" = "arizona", "AR" = "arkansas",
    "CA" = "california", "CO" = "colorado", "CT" = "connecticut", "DE" = "delaware",
    "FL" = "florida", "GA" = "georgia", "HI" = "hawaii", "ID" = "idaho",
    "IL" = "illinois", "IN" = "indiana", "IA" = "iowa", "KS" = "kansas",
    "KY" = "kentucky", "LA" = "louisiana", "ME" = "maine", "MD" = "maryland",
    "MA" = "massachusetts", "MI" = "michigan", "MN" = "minnesota", "MS" = "mississippi",
    "MO" = "missouri", "MT" = "montana", "NE" = "nebraska", "NV" = "nevada",
    "NH" = "new hampshire", "NJ" = "new jersey", "NM" = "new mexico", "NY" = "new york",
    "NC" = "north carolina", "ND" = "north dakota", "OH" = "ohio", "OK" = "oklahoma",
    "OR" = "oregon", "PA" = "pennsylvania", "RI" = "rhode island", "SC" = "south carolina",
    "SD" = "south dakota", "TN" = "tennessee", "TX" = "texas", "UT" = "utah",
    "VT" = "vermont", "VA" = "virginia", "WA" = "washington", "WV" = "west virginia",
    "WI" = "wisconsin", "WY" = "wyoming"
  )
  
  # Canadian province abbreviations
  province_abbr_to_visits <- c(
    "ON" = "Ontario", "BC" = "British Columbia", "PQ" = "Quebec", "AB" = "Alberta",
    "MB" = "Manitoba", "SK" = "Saskatchewan", "NS" = "Nova Scotia", "NB" = "New Brunswick",
    "NL" = "Newfoundland", "PE" = "Prince Edward Island", "YT" = "Yukon", "NT" = "Northwest Territories", "NU" = "Nunavut"
  )
  
  # Extract state abbreviations from US meetings
  us_meetings$state_abbr <- gsub(".*,\\s*([A-Z]{2}).*", "\\1", us_meetings$location)
  us_meetings$state_abbr <- ifelse(grepl("^[A-Z]{2}$", us_meetings$state_abbr), 
                                  us_meetings$state_abbr, NA)
  
  # Count visits by US state
  state_visits <- table(us_meetings$state_abbr[!is.na(us_meetings$state_abbr)])
  state_counts_df <- data.frame(
    state_abbr = names(state_visits),
    visits = as.numeric(state_visits),
    stringsAsFactors = FALSE
  )
  
  # Convert to full state names
  state_counts_df$region <- state_abbr_to_full[state_counts_df$state_abbr]
  state_counts_df <- state_counts_df[!is.na(state_counts_df$region), ]
  
  # Count visits by Canadian province
  if (nrow(canada_meetings) > 0) {
    canada_meetings$province_abbr <- gsub(".*,\\s*([A-Z]{2}).*", "\\1", canada_meetings$location)
    province_visits <- table(canada_meetings$province_abbr)
    province_counts_df <- data.frame(
      province_abbr = names(province_visits),
      visits = as.numeric(province_visits),
      stringsAsFactors = FALSE
    )
  } else {
    province_counts_df <- data.frame(province_abbr = character(0), visits = numeric(0))
  }
  
  # Merge with map data
  us_states_with_counts <- merge(us_states, state_counts_df, by = "region", all.x = TRUE)
  us_states_with_counts$visits[is.na(us_states_with_counts$visits)] <- 0
  us_states_with_counts$country <- "USA"
  
  # Prepare Canada data (simplified since world map doesn't have detailed provinces)
  canada_with_counts <- canada_provinces
  canada_with_counts$visits <- sum(province_counts_df$visits, na.rm = TRUE)
  canada_with_counts$country <- "Canada"
  
  # Create the comprehensive plot
  p <- ggplot() +
    # Blue background
    theme(
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # World boundaries (for context)
    geom_polygon(data = world_map, 
                aes(x = long, y = lat, group = group),
                fill = "lightgray", color = "white", linewidth = 0.2, alpha = 0.3) +
    
    # US states with heatmap coloring
    geom_polygon(data = us_states_with_counts, 
                aes(x = long, y = lat, group = group, fill = visits),
                color = "white", linewidth = 0.5) +
    
    # Canada with heatmap coloring
    geom_polygon(data = canada_with_counts, 
                aes(x = long, y = lat, group = group, fill = visits),
                color = "white", linewidth = 0.5) +
    
    scale_fill_gradient(low = "white", high = "red", name = "Total\nVisits", 
                       na.value = "lightgray") +
    
    # Meeting points
    geom_point(data = meetings_geo,
              aes(x = lon_jittered, y = lat_jittered, color = year, 
                  shape = country, size = country),
              alpha = 0.8, stroke = 1.5) +
    
    # Color scale for years (viridis)
    scale_color_gradient(low = "#440154FF", high = "#FDE725FF", name = "Year") +
    
    # Shape scale for countries
    scale_shape_manual(values = c("USA" = 16, "Canada" = 17, "Costa Rica" = 15, 
                                 "Mexico" = 18, "Brazil" = 8), name = "Country") +
    
    # Size scale for countries
    scale_size_manual(values = c("USA" = 2.5, "Canada" = 3.5, "Costa Rica" = 3.5, 
                                "Mexico" = 3.5, "Brazil" = 3.5), name = "Country") +
    
    # City labels with repel to avoid overlap
    geom_text_repel(data = meetings_geo,
                   aes(x = label_lon, y = label_lat, label = city),
                   size = 2, alpha = 0.8, fontface = "bold",
                   box.padding = 0.5, point.padding = 0.3,
                   segment.color = "gray50", segment.linewidth = 0.3,
                   max.overlaps = 50) +
    
    # Year labels with repel to avoid overlap
    geom_text_repel(data = meetings_geo,
                   aes(x = label_lon, y = label_lat, label = year),
                   size = 1.8, alpha = 0.7, color = "navy",
                   nudge_y = -0.5, nudge_x = 0.3,
                   box.padding = 0.3, point.padding = 0.2,
                   segment.color = "navy", segment.linewidth = 0.2,
                   max.overlaps = 50) +
    
    # Coordinate system and limits
    coord_fixed(ratio = 1.3, xlim = c(-130, -50), ylim = c(10, 60)) +
    
    # Theme and labels
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
      plot.caption = element_text(hjust = 0.5, size = 10, color = "gray60", margin = margin(t = 10)),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9),
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    labs(
      title = "ASIH Annual Meeting Locations (1916-2025)",
      subtitle = "All meetings labeled with city and year • States colored by visit frequency • International meetings positioned by geographic direction",
      caption = "Data source: American Society of Ichthyologists and Herpetologists (asih.org/past-meetings)"
    )
  
  return(p)
}