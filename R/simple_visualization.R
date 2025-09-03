# Simplified ASIH Conference Visualization
# Uses minimal dependencies for maximum compatibility

library(ggplot2)

# Simple geocoding function for major US cities
simple_geocode <- function(locations) {
  # Manual coordinates for major cities that commonly host conferences
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
  
  # Merge with input locations
  result <- merge(locations, city_coords, by = "location", all.x = TRUE)
  
  # For locations without coordinates, use approximate US center
  result$lat[is.na(result$lat)] <- 39.8283
  result$lon[is.na(result$lon)] <- -98.5795
  
  return(result)
}

# Function to create jittered coordinates
add_jitter <- function(meetings_df) {
  # Count meetings per location
  location_counts <- table(meetings_df$location)
  
  # Add jitter for locations with multiple meetings
  for (loc in names(location_counts)) {
    if (location_counts[loc] > 1) {
      indices <- which(meetings_df$location == loc)
      n_meetings <- length(indices)
      
      for (i in 1:n_meetings) {
        idx <- indices[i]
        jitter_offset <- (i - mean(1:n_meetings)) * 0.3
        meetings_df$lat_jittered[idx] <- meetings_df$lat[idx] + jitter_offset
        meetings_df$lon_jittered[idx] <- meetings_df$lon[idx] + jitter_offset * 0.2
      }
    } else {
      idx <- which(meetings_df$location == loc)
      meetings_df$lat_jittered[idx] <- meetings_df$lat[idx]
      meetings_df$lon_jittered[idx] <- meetings_df$lon[idx]
    }
  }
  
  return(meetings_df)
}

# Function to get state boundaries (simplified)
get_us_states <- function() {
  # Use built-in state data
  states <- map_data("state")
  return(states)
}

# Function to count state visits
count_state_visits <- function(meetings_df) {
  # Extract state from location
  meetings_df$state <- sub(".*,\\s*([A-Z]{2}).*", "\\1", meetings_df$location)
  
  # Convert state abbreviations to full names for mapping
  state_abbr_to_name <- c(
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
  
  # Count visits by state (US only)
  us_meetings <- meetings_df[!meetings_df$state %in% c("ON", "BC", "PQ", "AB", "Costa Rica", "Mexico", "Brazil"), ]
  state_counts <- table(us_meetings$state)
  
  # Convert to data frame and add full state names
  state_counts_df <- data.frame(
    state_abbr = names(state_counts),
    visits = as.numeric(state_counts),
    stringsAsFactors = FALSE
  )
  
  state_counts_df$region <- state_abbr_to_name[state_counts_df$state_abbr]
  state_counts_df <- state_counts_df[!is.na(state_counts_df$region), ]
  
  return(state_counts_df)
}

# Main visualization function
create_simple_map <- function() {
  # Load data
  meetings <- read.csv("data/asih_meetings.csv", stringsAsFactors = FALSE)
  
  # Add coordinates
  meetings_geo <- simple_geocode(meetings)
  
  # Add jitter for overlapping points
  meetings_jittered <- add_jitter(meetings_geo)
  
  # Get US state boundaries
  us_states <- get_us_states()
  
  # Count state visits
  state_counts <- count_state_visits(meetings_jittered)
  
  # Merge state data with visit counts
  us_states_with_counts <- merge(us_states, state_counts, by = "region", all.x = TRUE)
  us_states_with_counts$visits[is.na(us_states_with_counts$visits)] <- 0
  
  # Separate US and international meetings
  us_meetings <- meetings_jittered[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings_jittered$location), ]
  intl_meetings <- meetings_jittered[grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings_jittered$location), ]
  
  # Position international meetings around US borders
  if (nrow(intl_meetings) > 0) {
    intl_meetings$lat_jittered[grepl("ON|BC|PQ|AB", intl_meetings$location)] <- 
      intl_meetings$lat_jittered[grepl("ON|BC|PQ|AB", intl_meetings$location)] + 3
    intl_meetings$lat_jittered[grepl("Costa Rica", intl_meetings$location)] <- 25
    intl_meetings$lon_jittered[grepl("Costa Rica", intl_meetings$location)] <- -85
    intl_meetings$lat_jittered[grepl("Mexico", intl_meetings$location)] <- 28
    intl_meetings$lon_jittered[grepl("Mexico", intl_meetings$location)] <- -110
    intl_meetings$lat_jittered[grepl("Brazil", intl_meetings$location)] <- 20
    intl_meetings$lon_jittered[grepl("Brazil", intl_meetings$location)] <- -60
  }
  
  # Create the plot
  p <- ggplot() +
    # US states with heatmap
    geom_polygon(data = us_states_with_counts, 
                aes(x = long, y = lat, group = group, fill = visits),
                color = "white", size = 0.3) +
    scale_fill_gradient(low = "white", high = "red", name = "Total\nVisits") +
    
    # US meeting points
    geom_point(data = us_meetings,
              aes(x = lon_jittered, y = lat_jittered, color = year),
              size = 2, alpha = 0.8) +
    
    # International meeting points
    {if (nrow(intl_meetings) > 0) 
      geom_point(data = intl_meetings,
                aes(x = lon_jittered, y = lat_jittered, color = year),
                size = 3, alpha = 0.8, shape = 17)} +
    
    # Color scale for years
    scale_color_gradient(low = "#440154", high = "#fde725", name = "Year") +
    
    # Add year labels (sample every 5th point to avoid overcrowding)
    geom_text(data = us_meetings[seq(1, nrow(us_meetings), 5), ],
             aes(x = lon_jittered, y = lat_jittered, label = year),
             size = 2, vjust = -0.5, alpha = 0.7) +
    
    # Coordinate system and limits
    coord_map("albers", lat0 = 39, lat1 = 45, xlim = c(-125, -65), ylim = c(20, 50)) +
    
    # Theme and labels
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.box = "horizontal"
    ) +
    
    labs(
      title = "ASIH Annual Meeting Locations (1916-2025)",
      subtitle = "Points colored by year • States colored by total visits • Triangles = International meetings",
      caption = "Data source: ASIH Past Meetings (asih.org)"
    )
  
  return(p)
}