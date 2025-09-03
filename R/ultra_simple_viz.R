# Ultra-simple ASIH Conference Visualization
# Uses only ggplot2 - no maps package needed

library(ggplot2)

# Create ultra-simple visualization
create_ultra_simple_map <- function() {
  # Load data
  meetings <- read.csv("../data/asih_meetings.csv", stringsAsFactors = FALSE)
  
  # Simple geocoding using manual coordinates for major cities
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
  
  # Add jitter for overlapping points
  location_counts <- table(meetings_geo$location)
  meetings_geo$lat_jittered <- meetings_geo$lat
  meetings_geo$lon_jittered <- meetings_geo$lon
  
  for (loc in names(location_counts)) {
    if (location_counts[loc] > 1) {
      indices <- which(meetings_geo$location == loc)
      n_meetings <- length(indices)
      
      for (i in 1:n_meetings) {
        idx <- indices[i]
        jitter_offset <- (i - mean(1:n_meetings)) * 0.5
        meetings_geo$lat_jittered[idx] <- meetings_geo$lat[idx] + jitter_offset
        meetings_geo$lon_jittered[idx] <- meetings_geo$lon[idx] + jitter_offset * 0.3
      }
    }
  }
  
  # Separate US and international meetings
  us_meetings <- meetings_geo[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings_geo$location), ]
  intl_meetings <- meetings_geo[grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings_geo$location), ]
  
  # Create a simple background grid for US boundaries (very simplified)
  us_outline <- data.frame(
    x = c(-125, -125, -65, -65, -125),
    y = c(20, 50, 50, 20, 20)
  )
  
  # Create the plot
  p <- ggplot() +
    # Simple US boundary box
    geom_path(data = us_outline, aes(x = x, y = y), 
              color = "gray60", size = 1, linetype = "dashed") +
    
    # US meeting points
    geom_point(data = us_meetings,
              aes(x = lon_jittered, y = lat_jittered, color = year, size = 2),
              alpha = 0.8) +
    
    # International meeting points (as triangles)
    {if (nrow(intl_meetings) > 0) 
      geom_point(data = intl_meetings,
                aes(x = lon_jittered, y = lat_jittered, color = year),
                size = 4, alpha = 0.8, shape = 17)} +
    
    # Color scale for years (viridis-like colors)
    scale_color_gradient(low = "#440154FF", high = "#FDE725FF", name = "Year") +
    scale_size_identity() +
    
    # Add selected year labels to avoid overcrowding
    geom_text(data = us_meetings[us_meetings$year %% 10 == 0 | us_meetings$year %in% c(1916, 2025), ],
             aes(x = lon_jittered, y = lat_jittered, label = year),
             size = 2.5, vjust = -0.8, alpha = 0.7, fontface = "bold") +
    
    # Coordinate system and limits
    coord_fixed(ratio = 1.3, xlim = c(-130, -60), ylim = c(15, 55)) +
    
    # Theme and labels
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
      plot.caption = element_text(hjust = 0.5, size = 10, color = "gray60", margin = margin(t = 10)),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    labs(
      title = "ASIH Annual Meeting Locations (1916-2025)",
      subtitle = "Circles = US meetings • Triangles = International meetings • Color indicates year",
      caption = "Data source: American Society of Ichthyologists and Herpetologists (asih.org/past-meetings)"
    )
  
  return(p)
}