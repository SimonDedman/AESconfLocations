# ASIH Conference Map Generator
# Simple, clean approach using ggplot2 and sf with rnaturalearth boundaries

# Install and load required packages
required_packages <- c("ggplot2", "sf", "dplyr", "readr", "rnaturalearth", "rnaturalearthdata", "ggrepel", "scales")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Load the meeting data
meetings <- read_csv("data/all_locs_latlon.csv")
# Ensure Decade is treated as character for discrete coloring
meetings$Decade <- as.character(meetings$Decade)

# Clean up Philadelphia (informal dinner) entry
meetings$City[meetings$City == "Philadelphia (informal dinner)"] <- "Philadelphia"

# Create labels in format "City, State, Country; Year"
meetings$label <- paste0(meetings$City, 
                        ifelse(is.na(meetings$State) | meetings$State == "", 
                               "", paste0(", ", meetings$State)),
                        ", ", meetings$Country, "; ", meetings$Year)

# Load world boundaries and get subnational boundaries (states/provinces)
cat("Loading world boundaries...\n")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get US states
cat("Loading US states...\n")
us_states <- ne_states(country = "United States of America", returnclass = "sf")

# Get Canadian provinces  
cat("Loading Canadian provinces...\n")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Get other countries for context
mexico <- ne_countries(country = "Mexico", scale = "medium", returnclass = "sf")
costa_rica <- ne_countries(country = "Costa Rica", scale = "medium", returnclass = "sf") 
brazil <- ne_countries(country = "Brazil", scale = "medium", returnclass = "sf")

# Count visits by US state for heatmap coloring
us_state_visits <- meetings %>%
  filter(Country == "USA" & !is.na(State)) %>%
  group_by(State) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  rename(postal = State)

# Count visits by Canadian province for heatmap coloring  
canada_province_visits <- meetings %>%
  filter(Country == "Canada" & !is.na(State)) %>%
  group_by(State) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  rename(postal = State)

# Merge visit counts with boundary data
us_states_with_visits <- us_states %>%
  left_join(us_state_visits, by = "postal") %>%
  mutate(visits = ifelse(is.na(visits), 0, visits))

canada_provinces_with_visits <- canada_provinces %>%
  left_join(canada_province_visits, by = "postal") %>%  
  mutate(visits = ifelse(is.na(visits), 0, visits))

# Create decade colors - distinct colors for each decade
decade_levels <- sort(unique(meetings$Decade))
decade_colors <- c("#FF0000", "#FF8000", "#FFD700", "#80FF00", "#00FF80", 
                   "#00FFFF", "#0080FF", "#8000FF", "#FF00FF", "#FF0080",
                   "#C0C0C0", "#808080")
names(decade_colors) <- decade_levels

# Function to format years with smart concatenation
format_years <- function(years) {
  years <- sort(years)
  if(length(years) == 1) return(as.character(years[1]))
  
  formatted <- character(length(years))
  formatted[1] <- as.character(years[1])  # First year is always full
  
  for(i in 2:length(years)) {
    prev_year <- years[i-1]
    curr_year <- years[i]
    
    # If same millennium (same first 2 digits), use last 2 digits only
    if(floor(prev_year/100) == floor(curr_year/100)) {
      formatted[i] <- sprintf("%02d", curr_year %% 100)
    } else {
      formatted[i] <- as.character(curr_year)
    }
  }
  
  return(paste(formatted, collapse = ", "))
}

# Create labels in format "City; Year1, Year2, ..." with smart year concatenation
meetings_labels <- meetings %>%
  group_by(City, Longitude, Latitude) %>%
  summarise(
    years_formatted = format_years(Year),
    label = paste0(first(City), "; ", format_years(Year)),
    Decade = first(Decade),
    Country = first(Country),
    .groups = 'drop'
  )

# Calculate distances between cities to determine which need connection lines
# Only show connection lines for cities within 2 degrees of each other
meetings_labels$needs_line <- FALSE
for(i in 1:nrow(meetings_labels)) {
  distances <- sqrt((meetings_labels$Longitude[i] - meetings_labels$Longitude)^2 + 
                   (meetings_labels$Latitude[i] - meetings_labels$Latitude)^2)
  nearby_cities <- sum(distances < 2 & distances > 0)  # Within 2 degrees, not self
  if(nearby_cities > 0) {
    meetings_labels$needs_line[i] <- TRUE
  }
}

# Calculate US state centroids for labels and add visit counts
us_states_centroids <- us_states %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    cent_lon = st_coordinates(centroid)[,1],
    cent_lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(postal, cent_lon, cent_lat) %>%
  filter(!is.na(postal)) %>%
  left_join(us_state_visits, by = "postal") %>%
  mutate(
    visits = ifelse(is.na(visits), 0, visits),
    label = paste0(postal, ":", visits)
  )

# Calculate Canadian province centroids and add visit counts
canada_provinces_centroids <- canada_provinces %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    cent_lon = st_coordinates(centroid)[,1],
    cent_lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(postal, cent_lon, cent_lat) %>%
  filter(!is.na(postal)) %>%
  left_join(canada_province_visits, by = "postal") %>%
  mutate(
    visits = ifelse(is.na(visits), 0, visits),
    label = paste0(postal, ":", visits)
  )

# Adjust Canadian province label positions to prevent cutoff
canada_provinces_centroids$cent_lat[canada_provinces_centroids$postal == "BC"] <- 
  canada_provinces_centroids$cent_lat[canada_provinces_centroids$postal == "SK"]
canada_provinces_centroids$cent_lat[canada_provinces_centroids$postal == "AB"] <- 
  canada_provinces_centroids$cent_lat[canada_provinces_centroids$postal == "SK"]  
canada_provinces_centroids$cent_lat[canada_provinces_centroids$postal == "MB"] <- 
  canada_provinces_centroids$cent_lat[canada_provinces_centroids$postal == "SK"]

# Define map bounds
map_xlim <- c(-127, -68.5)
map_ylim <- c(23, 54.5)

# Identify cities outside the map bounds and create edge markers
outside_cities <- meetings_labels %>%
  filter(Longitude < map_xlim[1] | Longitude > map_xlim[2] | 
         Latitude < map_ylim[1] | Latitude > map_ylim[2])

# Create edge markers for outside cities with directional arrows
edge_markers <- data.frame()
if(nrow(outside_cities) > 0) {
  for(i in 1:nrow(outside_cities)) {
    city <- outside_cities[i,]
    
    # Use original coordinates as before, but adjust to map edge
    edge_lon <- pmax(map_xlim[1] + 0.5, pmin(map_xlim[2] - 0.5, city$Longitude))
    edge_lat <- pmax(map_ylim[1] + 0.5, pmin(map_ylim[2] - 0.5, city$Latitude))
    
    # Determine arrow direction based on original position relative to map bounds
    arrow_direction <- ""
    if(city$Longitude < map_xlim[1]) {
      edge_lon <- map_xlim[1] + 0.5
      arrow_direction <- paste0(arrow_direction, "←")
    }
    if(city$Longitude > map_xlim[2]) {
      edge_lon <- map_xlim[2] - 0.5
      arrow_direction <- paste0(arrow_direction, "→")
    }
    if(city$Latitude < map_ylim[1]) {
      edge_lat <- map_ylim[1] + 0.5
      arrow_direction <- paste0(arrow_direction, "↓")
    }
    if(city$Latitude > map_ylim[2]) {
      edge_lat <- map_ylim[2] - 0.5
      arrow_direction <- paste0(arrow_direction, "↑")
    }
    
    # Manual position adjustments for specific cities
    if(city$City == "Fairbanks") {
      edge_lon <- map_xlim[1] + 3  # NW corner - moved 1 degree east
      edge_lat <- map_ylim[2] - 0.5  # Top
    }
    if(city$City == "Manaus") {
      edge_lon <- map_xlim[2] - 3  # SE corner - moved slightly west (1 degree)
      edge_lat <- map_ylim[1] + 0.5  # Bottom
    }
    
    edge_markers <- rbind(edge_markers, data.frame(
      City = city$City,
      label = paste0(arrow_direction, " ", city$label),
      Longitude = edge_lon,
      Latitude = edge_lat,
      Decade = city$Decade,
      stringsAsFactors = FALSE
    ))
  }
}

# Filter meetings_labels to only include cities within bounds
meetings_labels_inside <- meetings_labels %>%
  filter(Longitude >= map_xlim[1] & Longitude <= map_xlim[2] & 
         Latitude >= map_ylim[1] & Latitude <= map_ylim[2])

# Recalculate needs_line for only the inside cities
meetings_labels_inside$needs_line <- FALSE
for(i in 1:nrow(meetings_labels_inside)) {
  distances <- sqrt((meetings_labels_inside$Longitude[i] - meetings_labels_inside$Longitude)^2 + 
                   (meetings_labels_inside$Latitude[i] - meetings_labels_inside$Latitude)^2)
  nearby_cities <- sum(distances < 2 & distances > 0)  # Within 2 degrees, not self
  if(nearby_cities > 0) {
    meetings_labels_inside$needs_line[i] <- TRUE
  }
}

# Set default label positioning to center under point
meetings_labels_inside$hjust <- 0.5
meetings_labels_inside$vjust <- 1.1  # Just below the point

# Specific adjustments only for problem labels that need to dodge
meetings_labels_inside$hjust[meetings_labels_inside$City == "Orono"] <- 1
meetings_labels_inside$hjust[meetings_labels_inside$City == "Bloomington"] <- 1
meetings_labels_inside$vjust[meetings_labels_inside$City == "Bloomington"] <- 0.5
meetings_labels_inside$hjust[meetings_labels_inside$City == "Ottawa"] <- 1  # Right align
meetings_labels_inside$vjust[meetings_labels_inside$City == "Ottawa"] <- 0.5
meetings_labels_inside$hjust[meetings_labels_inside$City == "Montreal"] <- 0  # Left align
meetings_labels_inside$vjust[meetings_labels_inside$City == "Montreal"] <- 0.5
meetings_labels_inside$hjust[meetings_labels_inside$City == "Brooklyn"] <- 0  # Left align to go east  
meetings_labels_inside$vjust[meetings_labels_inside$City == "Brooklyn"] <- -0.3  # Above NY label

# Split New York label into two lines and position east of point - split after '30'
meetings_labels_inside$label[meetings_labels_inside$City == "New York"] <- "New York; 1916, 30,\n34, 42, 53, 69, 91"
meetings_labels_inside$hjust[meetings_labels_inside$City == "New York"] <- 0  # Left align to go east
meetings_labels_inside$vjust[meetings_labels_inside$City == "New York"] <- 0.5  # Center vertically

# Additional specific adjustments
meetings_labels_inside$hjust[meetings_labels_inside$City == "Northampton"] <- 0.5
meetings_labels_inside$vjust[meetings_labels_inside$City == "Northampton"] <- 1.2  # Move down slightly
meetings_labels_inside$hjust[meetings_labels_inside$City == "Albany"] <- 0  # Move right (left align)
meetings_labels_inside$vjust[meetings_labels_inside$City == "Albany"] <- 0.5  # Near point

# Create the ggplot map
cat("Creating map...\n")
map_plot <- ggplot() +
  # Base world map (light gray with light grey borders)
  geom_sf(data = world, fill = "lightgray", color = "lightgrey", size = 0.3) +
  
  # US states with visit heatmap (light grey borders)
  geom_sf(data = us_states_with_visits, 
          aes(fill = visits), 
          color = "lightgrey", size = 0.5) +
  
  # Canadian provinces with visit heatmap (light grey borders)
  geom_sf(data = canada_provinces_with_visits, 
          aes(fill = visits), 
          color = "lightgrey", size = 0.5) +
  
  # Mexico colored exactly like states with 1 visit (New Mexico color)
  geom_sf(data = mexico, fill = "#FFECE5", color = "lightgrey", size = 0.5) +  # Exact 1-visit color
  geom_sf(data = costa_rica, fill = "#FFECE5", color = "lightgrey", size = 0.5) +  # Exact 1-visit color  
  geom_sf(data = brazil, fill = "#FFECE5", color = "lightgrey", size = 0.5) +  # Exact 1-visit color
  
  # Heatmap color scale for states/provinces
  scale_fill_gradient(low = "white", high = "red", name = "Visits", na.value = "lightgray") +
  
  # Add meeting points with white rings for visibility on red states
  geom_point(data = meetings_labels_inside, 
             aes(x = Longitude, y = Latitude),
             size = 3, color = "white", alpha = 1) +  # White ring
  geom_point(data = meetings_labels_inside, 
             aes(x = Longitude, y = Latitude, color = Decade),
             size = 2, alpha = 0.9) +  # Colored center
  
  # Decade color scale  
  scale_color_manual(values = decade_colors, name = "Decade") +
  
  # Add city labels - with connection lines only for cities near other cities (inside bounds)
  {if(nrow(meetings_labels_inside[meetings_labels_inside$needs_line, ]) > 0) {
    geom_text_repel(data = meetings_labels_inside[meetings_labels_inside$needs_line, ],
                    aes(x = Longitude, y = Latitude, label = label),
                    size = 5, alpha = 0.7,  # 2X size: 2.5 -> 5
                    box.padding = 0.35, point.padding = 0.3,
                    force = 3, max.overlaps = Inf,
                    min.segment.length = 0.1)
  }} +
  
  # Add city labels without connection lines for isolated cities (inside bounds)
  {if(nrow(meetings_labels_inside[!meetings_labels_inside$needs_line, ]) > 0) {
    geom_text(data = meetings_labels_inside[!meetings_labels_inside$needs_line, ],
              aes(x = Longitude, y = Latitude, label = label, hjust = hjust, vjust = vjust),
              size = 5, alpha = 0.7)  # 2X size: 2.5 -> 5
  }} +
  
  # Add directional arrow labels for edge markers (no points) - same style as other labels
  {if(nrow(edge_markers) > 0) {
    geom_text(data = edge_markers,
              aes(x = Longitude, y = Latitude, label = label),
              size = 5, alpha = 0.7, hjust = 0.5, vjust = 0.5, color = "darkred")  # Same size as other labels
  }} +
  
  # Add US state labels with visit counts at centroids
  geom_text(data = us_states_centroids,
            aes(x = cent_lon, y = cent_lat, label = label),
            size = 3, color = "grey60", alpha = 0.8, fontface = "bold") +
            
  # Add Canadian province labels with visit counts at centroids
  geom_text(data = canada_provinces_centroids,
            aes(x = cent_lon, y = cent_lat, label = label),
            size = 3, color = "grey60", alpha = 0.8, fontface = "bold") +
            
  # Add Mexico country label at La Paz latitude on mainland
  geom_text(aes(x = -104, y = 24, label = "MX:1"),
            size = 3, color = "grey60", alpha = 0.8, fontface = "bold") +
  
  # Set map bounds with tighter limits
  # NW tip of Vancouver Island, just east of Orono, bottom of Baja, just over Edmonton
  coord_sf(xlim = c(-127, -68.5), ylim = c(23, 54.5), expand = FALSE) +
  
  # Clean theme with stacked legends in right edge
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = c(0.98, 0.25),
    legend.justification = c("right", "center"),
    legend.box = "vertical",  # Stack vertically
    legend.box.just = "right",
    legend.background = element_rect(fill = "white", color = "lightgrey", linewidth = 0.5),
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 12),  # Larger legend titles
    legend.text = element_text(size = 10),   # Larger legend text
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Add title and subtitle
  labs(
    title = "ASIH Annual Meeting Locations (1916-2025)",
    subtitle = "Points colored by decade • States/provinces colored by visit frequency"
  )

# Create output directory
if (!dir.exists("docs")) {
  dir.create("docs")
}

# Save the map
cat("Saving map to docs/asih_conference_map.png...\n")
ggsave("docs/asih_conference_map.png", map_plot,
       width = 20, height = 14, units = "in", dpi = 300,
       bg = "white")

# Display summary statistics
cat("\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("ASIH CONFERENCE MAP GENERATED\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat(sprintf("Total meetings: %d\n", nrow(meetings)))
cat(sprintf("Years span: %d - %d\n", min(meetings$Year), max(meetings$Year)))
cat(sprintf("Countries: %d\n", length(unique(meetings$Country))))
cat(sprintf("Decades: %s\n", paste(decade_levels, collapse = ", ")))

# Count by decade
decade_counts <- table(meetings$Decade)
cat("\nMeetings by decade:\n")
for (d in names(decade_counts)) {
  cat(sprintf("  %s: %d meetings\n", d, decade_counts[d]))
}

# Top cities
city_counts <- sort(table(meetings$City), decreasing = TRUE)
cat("\nTop 5 most visited cities:\n")
for (i in 1:min(5, length(city_counts))) {
  cat(sprintf("  %d. %s (%d meetings)\n", i, names(city_counts)[i], city_counts[i]))
}

cat("\n✅ Map saved to docs/asih_conference_map.png\n")
cat("📁 Ready for GitHub Pages deployment\n")

# Generate 2000+ map
cat("\n")
cat(paste(rep("=", 75), collapse = ""))
cat("\nGENERATING 2000+ MAP\n")
cat(paste(rep("=", 75), collapse = ""))
cat("\n")

# Filter data for 2000 and afterwards
meetings_2000 <- meetings %>% filter(Year >= 2000)

# Create labels with smart year concatenation for 2000+ meetings
meetings_labels_2000 <- meetings_2000 %>%
  group_by(City, Longitude, Latitude) %>%
  summarise(
    years_formatted = format_years(Year),
    label = paste0(first(City), "; ", format_years(Year)),
    Decade = first(Decade),
    Country = first(Country),
    .groups = 'drop'
  )

# Calculate distances between cities for 2000+ meetings
meetings_labels_2000$needs_line <- FALSE
for(i in 1:nrow(meetings_labels_2000)) {
  distances <- sqrt((meetings_labels_2000$Longitude[i] - meetings_labels_2000$Longitude)^2 + 
                   (meetings_labels_2000$Latitude[i] - meetings_labels_2000$Latitude)^2)
  nearby_cities <- sum(distances < 2 & distances > 0)
  if(nearby_cities > 0) {
    meetings_labels_2000$needs_line[i] <- TRUE
  }
}

# Count visits by US state for 2000+ meetings
us_state_visits_2000 <- meetings_2000 %>%
  filter(Country == "USA" & !is.na(State)) %>%
  group_by(State) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  rename(postal = State)

# Count visits by Canadian province for 2000+ meetings
canada_province_visits_2000 <- meetings_2000 %>%
  filter(Country == "Canada" & !is.na(State)) %>%
  group_by(State) %>%
  summarise(visits = n(), .groups = 'drop') %>%
  rename(postal = State)

# Merge visit counts with boundary data for 2000+
us_states_with_visits_2000 <- us_states %>%
  left_join(us_state_visits_2000, by = "postal") %>%
  mutate(visits = ifelse(is.na(visits), 0, visits))

canada_provinces_with_visits_2000 <- canada_provinces %>%
  left_join(canada_province_visits_2000, by = "postal") %>%  
  mutate(visits = ifelse(is.na(visits), 0, visits))

# Update state/province centroids with 2000+ visit counts
us_states_centroids_2000 <- us_states %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    cent_lon = st_coordinates(centroid)[,1],
    cent_lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(postal, cent_lon, cent_lat) %>%
  filter(!is.na(postal)) %>%
  left_join(us_state_visits_2000, by = "postal") %>%
  mutate(
    visits = ifelse(is.na(visits), 0, visits),
    label = paste0(postal, ":", visits)
  )

canada_provinces_centroids_2000 <- canada_provinces %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    cent_lon = st_coordinates(centroid)[,1],
    cent_lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(postal, cent_lon, cent_lat) %>%
  filter(!is.na(postal)) %>%
  left_join(canada_province_visits_2000, by = "postal") %>%
  mutate(
    visits = ifelse(is.na(visits), 0, visits),
    label = paste0(postal, ":", visits)
  )

# Adjust Canadian province label positions for 2000+ map
canada_provinces_centroids_2000$cent_lat[canada_provinces_centroids_2000$postal == "BC"] <- 
  canada_provinces_centroids_2000$cent_lat[canada_provinces_centroids_2000$postal == "SK"]
canada_provinces_centroids_2000$cent_lat[canada_provinces_centroids_2000$postal == "AB"] <- 
  canada_provinces_centroids_2000$cent_lat[canada_provinces_centroids_2000$postal == "SK"]  
canada_provinces_centroids_2000$cent_lat[canada_provinces_centroids_2000$postal == "MB"] <- 
  canada_provinces_centroids_2000$cent_lat[canada_provinces_centroids_2000$postal == "SK"]

# Identify cities outside map bounds for 2000+ meetings
outside_cities_2000 <- meetings_labels_2000 %>%
  filter(Longitude < map_xlim[1] | Longitude > map_xlim[2] | 
         Latitude < map_ylim[1] | Latitude > map_ylim[2])

# Create edge markers for 2000+ outside cities
edge_markers_2000 <- data.frame()
if(nrow(outside_cities_2000) > 0) {
  for(i in 1:nrow(outside_cities_2000)) {
    city <- outside_cities_2000[i,]
    
    edge_lon <- pmax(map_xlim[1] + 0.5, pmin(map_xlim[2] - 0.5, city$Longitude))
    edge_lat <- pmax(map_ylim[1] + 0.5, pmin(map_ylim[2] - 0.5, city$Latitude))
    
    arrow_direction <- ""
    if(city$Longitude < map_xlim[1]) {
      edge_lon <- map_xlim[1] + 0.5
      arrow_direction <- paste0(arrow_direction, "←")
    }
    if(city$Longitude > map_xlim[2]) {
      edge_lon <- map_xlim[2] - 0.5
      arrow_direction <- paste0(arrow_direction, "→")
    }
    if(city$Latitude < map_ylim[1]) {
      edge_lat <- map_ylim[1] + 0.5
      arrow_direction <- paste0(arrow_direction, "↓")
    }
    if(city$Latitude > map_ylim[2]) {
      edge_lat <- map_ylim[2] - 0.5
      arrow_direction <- paste0(arrow_direction, "↑")
    }
    
    # Manual position adjustments for specific cities
    if(city$City == "Fairbanks") {
      edge_lon <- map_xlim[1] + 3
      edge_lat <- map_ylim[2] - 0.5
    }
    if(city$City == "Manaus") {
      edge_lon <- map_xlim[2] - 3
      edge_lat <- map_ylim[1] + 0.5
    }
    
    edge_markers_2000 <- rbind(edge_markers_2000, data.frame(
      City = city$City,
      label = paste0(arrow_direction, " ", city$label),
      Longitude = edge_lon,
      Latitude = edge_lat,
      Decade = city$Decade,
      stringsAsFactors = FALSE
    ))
  }
}

# Filter 2000+ meetings to only include cities within bounds
meetings_labels_inside_2000 <- meetings_labels_2000 %>%
  filter(Longitude >= map_xlim[1] & Longitude <= map_xlim[2] & 
         Latitude >= map_ylim[1] & Latitude <= map_ylim[2])

# Recalculate needs_line for 2000+ inside cities
meetings_labels_inside_2000$needs_line <- FALSE
for(i in 1:nrow(meetings_labels_inside_2000)) {
  distances <- sqrt((meetings_labels_inside_2000$Longitude[i] - meetings_labels_inside_2000$Longitude)^2 + 
                   (meetings_labels_inside_2000$Latitude[i] - meetings_labels_inside_2000$Latitude)^2)
  nearby_cities <- sum(distances < 2 & distances > 0)
  if(nearby_cities > 0) {
    meetings_labels_inside_2000$needs_line[i] <- TRUE
  }
}

# Set label positioning for 2000+ meetings
meetings_labels_inside_2000$hjust <- 0.5
meetings_labels_inside_2000$vjust <- 1.1

# Apply same specific adjustments for problematic labels
if("Orono" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Orono"] <- 1
}
if("Bloomington" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Bloomington"] <- 1
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "Bloomington"] <- 0.5
}
if("Ottawa" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Ottawa"] <- 1
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "Ottawa"] <- 0.5
}
if("Montreal" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Montreal"] <- 0
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "Montreal"] <- 0.5
}
if("Brooklyn" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Brooklyn"] <- 0
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "Brooklyn"] <- -0.3
}
if("New York" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$label[meetings_labels_inside_2000$City == "New York"] <- "New York; 2006, 16"
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "New York"] <- 0
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "New York"] <- 0.5
}
if("Northampton" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Northampton"] <- 0.5
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "Northampton"] <- 1.2
}
if("Albany" %in% meetings_labels_inside_2000$City) {
  meetings_labels_inside_2000$hjust[meetings_labels_inside_2000$City == "Albany"] <- 0
  meetings_labels_inside_2000$vjust[meetings_labels_inside_2000$City == "Albany"] <- 0.5
}

# Create the 2000+ ggplot map
cat("Creating 2000+ map...\n")
map_plot_2000 <- ggplot() +
  # Base world map
  geom_sf(data = world, fill = "lightgray", color = "lightgrey", size = 0.3) +
  
  # US states with visit heatmap
  geom_sf(data = us_states_with_visits_2000, 
          aes(fill = visits), 
          color = "lightgrey", size = 0.5) +
  
  # Canadian provinces with visit heatmap
  geom_sf(data = canada_provinces_with_visits_2000, 
          aes(fill = visits), 
          color = "lightgrey", size = 0.5) +
  
  # Mexico colored exactly like states with 1 visit
  geom_sf(data = mexico, fill = "#FFECE5", color = "lightgrey", size = 0.5) +
  geom_sf(data = costa_rica, fill = "#FFECE5", color = "lightgrey", size = 0.5) +  
  geom_sf(data = brazil, fill = "#FFECE5", color = "lightgrey", size = 0.5) +
  
  # Heatmap color scale for states/provinces
  scale_fill_gradient(low = "white", high = "red", name = "Visits", na.value = "lightgray") +
  
  # Add meeting points with white rings
  geom_point(data = meetings_labels_inside_2000, 
             aes(x = Longitude, y = Latitude),
             size = 3, color = "white", alpha = 1) +
  geom_point(data = meetings_labels_inside_2000, 
             aes(x = Longitude, y = Latitude, color = Decade),
             size = 2, alpha = 0.9) +
  
  # Decade color scale  
  scale_color_manual(values = decade_colors, name = "Decade") +
  
  # Add city labels with connection lines for nearby cities
  {if(nrow(meetings_labels_inside_2000) > 0 && nrow(meetings_labels_inside_2000[meetings_labels_inside_2000$needs_line, ]) > 0) {
    geom_text_repel(data = meetings_labels_inside_2000[meetings_labels_inside_2000$needs_line, ],
                    aes(x = Longitude, y = Latitude, label = label),
                    size = 5, alpha = 0.7,
                    box.padding = 0.35, point.padding = 0.3,
                    force = 3, max.overlaps = Inf,
                    min.segment.length = 0.1)
  }} +
  
  # Add city labels without connection lines
  {if(nrow(meetings_labels_inside_2000) > 0 && nrow(meetings_labels_inside_2000[!meetings_labels_inside_2000$needs_line, ]) > 0) {
    geom_text(data = meetings_labels_inside_2000[!meetings_labels_inside_2000$needs_line, ],
              aes(x = Longitude, y = Latitude, label = label, hjust = hjust, vjust = vjust),
              size = 5, alpha = 0.7)
  }} +
  
  # Add directional arrow labels for edge markers
  {if(nrow(edge_markers_2000) > 0) {
    geom_text(data = edge_markers_2000,
              aes(x = Longitude, y = Latitude, label = label),
              size = 5, alpha = 0.7, hjust = 0.5, vjust = 0.5, color = "darkred")
  }} +
  
  # Add US state labels with visit counts
  geom_text(data = us_states_centroids_2000,
            aes(x = cent_lon, y = cent_lat, label = label),
            size = 3, color = "grey60", alpha = 0.8, fontface = "bold") +
            
  # Add Canadian province labels with visit counts
  geom_text(data = canada_provinces_centroids_2000,
            aes(x = cent_lon, y = cent_lat, label = label),
            size = 3, color = "grey60", alpha = 0.8, fontface = "bold") +
            
  # Add Mexico country label (Mexico had 1 meeting in 2000)
  geom_text(aes(x = -104, y = 24, label = "MX:1"),
            size = 3, color = "grey60", alpha = 0.8, fontface = "bold") +
  
  # Set map bounds
  coord_sf(xlim = c(-127, -68.5), ylim = c(23, 54.5), expand = FALSE) +
  
  # Clean theme with stacked legends
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = c(0.98, 0.25),
    legend.justification = c("right", "center"),
    legend.box = "vertical",
    legend.box.just = "right",
    legend.background = element_rect(fill = "white", color = "lightgrey", linewidth = 0.5),
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Add title and subtitle
  labs(
    title = "ASIH Annual Meeting Locations (2000-2025)",
    subtitle = "Points colored by decade • States/provinces colored by visit frequency"
  )

# Save the 2000+ map
cat("Saving 2000+ map to docs/asih_conference_map_2000.png...\n")
ggsave("docs/asih_conference_map_2000.png", map_plot_2000,
       width = 20, height = 14, units = "in", dpi = 300,
       bg = "white")

# Display 2000+ statistics
cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("ASIH CONFERENCE MAP 2000+ GENERATED\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat(sprintf("Total meetings 2000+: %d\n", nrow(meetings_2000)))
cat(sprintf("Years span: %d - %d\n", min(meetings_2000$Year), max(meetings_2000$Year)))

# Count by decade for 2000+
decade_counts_2000 <- table(meetings_2000$Decade)
cat("\nMeetings by decade (2000+):\n")
for (d in names(decade_counts_2000)) {
  cat(sprintf("  %s: %d meetings\n", d, decade_counts_2000[d]))
}

# Top cities for 2000+
city_counts_2000 <- sort(table(meetings_2000$City), decreasing = TRUE)
cat("\nTop 5 most visited cities (2000+):\n")
for (i in 1:min(5, length(city_counts_2000))) {
  cat(sprintf("  %d. %s (%d meetings)\n", i, names(city_counts_2000)[i], city_counts_2000[i]))
}

cat("\n✅ 2000+ Map saved to docs/asih_conference_map_2000.png\n")
cat("📁 Both maps ready for GitHub Pages deployment\n")