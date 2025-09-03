# Simple ASIH Conference Map Generator
# Minimal dependencies - uses only ggplot2 and base R

# Check and load required packages
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load data
cat("Loading ASIH meeting data...\n")
meetings <- read.csv("data/asih_meetings.csv", stringsAsFactors = FALSE)

# Source the visualization functions
source("R/simple_visualization.R")

# Generate the map
cat("Generating visualization...\n")
map_plot <- create_simple_map()

# Create output directory if it doesn't exist
if (!dir.exists("docs")) {
  dir.create("docs")
}

# Save high-resolution image
cat("Saving high-resolution map...\n")
ggsave("docs/asih_conference_map.png", 
       map_plot, 
       width = 16, height = 10, dpi = 300,
       bg = "white")

# Also save to output folder
if (!dir.exists("output")) {
  dir.create("output")
}
ggsave("output/asih_conference_map.png", 
       map_plot, 
       width = 16, height = 10, dpi = 300,
       bg = "white")

# Calculate and display statistics
cat("\n=== ASIH Conference Statistics ===\n")
total_meetings <- nrow(meetings)
years_span <- paste(min(meetings$year), "-", max(meetings$year))
us_meetings <- meetings[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]
intl_meetings <- meetings[grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]

# Count unique US states
us_states <- sub(".*,\\s*([A-Z]{2}).*", "\\1", us_meetings$location)
us_states_clean <- us_states[us_states %in% c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
  "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
  "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
  "WI", "WY"
)]
unique_states <- length(unique(us_states_clean))

# Count countries
countries <- c("USA", "Canada", "Costa Rica", "Mexico", "Brazil")
international_countries <- sum(grepl("ON|BC|PQ|AB", meetings$location)) + 
                          sum(grepl("Costa Rica", meetings$location)) +
                          sum(grepl("Mexico", meetings$location)) +
                          sum(grepl("Brazil", meetings$location))

cat(sprintf("Total meetings: %d\n", total_meetings))
cat(sprintf("Years covered: %s\n", years_span))
cat(sprintf("US meetings: %d\n", nrow(us_meetings)))
cat(sprintf("International meetings: %d\n", nrow(intl_meetings)))
cat(sprintf("US states visited: %d\n", unique_states))
cat(sprintf("Countries represented: %d (USA, Canada, Costa Rica, Mexico, Brazil)\n", 5))

# Most visited cities
city_counts <- sort(table(meetings$location), decreasing = TRUE)
cat("\nTop 10 most visited cities:\n")
for (i in 1:min(10, length(city_counts))) {
  cat(sprintf("%2d. %-25s (%d meetings)\n", i, names(city_counts)[i], city_counts[i]))
}

cat(sprintf("\nVisualization saved to:\n"))
cat(sprintf("- docs/asih_conference_map.png (for GitHub Pages)\n"))
cat(sprintf("- output/asih_conference_map.png (for local use)\n"))
cat(sprintf("\nDone!\n"))