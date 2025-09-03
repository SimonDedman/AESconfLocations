# Ultra-Simple ASIH Conference Map Generator
# Only requires ggplot2

# Check and load ggplot2
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Source the ultra-simple visualization
source("R/ultra_simple_viz.R")

# Load data and calculate statistics
cat("Loading ASIH meeting data...\n")
meetings <- read.csv("data/asih_meetings.csv", stringsAsFactors = FALSE)

# Calculate detailed statistics
total_meetings <- nrow(meetings)
years_span <- paste(min(meetings$year), "-", max(meetings$year))
year_range <- max(meetings$year) - min(meetings$year) + 1

# Separate US and international meetings
us_meetings <- meetings[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]
intl_meetings <- meetings[grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]

# Count unique US states visited
us_states_pattern <- "\\b([A-Z]{2})\\b$"
us_states <- gsub(".*,\\s*", "", us_meetings$location)
us_states <- us_states[grepl("^[A-Z]{2}$", us_states)]
unique_states <- length(unique(us_states))

# Count countries
countries_count <- 1 + # USA
  (sum(grepl("ON|BC|PQ|AB", meetings$location)) > 0) + # Canada
  (sum(grepl("Costa Rica", meetings$location)) > 0) + # Costa Rica
  (sum(grepl("Mexico", meetings$location)) > 0) + # Mexico
  (sum(grepl("Brazil", meetings$location)) > 0) # Brazil

# Generate the map
cat("Generating ultra-simple visualization...\n")
map_plot <- create_ultra_simple_map()

# Create output directories
if (!dir.exists("docs")) {
  dir.create("docs")
}
if (!dir.exists("output")) {
  dir.create("output")
}

# Save high-resolution images
cat("Saving high-resolution maps...\n")
ggsave("docs/asih_conference_map.png", 
       map_plot, 
       width = 16, height = 12, dpi = 300,
       bg = "white")

ggsave("output/asih_conference_map.png", 
       map_plot, 
       width = 16, height = 12, dpi = 300,
       bg = "white")

# Display comprehensive statistics
cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n           ASIH CONFERENCE STATISTICS\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n\n")

cat(sprintf("📊 OVERVIEW:\n"))
cat(sprintf("   Total meetings: %d\n", total_meetings))
cat(sprintf("   Years covered: %s (%d years)\n", years_span, year_range))
cat(sprintf("   US meetings: %d (%.1f%%)\n", nrow(us_meetings), 100 * nrow(us_meetings) / total_meetings))
cat(sprintf("   International meetings: %d (%.1f%%)\n", nrow(intl_meetings), 100 * nrow(intl_meetings) / total_meetings))
cat(sprintf("   US states visited: %d\n", unique_states))
cat(sprintf("   Countries represented: %d\n", countries_count))

# Most visited cities
city_counts <- sort(table(meetings$location), decreasing = TRUE)
cat(sprintf("\n🏙️  TOP 10 MOST VISITED CITIES:\n"))
for (i in 1:min(10, length(city_counts))) {
  cat(sprintf("   %2d. %-25s (%d meetings)\n", i, names(city_counts)[i], city_counts[i]))
}

# Meetings by decade
decades <- floor(meetings$year / 10) * 10
decade_counts <- sort(table(decades))
cat(sprintf("\n📅 MEETINGS BY DECADE:\n"))
for (decade in names(decade_counts)) {
  decade_label <- paste0(decade, "s")
  if (decade == "2020") decade_label <- "2020s"
  cat(sprintf("   %-6s: %d meetings\n", decade_label, decade_counts[decade]))
}

# International breakdown
if (nrow(intl_meetings) > 0) {
  cat(sprintf("\n🌍 INTERNATIONAL MEETINGS:\n"))
  canada_meetings <- sum(grepl("ON|BC|PQ|AB", intl_meetings$location))
  costa_rica_meetings <- sum(grepl("Costa Rica", intl_meetings$location))
  mexico_meetings <- sum(grepl("Mexico", intl_meetings$location))
  brazil_meetings <- sum(grepl("Brazil", intl_meetings$location))
  
  if (canada_meetings > 0) cat(sprintf("   Canada: %d meetings\n", canada_meetings))
  if (costa_rica_meetings > 0) cat(sprintf("   Costa Rica: %d meetings\n", costa_rica_meetings))
  if (mexico_meetings > 0) cat(sprintf("   Mexico: %d meetings\n", mexico_meetings))
  if (brazil_meetings > 0) cat(sprintf("   Brazil: %d meetings\n", brazil_meetings))
}

# File output information
cat(sprintf("\n💾 OUTPUT FILES:\n"))
cat(sprintf("   📈 docs/asih_conference_map.png (GitHub Pages)\n"))
cat(sprintf("   📈 output/asih_conference_map.png (Local use)\n"))
cat(sprintf("   🔧 Resolution: 16\" × 12\" at 300 DPI\n"))

cat(sprintf("\n✅ Visualization complete!\n"))
cat(paste(rep("=", 50), collapse = ""))
cat("\n")