# Improved ASIH Conference Map Generator
# With boundaries, labels, proper positioning, and blue background

# Check and install required packages
required_packages <- c("ggplot2", "ggrepel", "maps")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing %s package...\n", pkg))
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Source the improved visualization
source("improved_visualization.R")

# Load data and calculate statistics
cat("Loading ASIH meeting data...\n")
meetings <- read.csv("../data/asih_meetings.csv", stringsAsFactors = FALSE)

# Calculate detailed statistics
total_meetings <- nrow(meetings)
years_span <- paste(min(meetings$year), "-", max(meetings$year))
year_range <- max(meetings$year) - min(meetings$year) + 1

# Separate US and international meetings
us_meetings <- meetings[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]
intl_meetings <- meetings[grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]

# Count unique US states visited
us_states <- gsub(".*,\\s*", "", us_meetings$location)
us_states <- us_states[grepl("^[A-Z]{2}$", us_states)]
unique_states <- length(unique(us_states))

# Count countries
countries_count <- 1 + # USA
  (sum(grepl("ON|BC|PQ|AB", meetings$location)) > 0) + # Canada
  (sum(grepl("Costa Rica", meetings$location)) > 0) + # Costa Rica
  (sum(grepl("Mexico", meetings$location)) > 0) + # Mexico
  (sum(grepl("Brazil", meetings$location)) > 0) # Brazil

# Generate the improved map
cat("Generating comprehensive visualization with boundaries, labels, and proper positioning...\n")
cat("This may take a moment due to the detailed labeling...\n")
map_plot <- create_comprehensive_map()

# Create output directories
if (!dir.exists("../docs")) {
  dir.create("../docs")
}
if (!dir.exists("../output")) {
  dir.create("../output")
}

# Save high-resolution images
cat("Saving high-resolution maps...\n")
ggsave("../docs/asih_conference_map.png", 
       map_plot, 
       width = 20, height = 14, dpi = 300,
       bg = "white")

ggsave("../output/asih_conference_map.png", 
       map_plot, 
       width = 20, height = 14, dpi = 300,
       bg = "white")

# Display comprehensive statistics
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n              IMPROVED ASIH CONFERENCE VISUALIZATION")
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n\n")

cat(sprintf("🗺️  VISUALIZATION IMPROVEMENTS:\n"))
cat(sprintf("   ✅ All points labeled with city names and years\n"))
cat(sprintf("   ✅ State and country boundaries displayed\n"))
cat(sprintf("   ✅ States colored by visit frequency (white to red heatmap)\n"))
cat(sprintf("   ✅ International meetings positioned by geographic direction\n"))
cat(sprintf("   ✅ Blue background for oceanic appearance\n"))
cat(sprintf("   ✅ Advanced label dodging to prevent overlap\n"))
cat(sprintf("   ✅ Different shapes for different countries\n"))

cat(sprintf("\n📊 DATA OVERVIEW:\n"))
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

# International breakdown
if (nrow(intl_meetings) > 0) {
  cat(sprintf("\n🌍 INTERNATIONAL MEETINGS (with directional positioning):\n"))
  canada_meetings <- sum(grepl("ON|BC|PQ|AB", intl_meetings$location))
  costa_rica_meetings <- sum(grepl("Costa Rica", intl_meetings$location))
  mexico_meetings <- sum(grepl("Mexico", intl_meetings$location))
  brazil_meetings <- sum(grepl("Brazil", intl_meetings$location))
  
  if (canada_meetings > 0) cat(sprintf("   🇨🇦 Canada: %d meetings (positioned north of USA)\n", canada_meetings))
  if (costa_rica_meetings > 0) cat(sprintf("   🇨🇷 Costa Rica: %d meetings (positioned south of USA)\n", costa_rica_meetings))
  if (mexico_meetings > 0) cat(sprintf("   🇲🇽 Mexico: %d meetings (positioned southwest of USA)\n", mexico_meetings))
  if (brazil_meetings > 0) cat(sprintf("   🇧🇷 Brazil: %d meetings (positioned southeast of USA)\n", brazil_meetings))
}

# File output information
cat(sprintf("\n💾 OUTPUT FILES:\n"))
cat(sprintf("   📈 docs/asih_conference_map.png (GitHub Pages)\n"))
cat(sprintf("   📈 output/asih_conference_map.png (Local use)\n"))
cat(sprintf("   🔧 Resolution: 20\" × 14\" at 300 DPI (increased for label clarity)\n"))

cat(sprintf("\n✅ Comprehensive visualization complete!\n"))
cat(paste(rep("=", 60), collapse = ""))
cat("\n")