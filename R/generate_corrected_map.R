# Corrected ASIH Conference Map Generator
# Fixed border slicing, proper country coloring, unified heatmap

# Check and install required packages
required_packages <- c("ggplot2", "ggrepel", "maps")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing %s package...\n", pkg))
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Source the corrected visualization
source("corrected_visualization.R")

# Load data and calculate statistics
cat("Loading ASIH meeting data...\n")
meetings <- read.csv("../data/asih_meetings.csv", stringsAsFactors = FALSE)

# Calculate detailed statistics
total_meetings <- nrow(meetings)
years_span <- paste(min(meetings$year), "-", max(meetings$year))
year_range <- max(meetings$year) - min(meetings$year) + 1

# Separate by country
us_meetings <- meetings[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]
canada_meetings <- meetings[grepl("ON|BC|PQ|AB", meetings$location), ]
costa_rica_meetings <- meetings[grepl("Costa Rica", meetings$location), ]
mexico_meetings <- meetings[grepl("Mexico", meetings$location), ]
brazil_meetings <- meetings[grepl("Brazil", meetings$location), ]

# Count unique US states visited
us_states <- gsub(".*,\\s*", "", us_meetings$location)
us_states <- us_states[grepl("^[A-Z]{2}$", us_states)]
unique_states <- length(unique(us_states))

# Count Canadian provinces
canada_provinces <- gsub(".*,\\s*", "", canada_meetings$location)
unique_provinces <- length(unique(canada_provinces))

# Generate the corrected map
cat("Generating corrected visualization with proper borders and coloring...\n")
cat("Key fixes: No connecting lines, unified heatmap, grey countries without meetings\n")
map_plot <- create_corrected_map()

# Create output directories
if (!dir.exists("../docs")) {
  dir.create("../docs")
}
if (!dir.exists("../output")) {
  dir.create("../output")
}

# Save high-resolution images
cat("Saving corrected high-resolution maps...\n")
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
cat(paste(rep("=", 65), collapse = ""))
cat("\n              CORRECTED ASIH CONFERENCE VISUALIZATION")
cat("\n")
cat(paste(rep("=", 65), collapse = ""))
cat("\n\n")

cat(sprintf("🔧 CORRECTIONS APPLIED:\n"))
cat(sprintf("   ✅ Removed connecting lines (prevented border slicing)\n"))
cat(sprintf("   ✅ Countries without meetings filled in grey\n"))
cat(sprintf("   ✅ Unified white-to-red heatmap scale for all regions\n"))
cat(sprintf("   ✅ Canada treated as individual country (not provinces)\n"))
cat(sprintf("   ✅ Mexico and Costa Rica use same heatmap scale\n"))
cat(sprintf("   ✅ Brazil properly positioned and colored\n"))
cat(sprintf("   ✅ Consistent border styling across all countries\n"))

cat(sprintf("\n📊 REGIONAL BREAKDOWN:\n"))
cat(sprintf("   🇺🇸 USA: %d meetings across %d states\n", nrow(us_meetings), unique_states))
cat(sprintf("   🇨🇦 Canada: %d meetings across %d provinces\n", nrow(canada_meetings), unique_provinces))
cat(sprintf("   🇨🇷 Costa Rica: %d meetings\n", nrow(costa_rica_meetings)))
cat(sprintf("   🇲🇽 Mexico: %d meetings\n", nrow(mexico_meetings)))
cat(sprintf("   🇧🇷 Brazil: %d meetings\n", nrow(brazil_meetings)))

cat(sprintf("\n🎨 HEATMAP COLORING:\n"))
if (nrow(us_meetings) > 0) {
  us_state_visits <- table(gsub(".*,\\s*", "", us_meetings$location))
  max_us_visits <- max(us_state_visits, na.rm = TRUE)
  cat(sprintf("   📍 US states: 0 (white) to %d visits (red)\n", max_us_visits))
}
cat(sprintf("   📍 Canada: %d total visits (single color)\n", nrow(canada_meetings)))
cat(sprintf("   📍 Costa Rica: %d visits\n", nrow(costa_rica_meetings)))
cat(sprintf("   📍 Mexico: %d visits\n", nrow(mexico_meetings)))
cat(sprintf("   📍 Brazil: %d visits\n", nrow(brazil_meetings)))
cat(sprintf("   📍 Other countries: 0 visits (grey fill)\n"))

# Most visited cities
city_counts <- sort(table(meetings$location), decreasing = TRUE)
cat(sprintf("\n🏙️  TOP 10 MOST VISITED CITIES:\n"))
for (i in 1:min(10, length(city_counts))) {
  cat(sprintf("   %2d. %-25s (%d meetings)\n", i, names(city_counts)[i], city_counts[i]))
}

# File output information
cat(sprintf("\n💾 OUTPUT FILES:\n"))
cat(sprintf("   📈 docs/asih_conference_map.png (GitHub Pages)\n"))
cat(sprintf("   📈 output/asih_conference_map.png (Local use)\n"))
cat(sprintf("   🔧 Resolution: 20\" × 14\" at 300 DPI\n"))

cat(sprintf("\n✅ Corrected visualization complete!\n"))
cat(sprintf("   🗺️  Intact borders, proper coloring, no slicing issues\n"))
cat(paste(rep("=", 65), collapse = ""))
cat("\n")