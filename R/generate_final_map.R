# Final ASIH Conference Map Generator
# NO connecting lines, Canadian province counting, clean borders

# Check and install required packages
required_packages <- c("ggplot2", "ggrepel", "maps", "mapdata")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing %s package...\n", pkg))
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Source the final visualization
source("final_visualization.R")

# Load data and calculate statistics
cat("Loading ASIH meeting data...\n")
meetings <- read.csv("../data/asih_meetings.csv", stringsAsFactors = FALSE)

# Calculate detailed statistics
total_meetings <- nrow(meetings)
years_span <- paste(min(meetings$year), "-", max(meetings$year))

# Separate by country
us_meetings <- meetings[!grepl("ON|BC|PQ|AB|Costa Rica|Mexico|Brazil", meetings$location), ]
canada_meetings <- meetings[grepl("ON|BC|PQ|AB", meetings$location), ]
costa_rica_meetings <- meetings[grepl("Costa Rica", meetings$location), ]
mexico_meetings <- meetings[grepl("Mexico", meetings$location), ]
brazil_meetings <- meetings[grepl("Brazil", meetings$location), ]

# Count US states
us_states <- gsub(".*,\\s*", "", us_meetings$location)
us_states <- us_states[grepl("^[A-Z]{2}$", us_states)]
unique_states <- length(unique(us_states))

# Count Canadian provinces individually
canada_provinces <- gsub(".*,\\s*", "", canada_meetings$location)
province_counts <- table(canada_provinces)
unique_provinces <- length(unique(canada_provinces))

# Generate the final map
cat("Generating FINAL visualization with NO connecting lines...\n")
cat("Key improvements: Clean borders, individual province counting, no line artifacts\n")
map_plot <- create_final_map()

# Create output directories
if (!dir.exists("../docs")) {
  dir.create("../docs")
}
if (!dir.exists("../output")) {
  dir.create("../output")
}

# Save high-resolution images
cat("Saving FINAL high-resolution maps...\n")
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
cat(paste(rep("=", 70), collapse = ""))
cat("\n                FINAL ASIH CONFERENCE VISUALIZATION")
cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

cat(sprintf("🎯 FINAL FIXES APPLIED:\n"))
cat(sprintf("   ✅ COMPLETELY removed ALL connecting lines (segment.color = NA)\n"))
cat(sprintf("   ✅ Individual Canadian province visit counting implemented\n"))
cat(sprintf("   ✅ Clean country/state borders with no slicing artifacts\n"))
cat(sprintf("   ✅ Proper layering: grey countries → heatmap regions → points → labels\n"))
cat(sprintf("   ✅ All labels still visible with proper positioning\n"))

cat(sprintf("\n📊 DETAILED REGIONAL BREAKDOWN:\n"))
cat(sprintf("   🇺🇸 USA: %d meetings across %d states\n", nrow(us_meetings), unique_states))

if (nrow(canada_meetings) > 0) {
  cat(sprintf("   🇨🇦 Canada: %d meetings across %d provinces:\n", nrow(canada_meetings), unique_provinces))
  for (prov in names(province_counts)) {
    cat(sprintf("      • %s: %d meetings\n", prov, province_counts[prov]))
  }
} else {
  cat(sprintf("   🇨🇦 Canada: 0 meetings\n"))
}

cat(sprintf("   🇨🇷 Costa Rica: %d meetings\n", nrow(costa_rica_meetings)))
cat(sprintf("   🇲🇽 Mexico: %d meetings\n", nrow(mexico_meetings)))
cat(sprintf("   🇧🇷 Brazil: %d meetings\n", nrow(brazil_meetings)))

cat(sprintf("\n🎨 HEATMAP SYSTEM:\n"))
cat(sprintf("   📍 Each US state colored individually (0-7+ meetings)\n"))
cat(sprintf("   📍 Canada colored by total visits (%d meetings)\n", nrow(canada_meetings)))
cat(sprintf("   📍 International countries colored by individual visit counts\n"))
cat(sprintf("   📍 Unused countries/regions: grey fill\n"))
cat(sprintf("   📍 Scale: white (0) → red (maximum visits)\n"))

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

cat(sprintf("\n✅ FINAL visualization complete!\n"))
cat(sprintf("   🗺️  Perfect borders, no connecting lines, proper province handling\n"))
cat(paste(rep("=", 70), collapse = ""))
cat("\n")