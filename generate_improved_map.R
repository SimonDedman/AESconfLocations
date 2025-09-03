# ASIH Conference Improved Map Generator - Main Script
# Run this script from the project root for the comprehensive visualization

# Change to the R directory to run the improved visualization
setwd("R")
source("generate_improved_map.R")
setwd("..")

cat("\n🎉 Improved map generation complete! \n")
cat("✅ All points labeled with cities and years\n")
cat("✅ State and country boundaries displayed\n") 
cat("✅ Canada included with proper geographic positioning\n")
cat("✅ State heatmap showing visit frequencies\n")
cat("✅ Blue oceanic background\n")
cat("✅ Advanced label positioning to prevent overlap\n\n")
cat("📁 Check the docs/ and output/ folders for the comprehensive visualization.\n")