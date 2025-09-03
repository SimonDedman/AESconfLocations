# ASIH Conference Map Generator - Main Script
# This is the main entry point - run this script from the project root

# Change to the R directory to run the visualization
setwd("R")
source("generate_ultra_simple_map.R")
setwd("..")

cat("\n🎉 Map generation complete! Check the docs/ and output/ folders for the visualization.\n")