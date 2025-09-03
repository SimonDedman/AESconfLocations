# ASIH Conference Locations Visualization
# Main script to run the analysis and create visualizations

# Load required libraries
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidygeocoder)
library(ggrepel)

# Source helper functions
source("R/data_processing.R")
source("R/visualization.R")

# Main execution
main <- function() {
  cat("Loading and processing ASIH meeting data...\n")
  
  # Load and process the data
  meetings <- read_and_process_meetings()
  cat("Loaded", nrow(meetings), "meeting records\n")
  
  # Geocode the meetings
  cat("Geocoding meeting locations...\n")
  meetings_geocoded <- geocode_meetings(meetings)
  
  # Create jittered coordinates for overlapping points
  cat("Creating jittered coordinates for overlapping locations...\n")
  meetings_jittered <- create_jittered_coordinates(meetings_geocoded)
  
  # Count state visits
  cat("Calculating state visit counts...\n")
  state_counts <- create_state_visit_counts(meetings_jittered)
  
  # Create the main visualization
  cat("Creating conference map visualization...\n")
  conference_map <- create_conference_map(meetings_jittered, state_counts)
  
  # Add chronological connecting lines
  cat("Adding chronological connecting lines...\n")
  conference_map_with_lines <- add_chronological_lines(conference_map, meetings_jittered)
  
  # Save the plot
  cat("Saving visualization...\n")
  ggsave("output/asih_conference_map.png", 
         conference_map_with_lines, 
         width = 16, height = 10, dpi = 300)
  
  cat("Visualization saved to output/asih_conference_map.png\n")
  
  # Display summary statistics
  cat("\nSummary Statistics:\n")
  cat("Total meetings:", nrow(meetings_jittered), "\n")
  cat("Years covered:", min(meetings_jittered$year), "-", max(meetings_jittered$year), "\n")
  cat("US states visited:", length(unique(meetings_jittered$state_province[meetings_jittered$country == "USA"])), "\n")
  cat("International locations:", sum(meetings_jittered$country != "USA"), "\n")
  
  # Most visited cities
  city_counts <- meetings_jittered %>%
    count(location, sort = TRUE) %>%
    head(10)
  
  cat("\nTop 10 most visited cities:\n")
  print(city_counts)
  
  return(list(
    meetings = meetings_jittered,
    state_counts = state_counts,
    plot = conference_map_with_lines
  ))
}

# Run the analysis
if (!interactive()) {
  results <- main()
}