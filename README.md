# ASIH Conference Locations Visualization

Interactive visualization of American Society of Ichthyologists and Herpetologists (ASIH) annual meeting locations from 1916-2025.

## Features

- **Interactive Map**: Pan, zoom, and click on meeting locations for details
- **Temporal Visualization**: Points colored by year using viridis palette (dark = old, bright = new)  
- **State Heatmap**: US states colored white-to-red based on total meeting frequency
- **International Support**: Fake states positioned outside US for international meetings
- **Chronological Flow**: Optional connecting lines showing meeting progression over time
- **Smart Jittering**: Overlapping points (like New Orleans) are separated for clarity
- **Filtering**: Year range and country filters available
- **Responsive Design**: Works on desktop and mobile devices

## Live Demo

Visit the interactive visualization at: https://simondedman.github.io/AESconfLocations/

## Data Source

Conference location data sourced from [ASIH Past Meetings](https://www.asih.org/past-meetings).

## Technical Stack

- **R** - Data processing and analysis
- **Shiny** - Interactive web application framework
- **Leaflet** - Interactive mapping
- **ggplot2** - Static visualizations with viridis color palette
- **sf & maps** - Geographic data processing
- **GitHub Actions** - Automated deployment
- **GitHub Pages** - Web hosting

## Local Development

1. Clone the repository:
```bash
git clone git@github.com:SimonDedman/AESconfLocations.git
cd AESconfLocations
```

2. Install required R packages:
```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "leaflet",
  "tidyverse", "sf", "maps", "viridis", "ggplot2", "dplyr", "purrr", 
  "tidygeocoder", "ggrepel", "rmarkdown", "knitr"
))
```

3. Run the main analysis:
```r
source("main.R")
```

4. Launch the Shiny app:
```r
shiny::runApp()
```

## Project Structure

```
AESconfLocations/
├── app.R                 # Main Shiny application
├── main.R               # Main analysis script  
├── index.Rmd            # R Markdown for GitHub Pages
├── data/
│   └── asih_meetings.csv # Conference location data
├── R/
│   ├── data_processing.R # Data cleaning and geocoding
│   └── visualization.R   # Plotting functions
├── output/              # Generated plots and results
└── .github/workflows/   # GitHub Actions deployment
```

## Data Processing

The visualization includes several data processing steps:

1. **Geocoding**: Convert city names to latitude/longitude coordinates
2. **Jittering**: Separate overlapping points for better visibility
3. **State Mapping**: Join meeting locations with US state boundaries
4. **International Handling**: Position international meetings appropriately
5. **Temporal Ordering**: Sort meetings chronologically for flow visualization

## Visualization Features

### Static Map
- US states colored by visit frequency (white = 0 visits, red = most visits)
- Meeting points colored by year (viridis palette)
- Jittered points for overlapping locations
- Optional chronological connecting lines with arrows
- Year labels on points

### Interactive Map  
- Leaflet-based web map with multiple base layers
- Clickable points showing meeting details
- Year range and country filtering
- Responsive design for mobile devices

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable  
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Contact

For questions or suggestions, please open an issue on GitHub.