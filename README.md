# ASIH Conference Locations Visualization

Static visualization of American Society of Ichthyologists and Herpetologists (ASIH) annual meeting locations from 1916-2025.

## 📊 Overview Statistics

- **105 total meetings** across 110 years (1916-2025)
- **28 US states visited** with multiple repeat locations
- **5 countries represented**: USA, Canada, Costa Rica, Mexico, Brazil
- **Top cities**: New York (7 meetings), New Orleans (6), Washington DC (6)

## 🎯 Features

- **Temporal Visualization**: Points colored by year using viridis-inspired palette (dark = old, bright = new)  
- **Smart Jittering**: Overlapping points (like New Orleans) are separated for clarity
- **International Support**: International meetings marked with triangles
- **Minimal Dependencies**: Uses only ggplot2 and base R for maximum compatibility
- **High Resolution**: 16"×12" at 300 DPI output suitable for presentations
- **Comprehensive Statistics**: Detailed breakdown by decade, city, and country

## 🌐 Live Demo

Visit the visualization at: https://simondedman.github.io/AESconfLocations/

## 📋 Data Source

Conference location data sourced from [ASIH Past Meetings](https://www.asih.org/past-meetings).

## 🛠️ Technical Stack

- **R** - Data processing and statistical analysis
- **ggplot2** - Static visualizations with custom color palette
- **Base R functions** - Manual geocoding and data processing
- **GitHub Pages** - Static web hosting
- **Minimal dependencies** - Only requires ggplot2!

## 🚀 Quick Start

1. Clone the repository:
```bash
git clone git@github.com:SimonDedman/AESconfLocations.git
cd AESconfLocations
```

2. Install ggplot2 (only required package):
```r
install.packages("ggplot2")
```

3. Generate the visualization:
```r
source("generate_map.R")
```

**That's it!** The script will create high-resolution PNG files in both `docs/` and `output/` folders.

## 📁 Project Structure

```
AESconfLocations/
├── generate_map.R               # Main script - run this!
├── data/
│   └── asih_meetings.csv        # Conference location data (1916-2025)
├── R/                           # All R code organized here
│   ├── generate_ultra_simple_map.R  # Core generation script
│   ├── ultra_simple_viz.R       # Visualization functions
│   ├── main.R                   # Original main script
│   └── data_processing.R        # Data processing functions
├── docs/                        # GitHub Pages site
│   ├── index.html              # Project homepage
│   └── asih_conference_map.png # Generated visualization
└── output/                      # Local output folder
    └── asih_conference_map.png # Copy of generated visualization
```

## 🔍 Data Processing

The visualization includes several key steps:

1. **Manual Geocoding**: Pre-defined coordinates for 60+ major cities
2. **Smart Jittering**: Automatic separation of overlapping meeting locations
3. **International Positioning**: Special handling for non-US meetings
4. **Temporal Coloring**: Year-based color mapping using viridis-inspired palette
5. **Statistical Analysis**: Comprehensive breakdown by location, decade, and country

## 📈 Output Statistics

The script generates detailed statistics including:
- **Top 10 most visited cities** with meeting counts
- **Meetings by decade** showing temporal distribution  
- **International meetings breakdown** by country
- **Geographic coverage** across US states
- **High-resolution visualization** ready for presentations

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 📬 Contact

For questions, suggestions, or collaborations, please:
- Open an issue on [GitHub](https://github.com/SimonDedman/AESconfLocations/issues)
- Visit the live demo at [simondedman.github.io/AESconfLocations](https://simondedman.github.io/AESconfLocations/)

---
*🤖 Generated with [Claude Code](https://claude.ai/code)*