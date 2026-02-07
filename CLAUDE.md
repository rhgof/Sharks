# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based geospatial data analysis project that correlates Australian shark attack incidents (primarily NSW white sharks) with satellite-derived oceanographic data: chlorophyll concentration (CHL) and sea surface temperature (SST). Produces statistical analyses and publication-quality map visualizations.

## Running Code

This is an RStudio project (`Sharks.Rproj`). There is no build system, test suite, or linter. Scripts are run interactively in RStudio or via `source()`. Open the project in RStudio and source individual R files as needed.

## Key Dependencies

- **RUtils**: Custom R utility package (not on CRAN) that provides path resolution functions (`codeFile()`, `inputFile()`, `outputFile()`), file caching (`readCachedFile()`), chart saving helpers (`saveHorizChart()`, `saveVertChart()`, `saveSquareChart()`), and theming (`customTheme()`, `makeCaption()`). Must be installed separately.
- **CRAN packages**: tidyverse, terra, tidyterra, ncdf4, patchwork, gifski, paletteer, viridis, scales, readxl
- **External data cache**: Satellite NetCDF files are cached on `/Volumes/Samples/InputData/cache/` with 1-year expiration

## Architecture

### Data Pipeline

```
Shark DB (CSV) + IMOS Satellite Portal (NetCDF) + Shapefiles
        ↓
  Read/Parse Layer  →  readSharkDB.R, readIMOSFileListing.R, readChlAFiles.R
        ↓
  Download/Cache    →  downloadCHLFiles.R, downloadSSTFiles.R, getnetcdfFile.R
        ↓
  Processing        →  rastUtilities.R (chlRasterDateLocation, sstRasterDateLocation, cropToArea)
        ↓
  Analysis          →  buildSharkDataSet.R, buildCHLSSTTraceDataSet.R
        ↓
  Visualization     →  chartCHL.R, chartSST.R, plotAttackStats.R, animateData.R
        ↓
  Outputs           →  CSV statistics + PNG maps + GIF animations in Outputs/
```

### Main Entry Points

- **buildSharkDataSet.R**: Single-day snapshot analysis. For each incident, downloads CHL/SST data, extracts statistics at regional (±1 degree) and local (±0.2 degree) scales, generates side-by-side SST/CHL maps.
- **buildCHLSSTTraceDataSet.R**: 30-day temporal trace analysis. Same as above but iterates over 30 days prior to each incident to show temporal evolution.
- **plotAttackStats.R**: Statistical visualization of the output CSVs (scatter plots, map overlays).
- **AnalyseIncidents.R**: Animated GIF workflow showing 30-60 day CHL evolution per incident.

### Source Loading Pattern

Files are loaded via `source(codeFile("filename.R"))` where `codeFile()` resolves paths relative to the project's `R/` directory. All R source files live in `/R`.

### Key Data Conventions

- SST satellite data is in Kelvin; converted to Celsius via `sstRast - 273.15`
- CHL visualization uses log transformation: `log(theRast)` with breaks at c(0.1, 0.5, 1, 2, 4, 8, 16) mg/m^3
- Coordinate system: WGS84 (EPSG:4326) for satellite data; some bathymetry uses GDA2020 (EPSG:7844)
- Multi-scale analysis: regional extent (±1 degree ~111km) vs local/attack area (±0.2 degree ~22km)
- Named geographic extents defined in `usefulExtents.R` (AU, NSW, WA, etc.)

### Input Data

- `Inputs/Australian Shark-Incident Adjusted.csv`: Primary shark incident database
- `Inputs/IMOS/`: IMOS satellite data file listings
- `Inputs/AUS_2021_AUST_SHP_GDA2020/`: Australian boundary shapefiles
- Satellite data fetched on-demand from IMOS/AODN THREDDS servers and cached locally
