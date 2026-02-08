# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based geospatial data analysis project that correlates Australian shark attack incidents (primarily NSW white sharks) with satellite-derived oceanographic data: chlorophyll concentration (CHL) and sea surface temperature (SST). Produces statistical analyses and publication-quality map visualizations.

## Running Code

This is an RStudio project (`Sharks.Rproj`). There is no build system or linter. Scripts are run interactively in RStudio or via `source()`. Open the project in RStudio and source individual R files as needed.

### Tests

Tests use `testthat` and live in `tests/`. Run from the project root:

```r
Rscript -e 'testthat::test_dir("tests")'
```

Test files use `source(codeFile(...))` to load the real source files, with a `setwd` guard for testthat compatibility. Current test coverage: `cropToArea()`, `imosFiles()`.

## Key Dependencies

- **RUtils**: Custom R utility package (not on CRAN) that provides path resolution functions (`codeFile()`, `inputFile()`, `outputFile()`), file caching (`readCachedFile()`), chart saving helpers (`saveHorizChart()`, `saveVertChart()`, `saveSquareChart()`), and theming (`customTheme()`, `makeCaption()`). Must be installed separately.
- **CRAN packages**: tidyverse, terra, tidyterra, ncdf4, patchwork, gifski, paletteer, viridis, scales, readxl
- **External data cache**: Satellite NetCDF files are cached on `/Volumes/Samples/InputData/cache/` with 1-year expiration

## Architecture

### Data Pipeline

```
Shark DB (CSV) + IMOS Satellite Portal (NetCDF) + Shapefiles
        ↓
  Read/Parse Layer  →  readSharkDB.R, readIMOSFileListing.R (imosFiles), readChlAFiles.R
        ↓
  Download/Cache    →  downloadCHLFiles.R, downloadSSTFiles.R, getnetcdfFile.R
        ↓
  Processing        →  rastUtilities.R (cropToArea, chlRasterDateLocation, sstRasterDateLocation)
        ↓
  Analysis          →  buildSharkDataSet.R, buildCHLSSTTraceDataSet.R
        ↓
  Visualization     →  chartCHL.R, chartSST.R, plotCHL.R, plotSST.R, plotAttackStats.R, animateData.R
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

### Key Design Decisions

- **Canonical function locations**: `cropToArea()`, `chlRasterDateLocation()`, and `sstRasterDateLocation()` are defined in `rastUtilities.R`. No other file should redefine these.
- **IMOS file listing**: `readIMOSFileListing.R` provides a unified `imosFiles(type)` function with `type = "CHL"` or `"SST"`. Thin wrappers `imosCHLFiles()` and `imosSSTFiles()` exist for backward compatibility.
- **Retry safety**: `chlRasterDateLocation()` and `sstRasterDateLocation()` have a `maxDaysPrior = 30` cap to prevent infinite loops when satellite data is unavailable.
- **plotCHL.R and plotSST.R** are structurally parallel scripts with intentionally different layer extraction (CHL uses all layers; SST selects `"sea_surface_temperature"` and converts from Kelvin).
- **CRS handling**: Satellite data arrives in WGS84 (EPSG:4326). Use `crs(rast) <- "EPSG:7844"` to relabel to GDA2020 (not `project()`, which unnecessarily resamples at this scale).

### Key Data Conventions

- SST satellite data is in Kelvin; converted to Celsius via `sstRast - 273.15`
- CHL visualization uses log transformation: `log(theRast)` with breaks at c(0.1, 0.5, 1, 2, 4, 8, 16) mg/m^3
- Coordinate system: WGS84 (EPSG:4326) for satellite data, relabelled to GDA2020 (EPSG:7844) for consistency with coastline shapefiles
- Multi-scale analysis: regional extent (±1 degree ~111km) vs local/attack area (±0.2 degree ~22km)
- Named geographic extents defined in `usefulExtents.R` (AU, NSW, WA, etc.)

### Input Data

- `Inputs/Australian Shark-Incident Adjusted.csv`: Primary shark incident database
- `Inputs/IMOS/`: IMOS satellite data file listings
- `Inputs/AUS_2021_AUST_SHP_GDA2020/`: Australian boundary shapefiles
- Satellite data fetched on-demand from IMOS/AODN THREDDS servers and cached locally
