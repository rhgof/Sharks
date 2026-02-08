library(tidyverse)
library(terra)
library(tidyterra)

library(RUtils)

source(codeFile("downloadSSTFiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("usefulExtents.R"))
source(codeFile("chartSST.R"))

{
  daysPrior <- 5
  theDate <- today()
  geoScope <- "NSW"

  # Download and list available files
  downloadSSTFilesDate(theDate, daysPrior = daysPrior, period = "3d")
  sstFiles <- imosSSTFiles()

  filesOfInterest <- sstFiles |>
    filter(StartDate %within% interval(theDate - days(daysPrior), theDate))

  lastSatPass <- max(filesOfInterest$StartDate)
  firstSatPass <- min(filesOfInterest$StartDate)

  # Load raster, extract SST layer, crop, transform, average
  sstRast <- rast(filesOfInterest$FullPath)
  sstRast <- sstRast["sea_surface_temperature"]
  sstRast <- crop(sstRast, getExtent(geoScope))
  sstRast <- mean(sstRast, na.rm = TRUE)
  names(sstRast) <- "sea_surface_temperature"
  crs(sstRast) <- "EPSG:7844"
  sstRast <- sstRast - 273.15

  # Build chart
  theTitle <- paste(format(lastSatPass, "%d %b %Y"), geoScope, "Sea Surface Temperature")
  theSubTitle <- paste("Single-sensor multi-satellite SST for prior", daysPrior, "day 24 hour average.\nLast satellite pass:",
                       format(lastSatPass, "%Y-%m-%d %Z."), "No data in grey.")

  sstChart <- chartSST(sstRast, theTitle, theSubTitle, theRange = c(10, 30))
}

sstChart

saveVertChart(paste0(geoScope, "-SST-", daysPrior), sstChart, theDate = lastSatPass, onlyLatest = FALSE)
