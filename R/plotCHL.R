library(tidyverse)
library(terra)
library(tidyterra)

library(RUtils)

source(codeFile("downloadCHLFiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("usefulExtents.R"))
source(codeFile("chartCHL.R"))

{
  daysPrior <- 5
  theDate <- today()
  geoScope <- "NSW"

  # Download and list available files
  downloadCHLFilesDate(theDate, daysPrior = daysPrior)
  chlFiles <- imosCHLFiles()

  filesOfInterest <- chlFiles |>
    filter(StartDate %within% interval(theDate - days(daysPrior), theDate))

  lastSatPass <- max(filesOfInterest$StartDate)
  firstSatPass <- min(filesOfInterest$StartDate)

  # Load raster, crop, average
  chlRast <- rast(filesOfInterest$FullPath)
  chlRast <- crop(chlRast, getExtent(geoScope))
  chlRast <- mean(chlRast, na.rm = TRUE)
  crs(chlRast) <- "EPSG:7844"

  # Build chart
  theTitle <- paste(format(lastSatPass, "%d %b %Y"), geoScope, "Ocean Chlorophyll")
  theSubTitle <- paste("Prior", daysPrior, "days average of CHL concentration. GSM Model\nLast satellite pass:",
                       format(lastSatPass, "%Y-%m-%d %Z."), "No data in grey")

  chlChart <- chartCHL(chlRast, theTitle, theSubTitle, squishTo = 80, useLog = TRUE)
}

chlChart

saveVertChart(paste0(geoScope, "-CHL-", daysPrior), chlChart, theDate = lastSatPass, onlyLatest = FALSE)
