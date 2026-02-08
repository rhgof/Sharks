library(tidyverse)
library(RUtils)
library(terra)

source(codeFile("downloadCHLFiles.R"))
source(codeFile("downloadSSTFiles.R"))
source(codeFile("readIMOSFileListing.R"))

cropToArea <- function(theRaster,lat,long, degrees = 1) {
  theArea <- ext(long-degrees,long+degrees,lat-degrees,lat+degrees)

  theUnits = units(theRaster)
  croppedRast <-crop(theRaster,theArea)
  units(croppedRast) <- theUnits

  return(croppedRast)

}


chlRasterDateLocation <- function(theDate, lat, lon, daysPrior = 5, theDegrees = 1, maxDaysPrior = 30) {

  downloadCHLFilesDate(theDate, daysPrior)
  theFiles <- imosCHLFiles()

  repeat {
    filesOfInterest <- theFiles |>
      filter(StartDate %within% interval(theDate - days(daysPrior), theDate))
    if (nrow(filesOfInterest) > 0) break
    daysPrior <- daysPrior + 1
    print(c("CHL Extending", daysPrior))
    if (daysPrior > maxDaysPrior) {
      warning(paste("No CHL data found within", maxDaysPrior, "days of", theDate))
      return(NULL)
    }
  }

  theRast <- rast(filesOfInterest$FullPath)
  theRast <- cropToArea(theRast, lat, lon, theDegrees)

  return(theRast)
}


sstRasterDateLocation <- function(theDate, lat, lon, daysPrior = 1, theDegrees = 1, maxDaysPrior = 30) {

  downloadSSTFilesDate(theDate, daysPrior)
  theFiles <- imosSSTFiles()

  repeat {
    filesOfInterest <- theFiles |>
      filter(StartDate %within% interval(theDate - days(daysPrior), theDate))
    if (nrow(filesOfInterest) > 0) break
    daysPrior <- daysPrior + 1
    print(c("SST Extending", daysPrior))
    if (daysPrior > maxDaysPrior) {
      warning(paste("No SST data found within", maxDaysPrior, "days of", theDate))
      return(NULL)
    }
  }

  theRast <- rast(filesOfInterest$FullPath)

  sstName <- "sea_surface_temperature"
  sstRast <- theRast[sstName]
  sstRast <- cropToArea(sstRast, lat, lon, theDegrees)

  return(sstRast)
}
