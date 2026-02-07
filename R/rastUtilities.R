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


chlRasterDateLocation <- function(theDate,lat,lon, daysPrior = 5, theDegrees = 1) {

  downloadCHLFilesDate(theDate,daysPrior)
  theFiles <- imosCHLFiles()

  repeat {
    filesOfInterest <- theFiles |>
      filter(StartDate %within%  interval(theDate-days(daysPrior),theDate ))
    if (nrow(filesOfInterest) > 0 ) break
    daysPrior = daysPrior+1
    print(c("CHL Extending",daysPrior))
  }

  theRast = rast(filesOfInterest$FullPath)

  theRast <- cropToArea(theRast,lat,lon,theDegrees)
  theRast

  return (theRast)
}


sstRasterDateLocation <- function(theDate,lat,lon, daysPrior = 1,theDegrees = 1) {

  downloadSSTFilesDate(theDate,daysPrior)
  theFiles <- imosSSTFiles()

  repeat {
    filesOfInterest <- theFiles |>
      filter(StartDate %within%  interval(theDate-days(daysPrior),theDate ))
    if (nrow(filesOfInterest) > 0 ) break
    daysPrior = daysPrior+1
    print(c("SST Extending",daysPrior))
  }

  theRast = rast(filesOfInterest$FullPath)

  sstName = "sea_surface_temperature"
  sstRast <- theRast[sstName]

  sstRast<-cropToArea(sstRast,lat,lon, theDegrees)
  sstRast

  return (sstRast)
}
