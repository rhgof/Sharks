library(tidyverse)
library(terra)

cropToArea <- function(theRaster,lat,long, degrees = 1) {
  theArea <- ext(long-degrees,long+degrees,lat-degrees,lat+degrees)

  theUnits = units(theRaster)
  croppedRast <-crop(theRaster,theArea)
  units(croppedRast) <- theUnits

  return(croppedRast)

}

