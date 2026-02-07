library(tidyverse)
library(terra)
library(ncdf4)

library(RUtils)

getIncidentRaster <- function(incident,nc, channel = "CHL1") {

  varName <- paste0(channel,"_mean")
  print(varName)

  chlName <- ncvar_get(nc,varid = varName)
  #chl_A_flags <- ncvar_get(nc,varid = "CHL1_flags")
  #lat <- ncvar_get(nc,varid = "lat")
  #lon <- ncvar_get(nc,varid = "lon")
  #nlat <- dim(lat)
  #nlon <- dim(lon)

  atts <- ncatt_get(nc,varid = 0)

  # Set-up lat and longitudes in Raster

  east = atts$easternmost_longitude
  west = atts$westernmost_longitude
  north = atts$northernmost_latitude
  south = atts$southernmost_latitude
  #lonStep = atts$lon_step
  #latStep = atts$lat_step

  extNC = ext(west,east,south,north)

  ncRast <- rast(log(chlName))
  ncRast<-trans(ncRast)
  ext(ncRast) <- extNC
  crs(ncRast) <- "EPSG:4326"

  # crop else return NA
  incidentExt<-ext(incident$Longitude-1,incident$Longitude+1,incident$Latitude-1,incident$Latitude+1)

  cropRast = NULL
  if (relate(extNC,incidentExt,relation = "contains")[[1]] == TRUE ) {
    cropRast <- crop(ncRast,incidentExt)
  }

  return(cropRast)

}
