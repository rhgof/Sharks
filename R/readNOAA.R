library(tidyverse)

library(ncdf4)

library(terra)
library(tidyterra)

source(codeFile("usefulextents.R"))
library(RUtils)

# Baseline Data
# https://www.nodc.noaa.gov/OC5/woa13/
# https://polar.ncep.noaa.gov/global/about/product_description.shtml
# https://polar.ncep.noaa.gov/global/about/regional_description.shtml

noaaFileRemote <- "https://nomads.ncep.noaa.gov/pub/data/nccf/com/rtofs/prod/rtofs.20231108/rtofs_glo_2ds_f000_prog.nc"





noaaFile <- readCachedFile(noaaFileRemote)

auExt <- getExtent("AU")
terra::describe(noaaFile,sds=TRUE)


ncdf <- nc_open(noaaFile)

noaaRast <- rast(noaaFile,"sst")
noaaRast
plot(noaaRast,col=hcl.colors(n=100, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE))

names(noaaRast)
