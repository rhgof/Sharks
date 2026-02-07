library(tidyverse)
library(readr)
library(terra)
library(tidyterra)
library(sf)

library(RUtils)
source(codeFile("downloadSSTFiles.R"))
source(codeFile("readIMOSFileListing.R"))

source(codeFile("chartSST.R"))

{
  daysPrior = 2

  theDate = ymd("25-11-27")
  geoScope = "NSW"
  downloadSSTFilesDate(theDate,daysPrior=daysPrior,period="3d")
  sstFiles <- imosSSTFiles()

#  theDate = min(today(),lastSatDate)

  filesOfInterest <- sstFiles |>
    filter(StartDate %within%  interval(theDate-days(daysPrior),theDate ))


  lastSatPass = max(filesOfInterest$StartDate)
  firstSatPass = min(filesOfInterest$StartDate)

  allSSTRast <-rast(filesOfInterest$FullPath)
  allSSTRast
  names(allSSTRast)
  sstRast <- allSSTRast["sea_surface_temperature"]

  sstRast <- mean(sstRast,na.rm = TRUE )
  names(sstRast) <- "sea_surface_temperature"

  sstRast <-crop(sstRast,getExtent(geoScope))
  sstRast <- project(sstRast,GDA2020_CRS) #Australia GDA2020
  sstRast <- sstRast - 273.15

  theTitle = paste(format(lastSatPass,"%d %b %Y"), "NSW", "Sea Surface Temperature")
  theSubStitle = paste("Single-sensor multi-satellite SST for prior",daysPrior,"day 24 hour average.\nLast satellite pass:", format(lastSatPass,"%Y-%m-%d %Z."),"No data in grey.")

  sstChart <- chartSST(sstRast,theTitle,theSubStitle,theRange = c(10,30))
}
sstChart <- sstChart + annotate(geom="point", y=-31.8464,x= 152.7489,color="red",size=2)

sstChart

saveVertChart(paste0(geoScope,"-SST-",daysPrior),sstChart,theDate = lastSatPass, onlyLatest=FALSE)

