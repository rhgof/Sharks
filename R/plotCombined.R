library(tidyverse)
library(patchwork)
library(RUtils)

source(codeFile("rastUtilities.R"))
source(codeFile("chartCHL.R"))
source(codeFile("chartSST.R"))
source(codeFile("coastsNcities.R"))

city = "Sydney"
rangeDegrees = 3
nDays = 8
lat = filter(ozCities,name==city)$lat
lon = filter(ozCities,name==city)$long

theDate = today()-days(16)
theDate = ymd("2025-11-27")


chlRast <- chlRasterDateLocation(theDate,lat,lon, daysPrior = nDays, theDegrees = rangeDegrees)
chlUnit = units(chlRast)[1]
chlRast
chlDays = nlyr(chlRast)

chlRast <- scales::oob_squish(chlRast,range=c(0,20))
chlRast <- mean(chlRast,na.rm=TRUE)

chlMin = global(chlRast,"min",na.rm=TRUE)[[1]]
chlMean = global(chlRast,"mean",na.rm=TRUE)[[1]]
chlMax= global(chlRast,"max",na.rm=TRUE)[[1]]


sstRast <- sstRasterDateLocation(theDate, lat, lon, daysPrior=nDays, theDegrees = rangeDegrees)
sstRast <- sstRast - 273.15
sstDays = nlyr(sstRast)

sstRast
sstRast <- scales::squish(sstRast,range = c(15,28))
sstRast <- mean(sstRast,na.rm=TRUE)
sstMin = global(sstRast,"min",na.rm=TRUE)[[1]]
sstMean = global(sstRast,"mean",na.rm=TRUE)[[1]]
sstMax= global(sstRast,"max",na.rm=TRUE)[[1]]


theTitle = paste(format(theDate,"%d %b '%y"),str_to_title(city),"Ocean Chlorophyll")
theSubTitle = paste("Prior",nDays,"days average","Concentration mg/m^3: Mean",round(chlMean,1),"Max",round(chlMax,1),"Min",round(chlMin,1))

chlChart <- chartCHL(chlRast,theTitle, theSubTitle,useLog = TRUE)
chlChart
saveSquareChart(str_replace_all(paste(today(),city,"CHL")," ","_"),chlChart)


set.names(sstRast,"sea_surface_temperature")
theTitle = paste(format(theDate,"%d %b '%y"),str_to_title(city),"Sea Surface Temperature")
theSubTitle = paste("Prior",nDays,"days average","Temperature \u00B0C: Mean",round(sstMean,1),"Max",round(sstMax,1),"Min",round(sstMin,1))

sstChart <- chartSST(sstRast,theTitle,theSubTitle,theRange = c(15,28))
sstChart
saveSquareChart(str_replace_all(paste(today(),city,"SST")," ","_"),sstChart)



#city=""
city = "NSW"
combinedChart <- sstChart + chlChart
combinedChart

saveFile <- paste0(str_replace_all(paste(today(),city,"Combined-SST-CHL")," ","_"))
saveHorizChart(saveFile,combinedChart)
