library(tidyverse)
library(patchwork)
library(terra)

library(RUtils)

source(codeFile("readSharkDB.R"))
source(codeFile("downloadCHLFiles.R"))
source(codeFile("downloadSSTFiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("usefulExtents.R"))
source(codeFile("rastUtilities.R"))
source(codeFile("chartSST.R"))
source(codeFile("chartCHL.R"))

sharkIncidents <- readSharkDB()

earliestSatDate <- ymd("2002-07-01")

theIncidents <- sharkIncidents |>
  filter(Incident.Date >= earliestSatDate) |>
  filter(State == "NSW") |>
  filter(Shark.common.name == "white shark")


theIncidents <- theIncidents |>
  filter(UIN %in% c(1193))
# get and cache all the data
for (i in 1:nrow(theIncidents)) {
  incident <- theIncidents[i,]

  downloadSSTFilesDate(incident$Incident.Date,daysPrior = 3)
  downloadCHLFilesDate(incident$Incident.Date,daysPrior = 6)
}


rangeDegrees=1
dataDegrees = 0.2

i=64
attackProfiles = NULL
for (i in 1:nrow(theIncidents)) {
#for (i in 1:10) {
  inc = theIncidents[i,]
  inc
  print(inc)
  theDate = inc$Incident.Date
  lat = inc$Latitude
  lon = inc$Longitude

  chlRast <- chlRasterDateLocation(theDate,lat,lon, daysPrior = 6, theDegrees = rangeDegrees)
  chlUnit = units(chlRast)[1]
  chlRast
  chlDays = nlyr(chlRast)
  chlRast <- mean(chlRast,na.rm=TRUE)

  chlMin = global(chlRast,"min",na.rm=TRUE)[[1]]
  chlMean = global(chlRast,"mean",na.rm=TRUE)[[1]]
  chlMax= global(chlRast,"max",na.rm=TRUE)[[1]]


  sstRast <- sstRasterDateLocation(theDate, lat, lon, daysPrior=3, theDegrees = rangeDegrees)
  sstRast <- sstRast - 273.15
  sstDays = nlyr(sstRast)

  sstRast
  sstRast <- mean(sstRast,na.rm=TRUE)


  sstMin = global(sstRast,"min",na.rm=TRUE)[[1]]
  sstMean = global(sstRast,"mean",na.rm=TRUE)[[1]]
  sstMax= global(sstRast,"max",na.rm=TRUE)[[1]]


  chlRastAttack = cropToArea(chlRast,lat,lon,degrees = dataDegrees)
  chlAttackMin = global(chlRastAttack,"min",na.rm=TRUE)[[1]]
  chlAttackMean = global(chlRastAttack,"mean",na.rm=TRUE)[[1]]
  chlAttackMax= global(chlRastAttack,"max",na.rm=TRUE)[[1]]

  sstRastAttack = cropToArea(sstRast,lat,lon,degrees = dataDegrees)
  sstAttackMin = global(sstRastAttack,"min",na.rm=TRUE)[[1]]
  sstAttackMean = global(sstRastAttack,"mean",na.rm=TRUE)[[1]]
  sstAttackMax= global(sstRastAttack,"max",na.rm=TRUE)[[1]]

  theTitle = paste(inc$Incident.Date,str_to_title(inc$Location))
  theSubTitle = paste("Chlorophyll Concentration mg/m^3: Mean",round(chlMean,1),"Max",round(chlMax,1),"Min",round(chlMin,1),
                      "\nNear Attack: Mean",round(chlAttackMean,1),"Max",round(chlAttackMax,1),"Min",round(chlAttackMin,1))
  chlChart <- chartCHL(chlRast,theTitle, theSubTitle,useLog = TRUE) +
    geom_point(mapping = aes(x= inc$Longitude,y=inc$Latitude),size=3,color="black") +
    geom_rect(mapping = aes(xmin = lon-dataDegrees,xmax = lon+dataDegrees,ymin=lat-dataDegrees, ymax=lat+dataDegrees),linewidth = 0.1,color="black", fill=NA)
  chlChart


  set.names(sstRast,"sea_surface_temperature")
  theTitle = paste(inc$Incident.Date,str_to_title(inc$Location))
  theSubTitle = paste("Sea Surface Temperature \u00B0C: Mean",round(sstMean,1),"Max",round(sstMax,1),"Min",round(sstMin,1),
                      "\nNear Attack: Mean",round(sstAttackMean,1),"Max",round(sstAttackMax,1),"Min",round(sstAttackMin,1))

  sstChart <- chartSST(sstRast,theTitle,theSubTitle) +
    geom_point(mapping = aes(x= lon, y=lat),size=3,color="black") +
    geom_rect(mapping = aes(xmin = lon-dataDegrees,xmax = lon+dataDegrees,ymin=lat-dataDegrees, ymax=lat+dataDegrees),linewidth = 0.1,color="black", fill=NA)
  sstChart

  combinedChart <- sstChart + chlChart
  combinedChart
  saveFile <- paste0(str_replace_all(paste(inc$Incident.Date,"SST-CHL",paste0(round(rangeDegrees,1),"-degree"),str_replace_all(str_to_title(inc$Location),",",""),sep="_")," ","-"),".png")
  saveHorizChart(saveFile,combinedChart)



  attackProfile = tribble(
    ~UIN, ~IncidentDate, ~State, ~Location, ~Lat, ~Lon, ~SharkCommonName,
    ~CHL_min, ~CHL_max, ~CHL_mean, ~CHLAttack_min, ~CHLAttack_max, ~CHLAttack_mean, ~CHL_unit, ~CHL_days,
    ~SST_min, ~SST_max, ~SST_mean, ~SSTAttack_min, ~SSTAttack_max, ~SSTAttack_mean, ~SST_Unit, ~SST_days, ~RangeDegrees, ~DataDegrees,
    inc$UIN, theDate, inc$State, inc$Location, lat, lon, inc$Shark.common.name,
    chlMin, chlMax, chlMean, chlAttackMin, chlAttackMax, chlAttackMean, chlUnit, chlDays,
    sstMin,sstMax,sstMean,sstAttackMin, sstAttackMax, sstAttackMean,"\u00B0C", sstDays,rangeDegrees, dataDegrees )

  attackProfiles <- attackProfiles |>
    bind_rows(attackProfile)

}

write_csv(attackProfiles,inputFile(paste0("SharkIncidents-SST-SHL-",rangeDegrees,"Deg.csv")))
