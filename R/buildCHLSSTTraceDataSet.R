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
  filter(year(Incident.Date) %in% c(2015:2026))

# get and cache all the data
for (i in 1:nrow(theIncidents)) {
  incident <- theIncidents[i,]

  downloadSSTFilesDate(incident$Incident.Date,daysPrior = 30)
  downloadCHLFilesDate(incident$Incident.Date,daysPrior = 30)
}


rangeDegrees=1
dataDegrees = 0.2

i=1
attackProfiles = NULL
traceDays = 30
for (i in 1:nrow(theIncidents)) {
#for (i in 1:10) {
  inc = theIncidents[i,]
  print(paste(inc$UIN, inc$Incident.Date,inc$Location))
  theDate = inc$Incident.Date
  lat = inc$Latitude
  lon = inc$Longitude

  for (d in seq.Date(theDate-days(traceDays),theDate,by = "day")) {
    traceDate = as_date(d)
    print(traceDate)
    chlRast <- chlRasterDateLocation(traceDate,lat,lon, daysPrior = 3, theDegrees = rangeDegrees)
    chlUnit = units(chlRast)[1]
    chlRast
    chlDays = nlyr(chlRast)
    chlRast <- mean(chlRast,na.rm=TRUE)

    chlMin = global(chlRast,"min",na.rm=TRUE)[[1]]
    chlMean = global(chlRast,"mean",na.rm=TRUE)[[1]]
    chlMax= global(chlRast,"max",na.rm=TRUE)[[1]]

    chlRastAttack = cropToArea(chlRast,lat,lon,degrees = dataDegrees)
    chlAttackMin = global(chlRastAttack,"min",na.rm=TRUE)[[1]]
    chlAttackMean = global(chlRastAttack,"mean",na.rm=TRUE)[[1]]
    chlAttackMax= global(chlRastAttack,"max",na.rm=TRUE)[[1]]


    sstRast <- sstRasterDateLocation(traceDate, lat, lon, daysPrior=3, theDegrees = rangeDegrees)
    sstRast <- sstRast - 273.15
    sstDays = nlyr(sstRast)

    sstRast
    sstRast <- mean(sstRast,na.rm=TRUE)

    sstMin = global(sstRast,"min",na.rm=TRUE)[[1]]
    sstMean = global(sstRast,"mean",na.rm=TRUE)[[1]]
    sstMax= global(sstRast,"max",na.rm=TRUE)[[1]]

    sstRastAttack = cropToArea(sstRast,lat,lon,degrees = dataDegrees)
    sstAttackMin = global(sstRastAttack,"min",na.rm=TRUE)[[1]]
    sstAttackMean = global(sstRastAttack,"mean",na.rm=TRUE)[[1]]
    sstAttackMax= global(sstRastAttack,"max",na.rm=TRUE)[[1]]

    attackProfile = tribble(
      ~UIN, ~IncidentDate, ~State, ~Location, ~Lat, ~Lon, ~SharkCommonName,
      ~CHL_min, ~CHL_max, ~CHL_mean, ~CHLAttack_min, ~CHLAttack_max, ~CHLAttack_mean, ~CHL_unit, ~CHL_days,
      ~SST_min, ~SST_max, ~SST_mean, ~SSTAttack_min, ~SSTAttack_max, ~SSTAttack_mean, ~SST_Unit, ~SST_days, ~RangeDegrees, ~DataDegrees, ~TraceDate,
      inc$UIN, theDate, inc$State, inc$Location, lat, lon, inc$Shark.common.name,
      chlMin, chlMax, chlMean, chlAttackMin, chlAttackMax, chlAttackMean, chlUnit, chlDays,
      sstMin,sstMax,sstMean,sstAttackMin, sstAttackMax, sstAttackMean,"\u00B0C", sstDays,rangeDegrees, dataDegrees, traceDate )

    attackProfiles <- attackProfiles |>
      bind_rows(attackProfile)
  }
}

write_csv(attackProfiles,inputFile(paste0("SharkIncidents-SST-SHL-Traces",rangeDegrees,"Deg.csv")))
