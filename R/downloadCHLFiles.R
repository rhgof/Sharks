library(tidyverse)

library(RUtils)

source(codeFile("readSharkDB.R"))
source(codeFile("getnetcdfFile.R"))

# https://portal.aodn.org.au/search

# File A.P1D.20220201T053000Z.aust.chl_gsm.nc (NC_FORMAT_NETCDF4_CLASSIC):
#
#   1 variables (excluding dimension variables):
#   float chl_gsm[longitude,latitude,time]   (Chunking: [2001,1401,1])  (Compression: shuffle,level 4)
# _FillValue: -999
# units: mg/m^3
# long_name: Chlorophyll Concentration, GSM model
# flag_applied: HISATZEN LAND CLDICE NAVFAIL
#
# 3 dimensions:
#   time  Size:1
# long_name: time
# standard_name: time
# units: days since 1990-01-01 00:00:00
# calendar: gregorian
# axis: T
# latitude  Size:7001
# long_name: latitude
# standard_name: latitude
# units: degrees_north
# axis: Y
# longitude  Size:10001
# long_name: longitude
# standard_name: longitude
# units: degrees_east
# axis: X
#
# 3 global attributes:
#  history: File initialised at 2022-02-17T17:21:07.389473

downloadCHLFiles <- function(yearsOfInterest = c(2002:2023),incidentsOfInterest = c(1:2000),theStateAbr = "NSW", daysPrior = 8) {
  sharkIncidents <- readSharkDB()

  earliestIMOSDate <- ymd("2002-07-04")

  recentIncidents <- sharkIncidents |>
    filter(Incident.Date >= earliestIMOSDate) |>
    filter(Shark.common.name == "white shark") |>
    filter(State == theStateAbr) |>
    filter( year(Incident.Date) %in% yearsOfInterest) |>
    filter(UIN %in% incidentsOfInterest)

  for (i in 1:nrow(recentIncidents)) {
    incident = recentIncidents[i,]
    downloadCHLFilesDate(incident$Incident.Date,daysPrior)
  }
}

# get netcdf files for the date and daysPrior days

downloadCHLFilesDate <-function(theDate, daysPrior = 8) {

  fileTemplate <- "https://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/OC/gridded/snpp/P1D/<YEAR>/<MONTH>/V.P1D.<YEAR><MONTH><DAY>T053000Z.aust.chl_gsm.nc"
  #fileTemplate <- "https://imos-data.s3-ap-southeast-2.amazonaws.com/IMOS/SRS/OC/gridded/aqua/P1D/<YEAR>/<MONTH>/A.P1D.<YEAR><MONTH><DAY>T053000Z.aust.chl_gsm.nc"

  for (i in c(0:daysPrior)) {
    downloadDate = theDate - days(i)

    y = format(downloadDate,"%Y")
    m = format(downloadDate,"%m")
    d = format(downloadDate,"%d")
    ncURL = str_replace_all(fileTemplate,"<YEAR>",y)
    ncURL = str_replace_all(ncURL,"<MONTH>",m)
    ncURL = str_replace_all(ncURL,"<DAY>",d)
    getNetcdfFileName(ncURL)
  }
}
