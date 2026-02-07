library(tidyverse)
library(scales)
library(terra)
library(viridis)
library(ncdf4)

library(RUtils)

source(codeFile("readSharkDB.R"))
source(codeFile("downloadIMOSfiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("getnetcdfFile.R"))
source(codeFile("getIncidentRaster.R"))
source(codeFile("animateData.R"))

# https://portal.aodn.org.au/search

# File /Volumes/Samples/InputData/cache/A.P1D.20220201T053000Z.aust.chl_gsm.nc (NC_FORMAT_NETCDF4_CLASSIC):
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

nop <-function(x) { return (x)}

nswExt <- ext(149.469,156.061,-37.893,-27.961)
waExtent <- ext(110,123,-35.5,-25)
cropToExt <- nswExt

auShpFile = inputFile("AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
auVect <-  vect(auShpFile)

sharkIncidents <- readSharkDB()

cntByYear <- sharkIncidents |>
  group_by(State,Incident.year) |>
  filter(Shark.common.name == "white shark") |>
    filter(State == "NSW") |>
  arrange(Incident.Date) |>
  add_count(Incident.Date) |>
  mutate(cumIncidents = cumsum(n)) |>

  ungroup() |>
  nop()

ggplot(cntByYear,aes(x=yday(Incident.Date),y=cumIncidents,group=`Incident.year`,color=`State`)) + geom_line()

cntByYear <- sharkIncidents |>
  group_by(State,Incident.year) |>
  count(Incident.Date)

ggplot(cntByYear,aes(x=Incident.year,y=n,fill=`State` )) +
  geom_col() +
  customTheme()


yearsOfInterest = c(2000:2023)
incidentsOfInterest = c(1:2000)

daysPrior = 8

downloadIMOSfiles(yearsOfInterest,incidentsOfInterest,daysPrior)


# NSW Coast Extent
# Lat/Lon -37.893, 149.469 to Lat/Lon -27.961, 156.061


#---- IMOS Files ----
ncdfFiles <- imosFiles()

earliestSatData <- unique(slice_min(ncdfFiles,order_by = StartDate)$StartDate)

SatPeriod = "DAY"
SatChannel = "CHL"
SatMethod = "GSM"

recentIncidents <- sharkIncidents |>
  filter(Incident.Date >= earliestSatData) |>
  filter(Shark.common.name == "white shark") |>
  filter(State == "NSW") |>
  filter( year(Incident.Date) %in% yearsOfInterest) |>
  filter(UIN %in% incidentsOfInterest)

recentIncidents <- slice_tail(recentIncidents)

netcdfFilesOfInterest <- ncdfFiles |>
  filter(MethodShort == SatMethod) |>
  filter(Period == SatPeriod) |>
  filter(Channel == SatChannel)



useLog = FALSE

for (i in 1:nrow(recentIncidents) ) {
  incident = recentIncidents[i,]

  incidentDate = incident$Incident.Date
  incidentFiles <- netcdfFilesOfInterest |>
    filter(StartDate %within% interval(incidentDate-days(daysPrior),incidentDate))

  if (nrow(incidentFiles) > 1) {
  # Raster with layer per day
    incidentRast = rast(incidentFiles$FullPath)
    incidentRast <-crop(incidentRast,cropToExt)
  # mean across days
    incidentRast <- mean(incidentRast,na.rm=TRUE)
    incidentRast <- scales::squish(incidentRast,range = c(0,15))
    incidentRast

#    incidentRast <- min(incidentRast,exp(minScale),na.rm=TRUE)
#    incidentRast <- max(incidentRast,exp(maxScale),na.rm=TRUE)


    #incidentExt<-ext(incident$Longitude-1,incident$Longitude+1,incident$Latitude-1,incident$Latitude+1)
    #incidentRast <-crop(incidentRast,incidentExt)

    if (!is.null(incidentRast)) {

      if (useLog) {
        chlRange = log(c(0.01,4.5))
        rastToPlot = log(incidentRast)
      } else {
        chlRange = (c(0.01,4.5))
        rastToPlot = incidentRast
      }

      saveFileName = str_replace_all(paste("IMOS",incidentDate,incident$UIN,incident$Location,incident$Shark.common.name,paste0("mean-",daysPrior),SatPeriod,SatChannel,SatMethod,".png",sep="_")," ","-")
      png(outputFile(saveFileName),width = 550,height=960)
      plot(rastToPlot,range = chlRange,col=hcl.colors(n=100, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE))
      lines(auVect,lwd=0.1,col="grey30")
      points(vect(incident,geom=c("Longitude","Latitude")),col="red",lwd=1,cex = 2)
      title(paste(incident$Incident.Date,incident$UIN,incident$Location,incident$Shark.common.name,daysPrior,SatPeriod,SatChannel,SatMethod),cex.main=0.8)
      dev.off()
    } else {
    print(paste("Problem",incident$Incident.Date,incident$UIN,incident$Location,incident$Longitude,incident$Latitude,incident$Shark.common.name))
    }
  }
}


incidentRast<-cropToArea(rastToPlot,incident$Latitude,incident$Longitude)

rastToPlot <- incidentRast

dfRast <- as.data.frame(rastToPlot,xy=TRUE)
#frequency of value
ggplot(data = dfRast) + geom_bar(aes(round(`mean`,digits=2))) #geom_bar(aes(round(scales::censor(mean,range = c(0,15)),digits=1)))

ggplot(dfRast,aes(x=x,y=y,fill=scales::censor(mean,range = c(0,15)))) +
  geom_tile() +
  geom_point(mapping = aes(x=incident$Longitude,y=incident$Latitude), color = "black") +
  scale_fill_gradientn(
    colors = hcl.colors(n=50, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE),
                       #trans="log",
                       limits = c(0.001,4.5),
                       breaks = c(0.05,0.5,1,2,4),
    guide = guide_colorbar(title = "mg/m^3"),
#                       oob = scales::censor(range = c(0.05,4.5)),
                       ) +

  #annotate(geom = "text",x=incident$Longitude,y = incident$Latitude, label = incident$Location, color = "white") +
  scale_x_continuous(expand = expansion(add=0)) +
  scale_y_continuous(expand = expansion(add=0)) +
  xlab("") + ylab("") +
  ggtitle(paste(incident$Incident.Date, str_to_title(incident$Location),incident$State,"-", str_to_title(incident$Shark.common.name))) +

  coord_quickmap() +
  customTheme() +
  theme(
    panel.background = element_rect(fill = "grey60"),
    panel.grid = element_blank(),
    #plot.background = element_rect(fill = "grey60")
  )

animateFiles(paste0("^IMOS.*",daysPrior,"_DAY.*.png"),w=550,h=960)
