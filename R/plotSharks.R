library(tidyverse)
library(terra)
#library(raster)
library(RColorBrewer)
library(viridis)
library(ncdf4)

library(RUtils)

source(codeFile("readSharkDB.R"))
source(codeFile("readChlAFiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("getIncidentRaster.R"))

auShpFile = inputFile("AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
auVect <-  vect(auShpFile)


nswExt <- ext(149.469,156.061,-37.893,-27.961)

sharkIncidents <- readSharkDB()

#---- Hermes -----

ncdfFiles <- ncFiles()

earliestSatData <- unique(slice_min(ncdfFiles,order_by = StartDate)$StartDate)

recentIncidents <- sharkIncidents |>
  filter(Incident.Date >= earliestSatData) |>
  filter(State == "NSW") |>
  filter(Shark.common.name != "wobbegong")


#recentIncidents <- slice_tail(recentIncidents,n=10)

SatPeriod = "MO"
SatChannel = "CHL"
SatMethod = "GSM"

for (i in 1:nrow(recentIncidents)) {
  incident = recentIncidents[i,]

  netcdfRecord <- ncdfFiles |>
    filter(incident$Incident.Date %within% Interval) |>
    filter(MethodShort == SatMethod) |>
    filter(Period == SatPeriod) |>
    filter(Channel == SatChannel)

  if (count(netcdfRecord) == 0) {
    stop("No files Match")
  }

  #print(paste(i,netcdfRecord$FileName))

  netcdfFile <- paste(netcdfRecord$Path,netcdfRecord$FileName,sep="/")


  #nc <- nc_open(netcdfFile)
  #incidentRast <- getIncidentRaster(incident,nc,channel = SatChannel)

  incidentRast <- rast(netcdfFile)
  incidentRast <-crop(incidentRast$CHL1_mean,nswExt)


  if (!is.null(incidentRast)) {

    saveFileName = str_replace_all(paste("HERMES",incident$Incident.Date,incident$UIN,incident$Location,incident$Shark.common.name,SatPeriod,SatChannel,SatMethod,".png",sep="_")," ","-")
    png(outputFile(saveFileName))
    plot(log(incidentRast),range = c(-3,3),col=hcl.colors(n=25, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE))
    lines(auVect,lwd=0.5,fg="grey40")
    points(vect(incident,geom=c("Longitude","Latitude")),col="red",lwd=2,cex = 2)
    title(paste(incident$Incident.Date,incident$UIN,incident$Location,incident$Shark.common.name,SatPeriod,SatChannel,SatMethod),cex.main=0.8)
    dev.off()
  } else {
    print(paste("Problem",incident$Incident.Date,incident$UIN,incident$Location,incident$Longitude,incident$Latitude,incident$Shark.common.name))
  }


}





#---------------------

theDate = ymd("2023-07-01")

netcdfRecord <- ncdfFiles |>
  filter(theDate %within% Interval) |>
  filter(MethodShort == "GSM")

if (count(netcdfRecord) == 0) {
  stop("No files Match")
}

netcdfFile <- paste(netcdfRecord$Path,netcdfRecord$FileName,sep="/")
titleChart <- netcdfRecord$Title


nc <- nc_open(netcdfFile)
#nc

chl_A_mean <- ncvar_get(nc,varid = "CHL1_mean")
chl_A_flags <- ncvar_get(nc,varid = "CHL1_flags")
lat <- ncvar_get(nc,varid = "lat")
lon <- ncvar_get(nc,varid = "lon")
nlat <- dim(lat)
nlon <- dim(lon)

#rastCHl <- raster(netcdFile,band=1,nrows = nlat,ncols = nlon )

minChlA <- min(chl_A_mean[1:nlon,1:nlat],na.rm = TRUE)
maxChlA <- max(chl_A_mean[1:nlon,1:nlat],na.rm = TRUE)
dim(chl_A_mean)


atts <- ncatt_get(nc,varid = 0)


# Set-up lat and longitudes in Raster

east = atts$easternmost_longitude
west = atts$westernmost_longitude
north = atts$northernmost_latitude
south = atts$southernmost_latitude
lonStep = atts$lon_step
latStep = atts$lat_step

extR = ext(west,east,south,north)

auShpFile = inputFile("AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
auVect <-  vect(auShpFile)



rA <- rast(log(chl_A_mean))
rA<-trans(rA)
ext(rA) <- extR
crs(rA) <- "EPSG:4326"
rA


#image(rA,col=viridis(n=20,direction=-1,alpha=0.5))
#plot(rA,col=terrain.colors(n=30,rev=TRUE))
#plot(rA,col=brewer.pal(n=9,"Spectral"))
plot(rA,col=hcl.colors(n=25, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE)     )

#au <- map(regions = "australia",plot=FALSE,resolution = 0)
#lines(au)

plot(auVect,add=TRUE,lwd=0.5,fg=gray(0.2))
te <- extR
theTitle <- paste(titleChart,
                  "(lon:",
                  round(ext(te)$xmin,digits=2),
                  round(ext(te)$xmax,digits=2),
                  "lat:",
                  round(ext(te)$ymin,digits=2),
                  round(ext(te)$ymax,digits=2),
                  ")")
title(theTitle,cex.main=1)

# test
portPhilipBayExt<-ext(144.857966-1,144.857966+1,-38.075950-1,-38.075950+1)

ppbR <- crop(rA,portPhilipBayExt)
te <- portPhilipBayExt
theTitle <- paste(titleChart,
                  "(lon:",
                  round(ext(te)$xmin,digits=2),
                  round(ext(te)$xmax,digits=2),
                  "lat:",
                  round(ext(te)$ymin,digits=2),
                  round(ext(te)$ymax,digits=2),
                  ")")
#png(paste0("Melb-",StartDate,"-",Gridkm,"-",Period,".png"),)
plot(ppbR,col=hcl.colors(n=25, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE))
title(theTitle,cex.main=1)
lines(auVect,lwd=0.5,fg="grey40")
#dev.off()

#Sydney
sydneyExt <- ext(151.325958-1,151.325958+1,-33.835111-1,-33.835111+1)

png(paste0("Sydney-",StartDate,"-",Gridkm,"-",Period,".png"),)
sydR <- crop(rA,sydneyExt)
te <- sydneyExt
theTitle <- paste(titleChart,
                    "(lon:",
                    round(ext(te)$xmin,digits=2),
                    round(ext(te)$xmax,digits=2),
                    "lat:",
                    round(ext(te)$ymin,digits=2),
                    round(ext(te)$ymax,digits=2),
                    ")")

plot(sydR,col=hcl.colors(n=25, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE))
title(theTitle,cex.main=1)
lines(auVect,lwd=0.5,fg="grey40")
dev.off()

