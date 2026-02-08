library(tidyverse)
library(scales)
library(terra)
library(tidyterra)
library(viridis)
library(ncdf4)

library(RUtils)

source(codeFile("readSharkDB.R"))
source(codeFile("downloadIMOSfiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("getnetcdfFile.R"))
source(codeFile("getIncidentRaster.R"))
source(codeFile("animateData.R"))
source(codeFile("rastUtilities.R"))


sharkIncidents <- readSharkDB()
auShpFile = inputFile("AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
auVect <-  vect(auShpFile)


yearsOfInterest = c(2016)
incidentsOfInterest = c(1:2000)
sharksOfInterest = c("white shark")
statesOfInterest = c("NSW")

recentIncidents <- sharkIncidents |>
  filter(Shark.common.name %in% sharksOfInterest) |>
  filter(State %in% statesOfInterest) |>
  filter( year(Incident.Date) %in% yearsOfInterest) |>
  filter(UIN %in% incidentsOfInterest)

# get listing of files
SatPeriod = "DAY"
SatChannel = "CHL"
SatMethod = "GSM"


daysPrior = 8
animationDays = 30
for (i in 1:nrow(recentIncidents)) {
  incident = recentIncidents[i,]
  print(incident)
  ds <- seq(from = incident$Incident.Date-days(animationDays), incident$Incident.Date + days(animationDays),by = "day")

  maxChl = 0
  minChl = 0
  for (d in as.list(ymd(ds)) ){
    #print(d)

    downloadIMOSFilesDate(d)
    ncdfFiles <- imosFiles()
    netcdfFilesOfInterest <- ncdfFiles |>
      filter(MethodShort == SatMethod) |>
      filter(Period == SatPeriod) |>
      filter(Channel == SatChannel)
    theFiles <- netcdfFilesOfInterest |>
       filter(StartDate %within% interval(d-days(daysPrior),d))



    incidentRast = rast(theFiles$FullPath)
    incidentRast <-cropToArea(incidentRast,incident$Latitude,incident$Longitude)
    # mean across days

    incidentRast <- mean(incidentRast,na.rm=TRUE)
    maxChl = max(minmax(incidentRast)["max",],maxChl)
    minChl = min(minmax(incidentRast)["min",],minChl)

    incidentRast <- scales::squish(incidentRast,range = c(0.05,25))
    incidentRast


    chlRange = (c(0.05,25))
    rastToPlot = (incidentRast)


    saveFileName = str_replace_all(paste("Area",incident$UIN,d,incident$Incident.Date,incident$Location,incident$Shark.common.name,paste0("mean-",daysPrior),SatPeriod,SatChannel,SatMethod,".png",sep="_")," ","-")
    png(outputFile(saveFileName),width = 550,height=960)
    plot(rastToPlot,range = chlRange,col=hcl.colors(n=100, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE))
    lines(auVect,lwd=1,col="grey30")
    points(vect(incident,geom=c("Longitude","Latitude")),col="black",alpha = 0.5,lwd=1,cex = 2)

    if (d == incident$Incident.Date) {
      points(vect(incident,geom=c("Longitude","Latitude")),col="red",lwd=1,cex = 2)
    }
    title(paste(d,incident$UIN,incident$Location,incident$Shark.common.name,daysPrior,SatPeriod,SatChannel,SatMethod),cex.main=0.8)
    dev.off()

  }
  print(paste("min max CHL",minChl,maxChl))

  animateFiles(paste0("^Area_",incident$UIN,"_.*.png"),w=550,h=960,
               outFile = paste0(paste("Area",incident$UIN,incident$Incident.Date,incident$Location,incident$Shark.common.name,sep="_"),".gif"),
               theDelay = 0.25)
  toRemove <- list.files(path = "./Outputs",pattern = paste0("^Area_",incident$UIN,"_.*.png"), full.names = TRUE)
  file.remove(toRemove)

}

dfRast <- as.data.frame(rastToPlot,xy=TRUE)

#frequency of value
ggplot(dfRast) + geom_bar(aes(round(`mean`,digits=2))) #geom_bar(aes(round(scales::censor(mean,range = c(0,15)),digits=1)))


