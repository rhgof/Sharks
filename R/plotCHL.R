library(tidyverse)
library(scales)
library(terra)
library(tidyterra)
library(viridis)
library(ncdf4)

library(RUtils)

source(codeFile("downloadCHLFiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("getnetcdfFile.R"))
source(codeFile("usefulextents.R"))
source(codeFile("animateData.R"))
source(codeFile("chartCHL.R"))

{
  daysPrior = 5
  theDate = ymd("25-11-27")
  theDate = today()
  geoScope = "NSW"
  #geoScope = "NSWSthQld"

  downloadCHLFilesDate(theDate,daysPrior=daysPrior)

  ncdfFiles <- imosCHLFiles()

#  lastSatDate = as.Date(max(ncdfFiles$StartDate)) # as.Date to make min below work
#  theDate = min(today(),lastSatDate)

  filesOfInterest <- ncdfFiles |>
    filter(StartDate %within%  interval(theDate-days(daysPrior),theDate ))

  lastSatPass = max(filesOfInterest$StartDate)
  firstSatPass = min(filesOfInterest$StartDate)

  theRast = rast(filesOfInterest$FullPath)
  measure = longnames(theRast)[1]
  measure = "Ocean Chlorophyll"
  unit = units(theRast)[1]
  nFile = nlyr(theRast)

  theExt <- getExtent(geoScope)
  theRast <-crop(theRast,theExt)
  #theRast <- scales::oob_squish(theRast,range=c(0,20))
  theRast <- mean(theRast,na.rm=TRUE)
  crs(theRast) <- "EPSG:7844"
  theRast


  theTitle = paste(format(lastSatPass,"%d %b %Y"), "NSW",(measure) )
  theSubTitle = paste("Prior",daysPrior,"days average of CHL concentration.GSM Model\nLast satellite pass:",
                      format(lastSatPass,"%Y-%m-%d %Z."),"No data in grey")


  chlChart <- chartCHL(theRast,theTitle, squishTo = 80,theSubTitle,useLog = TRUE)
}
#chlChart <- chlChart + annotate(geom="point", y=-31.8464,x= 152.7489,color="red",size=2)
chlChart


saveVertChart(paste0(geoScope,"-CHL-",daysPrior),chlChart,theDate = lastSatPass, onlyLatest=FALSE)



#------------------
# dfRast <- as.data.frame(theRast,xy=TRUE)
#
#
# ggplot(dfRast,aes(x=x,y=y,fill=scales::censor(mean,range = c(0.001,20)))) +
#   geom_tile() +
#
# #  geom_spatvector(data = nswVect, mapping = aes(), inherit.aes = FALSE) +
# #  geom_spatvector(data= nswVect,mapping = aes(fill=NA,color="grey20"),inherit.aes = FALSE) +
#
#   #annotate(geom = "text",x=incident$Longitude,y = incident$Latitude, label = incident$Location, color = "white") +
#   scale_fill_gradientn(
#     colors = hcl.colors(n=50, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE),
#     #trans="log",
#     limits = c(0.001,4.5),
#     breaks = c(0.05,0.5,1,2,4),
#     guide = guide_colorbar(title = "mg/m^3"),
#     #                       oob = scales::censor(range = c(0.05,4.5)),
#   ) +
#   scale_x_continuous(expand = expansion(add=0)) +
#   scale_y_continuous(expand = expansion(add=0)) +
#   xlab("") + ylab("") +
#   ggtitle(paste(format(theDate,"%d %b %Y"), "NSW", str_to_title(measure),unit)) +
#   labs(caption = makeCaption(paste("Source: https://portal.aodn.org.au/"))) +
#   coord_quickmap() +
#   customTheme() +
#   theme(
#     panel.background = element_rect(fill = "grey60"),
#     panel.grid = element_blank(),
#     #plot.background = element_rect(fill = "grey60")
#   )
