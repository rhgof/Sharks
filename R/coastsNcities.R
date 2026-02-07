library(terra)

library(RUtils)

source(codeFile("usefulExtents.R"))

nswExt <- getExtent("NSW")
auExt <- getExtent("AU")


GDA2020_CRS = "EPSG:7844"

if (!exists("auShpFile") ) {

  auShpFile = inputFile("AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
  AU_Coast <-  vect(auShpFile)
  NSW_Coast <- crop(AU_Coast,nswExt)

  #inputs
  ozCities <- read_csv(inputFile("Australia-Cities.csv"))
  citiesOfInterest <- c("Sydney","Wollongong","Newcastle","Coffs Harbour", "Byron Bay","Forster-Tuncurry","Port Macquarie",
                        "Tewantin-Noosa")
  citiesToPlot <- ozCities |>
    filter(name %in% citiesOfInterest)

  NSW_Cities <- vect(citiesToPlot,geom=c("long","lat"),crs = GDA2020_CRS)
}
