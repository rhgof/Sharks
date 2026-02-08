library(tidyverse)
library(terra)
library(scales)
library(paletteer)
library(tidyterra)
library(RUtils)

source(codeFile("usefulExtents.R"))
source(codeFile("coastsNcities.R"))
source(codeFile("rastUtilities.R"))

auBathy <- rast("/Volumes/Samples/InputData/GA/Australian Bathymetry and Topography 2023 250m/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")





city = "Sydney"
rangeDegrees = 2
lat = filter(ozCities,name==city)$lat
lon = filter(ozCities,name==city)$long
nswBathy<- cropToArea(auBathy,lat = lat, long = lon, degrees = 0.4)


seaLvl = -75
nswBathy <- crop(auBathy,getExtent("AU"))

names(nswBathy) <- "height"

nswBathy <- nswBathy - seaLvl
rastToPlot <- nswBathy
rastToPlot <- -log(-rastToPlot)
names(rastToPlot) <- "height"
auChart <- ggplot() +
  geom_spatraster(data = rastToPlot,
                  aes(fill = `height`),
#                  maxcell = ncell(nswBathy),
                  ) +
  coord_sf() +
  scale_x_continuous(expand = expansion(add=0)) +
  scale_y_continuous(expand = expansion(add=0)) +
  scale_fill_paletteer_c("pals::ocean.deep",
                         guide = guide_none(),
                         limits = c(-NA,0),
                         )
#  scale_fill_gradientn(colors = theColors$hex,
#                     values = scales::rescale(theColors$limit),
#                     limit = range(theColors$limit))
auChart
saveHorizChart(paste0("Sydney-Bathy",seaLvl),auChart)

# The multibeam backscatter data was acquired from Fugro Equator between June 2014 and February 2017 were processed
# by Geoscience Australia to 30 m resolution.
# This backscatter data was processed for the search area only, excluding all transit data and vessel turns.
# The data is presented as a yellow to bronze colour ramp, with high backscatter values in
# darker shades and overlain on a hillshade created from the 150 m bathymetry data.
# The hillshade was created with the parameters of point illumination azimuth at 45 degrees and altitude of 45 degrees.


bthyARaw <- rast("/Volumes/Samples/InputData/GA/North Australia Bathymetry 2018 30m/North_Australia_Bathymetry_A_2018_30m_cog.tif")
bthyBRaw <- rast("/Volumes/Samples/InputData/GA/North Australia Bathymetry 2018 30m/North_Australia_Bathymetry_B_2018_30m_cog.tif")
bthyCRaw <- rast("/Volumes/Samples/InputData/GA/North Australia Bathymetry 2018 30m/North_Australia_Bathymetry_C_2018_30m_cog.tif")
bthyDRaw <- rast("/Volumes/Samples/InputData/GA/North Australia Bathymetry 2018 30m/North_Australia_Bathymetry_D_2018_30m_cog.tif")

bthyA = crop(bthyARaw,getExtent("Tiwi"))
bthyB = crop(bthyBRaw,getExtent("Tiwi"))
bthyC = crop(bthyCRaw,getExtent("Tiwi"))
bthyD = crop(bthyDRaw,getExtent("Tiwi"))

bthyM <- merge(bthyD,bthyC)
rastToPlot = bthyM

seaLvl = 75
rastToPlot <- bthyC + seaLvl
rastToPlot
rastMin <- global(rastToPlot,"min")$min
rastMax <- global(rastToPlot,"max")$max
rastToPlot <- max(rastToPlot,-200)

#rastToPlot <- scales::oob_squish(rastToPlot,range=c(-200,Inf))

#plot(bthy)

theColors <- hypsometric_tints_db |>
  filter(pal == "gmt_globe")

neg <- nrow(filter(theColors, limit < 0))
pos <- nrow(filter(theColors, limit >= 0))

newLimits <- c(seq(-200,-20,length.out = neg),seq(0,90,length.out = 10),seq(100,1000,length.out = 9))

chart <-  ggplot() +
  geom_spatraster_contour_filled(data = rastToPlot,
                                 #bins = nrow(theColors),
                                 breaks = newLimits,
                                 aes(fill = after_stat(level),color=after_stat(level)),
                                 #color = NA,
   #               maxcell = 10000000, #ncell(bthyC)
  ) +
  geom_spatraster_contour(data = rastToPlot,breaks = c(0),color = "white") +
  geom_spatraster_contour(data = rastToPlot,breaks = c(seaLvl),color = "black" ) +
  coord_sf() +
  scale_x_continuous(expand = expansion(add=0)) +
  scale_y_continuous(expand = expansion(add=0)) +
#  scale_fill_gradientn(colors = theColors$hex,
#                       values = scales::rescale(theColors$limit),
#                       limit = range(theColors$limit)
  scale_color_manual(values = theColors$hex,
                     aesthetics = c("colour", "fill"),
 #                    limits = as.character(newLimits),
                     #limit = range(theColors$limit),
#  scale_fill_hypso_d(   palette = "gmt_globe",    #oob=scales::squish,
    #palette = "etopo1_hypso",
  #scale_fill_whitebox_c(palette = "muted",
#  scale_fill_scico(palette="roma", direction = 1,
  #scale_fill_paletteer_c("pals::ocean.haline", direction = 1,
    #breaks = c(-10000, 0, 1000, 2000),
#    guide = guide_colorbar(title = "Depth",title.position = "bottom"),
#  limits = c(-rastMax, rastMax),

  )
chart


theColors <- hypsometric_tints_db |>
  filter(pal == "gmt_globe")

neg <- nrow(filter(theColors, limit < 0))
pos <- nrow(filter(theColors, limit >= 0))

newLimits <- c(seq(-180,-20,length.out = neg),seq(0,1000,length.out = pos))



chart <-  ggplot() +
  geom_spatraster(data = rastToPlot,
  #                               binwidth = 20,
  #                               aes(fill = after_stat(level),color=after_stat(level)),
                                 #color = NA,
                                 #               maxcell = 10000000, #ncell(bthyC)
  ) +
  geom_spatraster_contour(data = rastToPlot,breaks = c(0),color = "white") +
  geom_spatraster_contour(data = rastToPlot,breaks = c(seaLvl),color = "grey40" ) +
  coord_sf() +
  scale_x_continuous(expand = expansion(add=0)) +
  scale_y_continuous(expand = expansion(add=0)) +
  #  scale_color_whitebox_b(palette = "atlas",guide=guide_none()) +
  #  scale_fill_whitebox_b(palette = "atlas",guide=guide_none(),
  # scale_color_hypso_c(palette = "gmt_globe",guide=guide_none()) +
  #  scale_color_manual(values = hypso.colors2(n=50,palette = "gmt_globe")) +
  #  scale_color_manual(values = hypso.colors2(n=50,palette = "gmt_globe"),aesthetics = c("colour", "fill"),
  scale_fill_gradientn(colors = theColors$hex,
                       oob = scales::oob_squish,
                    #n.breaks = nrow(theColors),
                    values = scales::rescale(newLimits),
                    #values = scales::rescale_mid(theColors$limit,from = range(theColors$limit),mid = 0),
                    limit = range(newLimits),
                    breaks = c(-100,-50,0,100,250,500,1000),
                    #guide = guide_colorsteps(),

  # scale_fill_hypso_c(   palette = "gmt_globe",    #oob=scales::squish,
                        #palette = "etopo1_hypso",
                        #scale_fill_whitebox_c(palette = "muted",
                        #  scale_fill_scico(palette="roma", direction = 1,
                        #scale_fill_paletteer_c("pals::ocean.haline", direction = 1,
                        #breaks = c(-10000, 0, 1000, 2000),
                        #    guide = guide_colorbar(title = "Depth",title.position = "bottom"),
                        #  limits = c(-rastMax, rastMax),

  )
chart

saveVertChart("BathyC-75m",chart)


