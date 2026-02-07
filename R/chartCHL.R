library(tidyverse)
library(scales)
library(terra)
library(tidyterra)
library(sf)
library(paletteer)

library(RUtils)

source(codeFile("coastsNcities.R"))

# ----- chart SST -----
chartCHL <- function(theRast,theTitle,theSubtitle,squishTo = 20, useLog = FALSE) {

  theRast <- scales::oob_squish(theRast,range=c(0,squishTo))

  if (useLog) {
    lm = log(c(0.01,squishTo))
    br = log(c(0.1,0.5,1,2,4,8,16))
    lb = as.character(c(0.1,0.5,1,2,4,8,16))
    rastToPlot = log(theRast)
    #  theRast <- scales::censor(theRast,range = log(c(0.001,15)))
  } else {
    lm = (c(0,squishTo))
    br = (c(1,2,4,8,16))
    lb = as.character(br)
    rastToPlot =theRast
    # theRast <- scales::censor(theRast,range = (c(0,15)))
  }

  theCoast <- crop(AU_Coast,theRast)
  theCities <- crop(NSW_Cities,theRast)

  chart <-  ggplot() +
    geom_spatraster(data = rastToPlot,
                    aes(fill = `mean`),
                    maxcell = ncell(theRast)
    ) +
    geom_spatvector(data = theCoast,linewidth = 0.02,color = "grey10") +
    geom_spatvector(data = theCities,color="grey40") +
    geom_spatvector_text(data = theCities,color="grey10",
                         mapping = aes(label=values(theCities)$name),
                         size=2.5,
                         hjust="right",
                         nudge_x = -0.05,
                         #dim="XY",
    ) +
    coord_sf() +
    scale_x_continuous(expand = expansion(add=0)) +
    scale_y_continuous(expand = expansion(add=0)) +
    scale_fill_paletteer_c("pals::ocean.haline", direction = 1,
                           limits = lm,
                           breaks = br,
                           labels = lb,
                           guide = guide_colorbar(title = "mg/m^3",title.position = "bottom"),
    ) +

    # scale_fill_gradientn(
    #   colors = hcl.colors(n=50, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE),
    #   #trans="log",
    #   limits = lm,
    #   breaks = br,
    #   labels = lb,
    #   guide = guide_colorbar(title = "mg/m^3"),
    # )  +
    ggtitle(theTitle,theSubtitle) +
    xlab("") + ylab("") +
    labs(caption = makeCaption(paste("Source: https://portal.aodn.org.au/"))) +
    customTheme() +
    theme(
      #panel.background = element_rect(fill = "grey60"),
      legend.title = element_text(size = 8),
      legend.position = "inside",
      legend.position.inside = c(0.15,0.85),
#      legend.background = element_rect(color="white"),
      panel.grid = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(size = 12, face = "bold",hjust=0,margin = margin(0,0,0,0)),
      #plot.background = element_rect(fill = "grey60")
    )
  chart

  return(chart)

}
