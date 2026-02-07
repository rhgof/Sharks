library(tidyverse)
library(scales)
library(terra)
library(tidyterra)
library(sf)

library(RUtils)

source(codeFile("coastsNcities.R"))

# ----- chart SST -----
chartSST <- function(theRast,theTitle,theSubStitle,theRange = c(10,30)) {


  theBreaks = seq(min(theRange %/% 5)*5+5,max(theRange %/% 5)*5,5)
  theLabels = as.character(theBreaks)


  theCoast <- crop(AU_Coast,theRast)
  theCities <- crop(NSW_Cities,theRast)

  theRast <- scales::squish(theRast,range = theRange)
  theBreaks = seq(min(theRange %/% 5)*5+5,max(theRange %/% 5)*5,5)


  chart <-  ggplot() +
    geom_spatraster(data = theRast,
                    aes(fill = `sea_surface_temperature`),
                    maxcell = ncell(theRast)) +

    geom_spatvector(data = theCoast,linewidth = 0.02,color = "grey10") +
    geom_spatvector(data = theCities,color="grey40") +
    geom_spatvector_text(data = theCities,color="grey10",
                         mapping = aes(label=values(theCities)$name),
                         size=2.5,
                         hjust="right",
                         nudge_x = -0.05,
    ) +
    coord_sf() +
    scale_x_continuous(expand = expansion(add=0)) +
    scale_y_continuous(expand = expansion(add=0)) +
    # scale_fill_discrete(direction = -1,
    #                     type = hcl.colors(n=20, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE),
    #                     guide = guide_colorsteps(title = "\u00B0C",reverse=FALSE),
    #                     ) +
    scale_fill_gradientn(
      colors = hcl.colors(n=50, palette = "Spectral", alpha = 1, rev = TRUE, fixup = TRUE),
      limits = theRange,
      breaks = theBreaks, labels = theLabels,
      guide = guide_colorbar(title = "\u00B0C",title.position = "bottom"),
    )  +
    ggtitle(theTitle,theSubStitle) +
    xlab("") + ylab("") +
    labs(caption = makeCaption(paste("Source: https://portal.aodn.org.au/"))) +
    customTheme() +
    theme(
      #panel.background = element_rect(fill = "grey60"),
      legend.title = element_text(size = 8),
      legend.position = "inside",
      legend.position.inside = c(0.15,0.85),
      panel.grid = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(size = 12, face = "bold",hjust=0,margin = margin(0,0,0,0),""),
      #plot.background = element_rect(fill = "grey60")
    )
  chart

  return(chart)

}
