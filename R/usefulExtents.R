library(tidyverse)
library(terra)

usefulExtents <- tribble(
  ~State, ~Extent,
  "AU", ext(95,169,-45,-8.5),
  "NSW", ext(149.469,156.061,-37.893,-27.961),
  "NSWSthQld", ext(149.469,156.061,-37.893,-26),
  "WA",ext(110,123,-35.5,-25),
  "Tiwi", ext(120,132,-16,-9),
  "EastAu", ext(139,156,-45,-8.5)
)

getExtent <- function(abbr = "AU") {
  e <- filter(usefulExtents,State == abbr)$Extent[[1]]
  return(e)
}
