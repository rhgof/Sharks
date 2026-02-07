library(tidyverse)
library(gifski)

animateFiles <- function(filePattern = "^IMOS.*8_DAY.*.png",w,h,outFile = "animation.gif",theDelay = 1) {
  theFiles <- as_tibble(list.files("./Outputs", pattern = filePattern, full.names = TRUE))

  theFiles <- theFiles |>
    rename(FileName = value ) |>
    arrange(theFiles,FileName)

  gifski(theFiles$FileName, gif_file = outputFile(outFile),
         width = w, height = h,
         delay = theDelay,loop=1)
}
