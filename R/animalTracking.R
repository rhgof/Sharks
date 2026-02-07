library(tidyverse)
library(sf)

library(RUtils)

detectionFile = inputFile("/IMOS/IMOS_detections_2023-09-24_12-53-40/IMOS_detections.csv")

detections <- read_csv(detectionFile)
