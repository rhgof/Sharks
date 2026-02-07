library(tidyverse)
library(readxl)
library(readr)

library(RUtils)

readSharkDB <- function() {

  #dbFile <- inputFile("Australian Shark-Incident Database Public Version.xlsx")
  adjustedDBFile <- inputFile("Australian Shark-Incident Adjusted.csv")

  # sharkDBRaw <- read_xlsx(dbFile)
  #
  # sharkDB <- sharkDBRaw |>
  #   mutate(Incident.Date = ym(paste0(Incident.year,Incident.month))) |>
  #   select(UIN,Incident.Date,State,Location,Latitude,Longitude,Site.category, contains("Shark"),`Provoked/unprovoked`,contains("Victim")) |>
  #   replace_na(list(Shark.common.name = "Unknown",Shark.scientific.name = "Unknown"))


  sharkDBAdj <- read_csv(adjustedDBFile) |>
    mutate(Incident.Date = ymd(paste(Incident.year,Incident.month,Incident.day,sep="-"))) |>
    select(-contains("Notes")) |>
    select(-contains("Orig")) |>
    select(-contains("Adj")) |>
    replace_na(list(Shark.common.name = "Unknown",Shark.scientific.name = "Unknown"))

  # sharkDBAdj <- sharkDB |>
  #   # fix locations
  #   # join original with updated locations.
  #   left_join(adjDBRaw, by = join_by(UIN),suffix = c(".old","")) |>
  #   select(-contains(".old"))


  return (sharkDBAdj)
}
