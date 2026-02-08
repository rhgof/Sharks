library(tidyverse)
library(RUtils)

# Reference
# https://oceancolor.gsfc.nasa.gov/about/
# https://hermes.acri.fr/index.php?class=archive

imosFiles <- function(type = c("CHL", "SST"), directory = NULL) {
  type <- match.arg(type)

  if (is.null(directory)) {
    directory <- "/Volumes/Samples/InputData/cache"
  }

  listing <- dir(directory, full.names = FALSE)

  if (type == "CHL") {
    # CHL file naming: V.P1D.20191001T053000Z.aust.chl_gsm.nc
    theFiles <- as_tibble(listing) |>
      rename(FileName = value) |>
      mutate(Path = directory) |>
      mutate(FullPath = paste(directory, FileName, sep = "/")) |>
      filter(str_detect(FileName, "^V\\.P1D")) |>
      mutate(StartDate = str_extract(FileName, "P1D\\.(\\d{8})", group = 1)) |>
      mutate(EndDate = StartDate) |>
      mutate(MethodShort = str_to_upper(str_extract(FileName, "_([:alpha:]+)\\.nc", group = 1))) |>
      mutate(MethodLong = MethodShort) |>
      mutate(Period = "DAY") |>
      mutate(Gridkm = 1) |>
      mutate(Channel = str_to_upper(str_extract(FileName, "\\.([:alnum:]+)_[:alnum:]+\\.nc", group = 1)))
  } else {
    # SST file naming: 20191001092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-3d_dn.nc
    filePattern <- "ABOM-L3S_GHRSST-SSTfnd-[:alnum:]+-3d_dn.nc$"
    theFiles <- as_tibble(listing) |>
      rename(FileName = value) |>
      mutate(Path = directory) |>
      mutate(FullPath = paste(directory, FileName, sep = "/")) |>
      filter(str_detect(FileName, filePattern)) |>
      mutate(StartDate = str_extract(FileName, "^(\\d{8})", group = 1)) |>
      mutate(EndDate = StartDate) |>
      mutate(GridDegree = 0.02) |>
      mutate(Period = str_to_upper(str_extract(FileName, "-([:alnum:]+)_[:alpha:]+\\.nc$", group = 1)))
  }

  # Type conversions and set up Intervals for search
  # e.g. filter(theFiles, (today() - months(4)) %within% Interval)
  theFiles <- theFiles |>
    mutate(StartDate = ymd(StartDate, tz = "UTC")) |>
    mutate(EndDate = ymd(EndDate, tz = "UTC")) |>
    mutate(Interval = StartDate %--% EndDate)

  if (type == "CHL") {
    theFiles <- theFiles |>
      mutate(PeriodLong = case_when(
        Period == "8D" ~ "8 Days",
        Period == "MO" ~ "Monthly",
        Period == "DAY" ~ "Daily",
        .default = "Unknown"
      )) |>
      mutate(Title = paste(
        paste0(StartDate, " to ", EndDate),
        paste0(Gridkm, "km"), PeriodLong, MethodShort, Channel
      ))
  }

  return(theFiles)
}

imosCHLFiles <- function(directory = NULL) imosFiles("CHL", directory)
imosSSTFiles <- function(directory = NULL) imosFiles("SST", directory)
