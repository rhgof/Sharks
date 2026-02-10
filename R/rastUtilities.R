library(tidyverse)
library(RUtils)
library(terra)

source(codeFile("downloadCHLFiles.R"))
source(codeFile("downloadSSTFiles.R"))
source(codeFile("readIMOSFileListing.R"))

cropToArea <- function(theRaster, lat, long, degrees = 1) {
  area <- ext(long - degrees, long + degrees, lat - degrees, lat + degrees)

  rast_units <- units(theRaster)
  cropped_rast <- crop(theRaster, area)
  units(cropped_rast) <- rast_units

  return(cropped_rast)
}


chlRasterDateLocation <- function(theDate, lat, lon, daysPrior = 5,
                                  theDegrees = 1, maxDaysPrior = 30) {
  downloadCHLFilesDate(theDate, daysPrior)
  files <- imosCHLFiles()

  repeat {
    files_of_interest <- files |>
      filter(StartDate %within% interval(theDate - days(daysPrior), theDate))
    if (nrow(files_of_interest) > 0) break
    daysPrior <- daysPrior + 1
    message(paste("CHL Extending", daysPrior))
    if (daysPrior > maxDaysPrior) {
      warning(paste("No CHL data found within", maxDaysPrior, "days of", theDate))
      return(NULL)
    }
  }

  rast_data <- rast(files_of_interest$FullPath)
  rast_data <- cropToArea(rast_data, lat, lon, theDegrees)

  return(rast_data)
}


sstRasterDateLocation <- function(theDate, lat, lon, daysPrior = 1,
                                  theDegrees = 1, maxDaysPrior = 30) {
  downloadSSTFilesDate(theDate, daysPrior)
  files <- imosSSTFiles()

  repeat {
    files_of_interest <- files |>
      filter(StartDate %within% interval(theDate - days(daysPrior), theDate))
    if (nrow(files_of_interest) > 0) break
    daysPrior <- daysPrior + 1
    message(paste("SST Extending", daysPrior))
    if (daysPrior > maxDaysPrior) {
      warning(paste("No SST data found within", maxDaysPrior, "days of", theDate))
      return(NULL)
    }
  }

  rast_data <- rast(files_of_interest$FullPath)

  sst_name <- "sea_surface_temperature"
  sst_rast <- rast_data[sst_name]
  sst_rast <- cropToArea(sst_rast, lat, lon, theDegrees)

  return(sst_rast)
}


#' Extract oceanographic statistics for a location and date
#'
#' Fetches CHL and SST satellite data, computes regional and attack-area
#' statistics (min, mean, max). Converts SST from Kelvin to Celsius.
#'
#' @param date Date of interest
#' @param lat Latitude of location
#' @param lon Longitude of location
#' @param chl_days_prior Days to search backward for CHL data
#' @param sst_days_prior Days to search backward for SST data
#' @param range_degrees Regional extent in degrees (±) around location
#' @param attack_degrees Attack-area extent in degrees (±) around location
#' @return A list with `$stats` (1-row tibble), `$chl_rast` (averaged CHL),
#'   `$sst_rast` (averaged SST in Celsius), or NULL if satellite data unavailable
extractOceanographicStats <- function(date, lat, lon,
                                      chl_days_prior = 5,
                                      sst_days_prior = 3,
                                      range_degrees = 1,
                                      attack_degrees = 0.2) {
  # --- CHL ---
  chl_rast <- chlRasterDateLocation(date, lat, lon,
                                    daysPrior = chl_days_prior,
                                    theDegrees = range_degrees)
  if (is.null(chl_rast)) {
    warning(paste("No CHL data available for", date, "at", lat, lon))
    return(NULL)
  }

  chl_unit <- units(chl_rast)[1]
  chl_days <- nlyr(chl_rast)
  chl_rast <- mean(chl_rast, na.rm = TRUE)

  chl_min  <- global(chl_rast, "min", na.rm = TRUE)[[1]]
  chl_mean <- global(chl_rast, "mean", na.rm = TRUE)[[1]]
  chl_max  <- global(chl_rast, "max", na.rm = TRUE)[[1]]

  chl_rast_attack <- cropToArea(chl_rast, lat, lon, degrees = attack_degrees)
  chl_attack_min  <- global(chl_rast_attack, "min", na.rm = TRUE)[[1]]
  chl_attack_mean <- global(chl_rast_attack, "mean", na.rm = TRUE)[[1]]
  chl_attack_max  <- global(chl_rast_attack, "max", na.rm = TRUE)[[1]]

  # --- SST ---
  sst_rast <- sstRasterDateLocation(date, lat, lon,
                                    daysPrior = sst_days_prior,
                                    theDegrees = range_degrees)
  if (is.null(sst_rast)) {
    warning(paste("No SST data available for", date, "at", lat, lon))
    return(NULL)
  }

  sst_rast <- sst_rast - 273.15
  sst_days <- nlyr(sst_rast)
  sst_rast <- mean(sst_rast, na.rm = TRUE)

  sst_min  <- global(sst_rast, "min", na.rm = TRUE)[[1]]
  sst_mean <- global(sst_rast, "mean", na.rm = TRUE)[[1]]
  sst_max  <- global(sst_rast, "max", na.rm = TRUE)[[1]]

  sst_rast_attack <- cropToArea(sst_rast, lat, lon, degrees = attack_degrees)
  sst_attack_min  <- global(sst_rast_attack, "min", na.rm = TRUE)[[1]]
  sst_attack_mean <- global(sst_rast_attack, "mean", na.rm = TRUE)[[1]]
  sst_attack_max  <- global(sst_rast_attack, "max", na.rm = TRUE)[[1]]

  # --- Assemble stats tibble (column names match existing CSV output) ---
  stats <- tibble(
    CHL_min = chl_min, CHL_max = chl_max, CHL_mean = chl_mean,
    CHLAttack_min = chl_attack_min, CHLAttack_max = chl_attack_max,
    CHLAttack_mean = chl_attack_mean,
    CHL_unit = chl_unit, CHL_days = chl_days,
    SST_min = sst_min, SST_max = sst_max, SST_mean = sst_mean,
    SSTAttack_min = sst_attack_min, SSTAttack_max = sst_attack_max,
    SSTAttack_mean = sst_attack_mean,
    SST_Unit = "\u00B0C", SST_days = sst_days,
    RangeDegrees = range_degrees, DataDegrees = attack_degrees
  )

  list(
    stats = stats,
    chl_rast = chl_rast,
    sst_rast = sst_rast
  )
}
