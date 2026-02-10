library(testthat)
library(terra)
library(tidyverse)
library(RUtils)

# Ensure wd is project root so codeFile() resolves correctly
if (basename(getwd()) == "tests") setwd("..")
source(codeFile("rastUtilities.R"))

# --- Mock functions to avoid network dependency ---
# Override the raster-fetching functions with synthetic data generators

mock_chl_raster <- function(theDate, lat, lon, daysPrior = 5,
                            theDegrees = 1, maxDaysPrior = 30) {
  r <- rast(nrows = 20, ncols = 20,
            xmin = lon - theDegrees, xmax = lon + theDegrees,
            ymin = lat - theDegrees, ymax = lat + theDegrees,
            nlyrs = daysPrior)
  # Fill with CHL-like values; corners get extreme values for testing
  vals <- matrix(runif(ncell(r) * nlyr(r), min = 0.5, max = 2.0),
                 nrow = ncell(r), ncol = nlyr(r))
  # Set corners of first layer to known extremes
  vals[1, 1] <- 0.01   # min in corners (regional only)
  vals[ncell(r), 1] <- 10.0  # max in corners (regional only)
  values(r) <- vals
  units(r) <- rep("mg/m^3", nlyr(r))
  r
}

mock_sst_raster <- function(theDate, lat, lon, daysPrior = 1,
                            theDegrees = 1, maxDaysPrior = 30) {
  r <- rast(nrows = 20, ncols = 20,
            xmin = lon - theDegrees, xmax = lon + theDegrees,
            ymin = lat - theDegrees, ymax = lat + theDegrees,
            nlyrs = daysPrior)
  # SST in Kelvin (~290-300K = ~17-27C)
  vals <- matrix(runif(ncell(r) * nlyr(r), min = 293, max = 298),
                 nrow = ncell(r), ncol = nlyr(r))
  values(r) <- vals
  names(r) <- rep("sea_surface_temperature", nlyr(r))
  r
}

mock_chl_null <- function(theDate, lat, lon, daysPrior = 5,
                          theDegrees = 1, maxDaysPrior = 30) {
  NULL
}

mock_sst_null <- function(theDate, lat, lon, daysPrior = 1,
                          theDegrees = 1, maxDaysPrior = 30) {
  NULL
}

# --- Install mocks ---
# Save originals so tests are isolated
original_chl <- chlRasterDateLocation
original_sst <- sstRasterDateLocation

setup_mocks <- function(chl_fn = mock_chl_raster, sst_fn = mock_sst_raster) {
  assign("chlRasterDateLocation", chl_fn, envir = globalenv())
  assign("sstRasterDateLocation", sst_fn, envir = globalenv())
}

restore_originals <- function() {
  assign("chlRasterDateLocation", original_chl, envir = globalenv())
  assign("sstRasterDateLocation", original_sst, envir = globalenv())
}

# --- Tests ---

test_that("extractOceanographicStats returns correct structure", {
  setup_mocks()
  on.exit(restore_originals())

  result <- extractOceanographicStats(
    date = as.Date("2023-06-15"), lat = -33, lon = 151,
    chl_days_prior = 3, sst_days_prior = 2
  )

  expect_type(result, "list")
  expect_named(result, c("stats", "chl_rast", "sst_rast"))
  expect_s3_class(result$stats, "tbl_df")
  expect_equal(nrow(result$stats), 1)
  expect_s4_class(result$chl_rast, "SpatRaster")
  expect_s4_class(result$sst_rast, "SpatRaster")
})

test_that("stats tibble has exactly 18 expected columns", {
  setup_mocks()
  on.exit(restore_originals())

  result <- extractOceanographicStats(
    date = as.Date("2023-06-15"), lat = -33, lon = 151
  )

  expected_cols <- c(
    "CHL_min", "CHL_max", "CHL_mean",
    "CHLAttack_min", "CHLAttack_max", "CHLAttack_mean",
    "CHL_unit", "CHL_days",
    "SST_min", "SST_max", "SST_mean",
    "SSTAttack_min", "SSTAttack_max", "SSTAttack_mean",
    "SST_Unit", "SST_days",
    "RangeDegrees", "DataDegrees"
  )
  expect_equal(names(result$stats), expected_cols)
  expect_equal(ncol(result$stats), 18)
})

test_that("SST is converted from Kelvin to Celsius", {
  setup_mocks()
  on.exit(restore_originals())

  result <- extractOceanographicStats(
    date = as.Date("2023-06-15"), lat = -33, lon = 151,
    sst_days_prior = 2
  )

  s <- result$stats
  # Mock SST is ~293-298K, so Celsius should be ~20-25, not ~293-298
  expect_true(s$SST_mean > 10 && s$SST_mean < 35)
  expect_true(s$SST_min > 10 && s$SST_min < 35)
  expect_true(s$SST_max > 10 && s$SST_max < 35)
  expect_equal(s$SST_Unit, "\u00B0C")
})

test_that("attack-area stats differ from regional when corners have extremes", {
  setup_mocks()
  on.exit(restore_originals())

  # With mock_chl_raster: corner cells have 0.01 and 10.0
  # Attack area (±0.2 degrees) is much smaller than regional (±1 degree)
  # so corners should be outside the attack area
  result <- extractOceanographicStats(
    date = as.Date("2023-06-15"), lat = -33, lon = 151,
    chl_days_prior = 1, range_degrees = 1, attack_degrees = 0.2
  )

  s <- result$stats
  # Regional min should be lower (includes corner extremes)
  expect_true(s$CHL_min <= s$CHLAttack_min)
  # Regional max should be higher (includes corner extremes)
  expect_true(s$CHL_max >= s$CHLAttack_max)
})

test_that("returns NULL when CHL data unavailable", {
  setup_mocks(chl_fn = mock_chl_null, sst_fn = mock_sst_raster)
  on.exit(restore_originals())

  expect_warning(
    result <- extractOceanographicStats(
      date = as.Date("2023-06-15"), lat = -33, lon = 151
    ),
    "No CHL data available"
  )
  expect_null(result)
})

test_that("returns NULL when SST data unavailable", {
  setup_mocks(chl_fn = mock_chl_raster, sst_fn = mock_sst_null)
  on.exit(restore_originals())

  expect_warning(
    result <- extractOceanographicStats(
      date = as.Date("2023-06-15"), lat = -33, lon = 151
    ),
    "No SST data available"
  )
  expect_null(result)
})

test_that("parameters pass through correctly", {
  setup_mocks()
  on.exit(restore_originals())

  result <- extractOceanographicStats(
    date = as.Date("2023-06-15"), lat = -33, lon = 151,
    chl_days_prior = 4, sst_days_prior = 2,
    range_degrees = 2, attack_degrees = 0.5
  )

  s <- result$stats
  expect_equal(s$RangeDegrees, 2)
  expect_equal(s$DataDegrees, 0.5)
  # CHL_days reflects mock layer count = chl_days_prior
  expect_equal(s$CHL_days, 4)
  # SST_days reflects mock layer count = sst_days_prior
  expect_equal(s$SST_days, 2)
})
