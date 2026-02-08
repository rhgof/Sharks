library(testthat)
library(terra)
library(tidyverse)
library(RUtils)

# Ensure wd is project root so codeFile() resolves correctly
if (basename(getwd()) == "tests") setwd("..")
source(codeFile("rastUtilities.R"))

test_that("cropToArea produces correct extent", {
  r <- rast(nrows = 100, ncols = 100, xmin = 140, xmax = 160, ymin = -40, ymax = -20)
  values(r) <- runif(ncell(r))
  units(r) <- "mg/m^3"

  result <- cropToArea(r, lat = -33, long = 151, degrees = 1)
  e <- ext(result)

  expect_equal(as.numeric(e$xmin), 150)
  expect_equal(as.numeric(e$xmax), 152)
  expect_equal(as.numeric(e$ymin), -34)
  expect_equal(as.numeric(e$ymax), -32)
})

test_that("cropToArea preserves units", {
  r <- rast(nrows = 100, ncols = 100, xmin = 140, xmax = 160, ymin = -40, ymax = -20)
  values(r) <- runif(ncell(r))
  units(r) <- "mg/m^3"

  result <- cropToArea(r, lat = -33, long = 151, degrees = 1)

  expect_equal(units(result), "mg/m^3")
})

test_that("cropToArea respects degrees parameter", {
  r <- rast(nrows = 100, ncols = 100, xmin = 140, xmax = 160, ymin = -40, ymax = -20)
  values(r) <- runif(ncell(r))
  units(r) <- "K"

  result_small <- cropToArea(r, lat = -30, long = 150, degrees = 0.2)
  result_large <- cropToArea(r, lat = -30, long = 150, degrees = 2)

  expect_true(ncell(result_small) < ncell(result_large))

  e_small <- ext(result_small)
  expect_equal(as.numeric(e_small$xmin), 149.8)
  expect_equal(as.numeric(e_small$xmax), 150.2)
})

test_that("cropToArea works with multi-layer rasters", {
  r <- rast(nrows = 50, ncols = 50, xmin = 140, xmax = 160, ymin = -40, ymax = -20, nlyrs = 3)
  values(r) <- runif(ncell(r) * 3)
  units(r) <- c("mg/m^3", "mg/m^3", "mg/m^3")

  result <- cropToArea(r, lat = -30, long = 150, degrees = 1)

  expect_equal(nlyr(result), 3)
  expect_equal(units(result), c("mg/m^3", "mg/m^3", "mg/m^3"))
})
