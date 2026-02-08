library(testthat)
library(tidyverse)
library(RUtils)

# Ensure wd is project root so codeFile() resolves correctly
if (basename(getwd()) == "tests") setwd("..")
source(codeFile("readIMOSFileListing.R"))

test_that("imosFiles parses CHL filenames correctly", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "imos_test_chl")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test CHL files
  file.create(file.path(test_dir, "V.P1D.20230915T053000Z.aust.chl_gsm.nc"))
  file.create(file.path(test_dir, "V.P1D.20230916T053000Z.aust.chl_gsm.nc"))
  # Non-matching file should be excluded
  file.create(file.path(test_dir, "some_other_file.nc"))

  result <- imosFiles("CHL", directory = test_dir)

  expect_equal(nrow(result), 2)
  expect_equal(as.Date(result$StartDate), as.Date(c("2023-09-15", "2023-09-16")))
  expect_equal(result$MethodShort, c("GSM", "GSM"))
  expect_equal(result$Channel, c("CHL", "CHL"))
  expect_equal(result$Period, c("DAY", "DAY"))
  expect_equal(result$Gridkm, c(1, 1))
  expect_true(all(str_detect(result$FullPath, test_dir)))
})

test_that("imosFiles parses SST filenames correctly", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "imos_test_sst")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test SST files
  file.create(file.path(test_dir, "20230915092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-3d_dn.nc"))
  file.create(file.path(test_dir, "20230916092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-3d_dn.nc"))
  # Non-matching file should be excluded
  file.create(file.path(test_dir, "V.P1D.20230915T053000Z.aust.chl_gsm.nc"))

  result <- imosFiles("SST", directory = test_dir)

  expect_equal(nrow(result), 2)
  expect_equal(as.Date(result$StartDate), as.Date(c("2023-09-15", "2023-09-16")))
  expect_equal(result$GridDegree, c(0.02, 0.02))
  expect_equal(result$Period, c("3D", "3D"))
  expect_true(all(str_detect(result$FullPath, test_dir)))
})

test_that("imosFiles returns empty tibble for empty directory", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "imos_test_empty")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  result_chl <- imosFiles("CHL", directory = test_dir)
  result_sst <- imosFiles("SST", directory = test_dir)

  expect_equal(nrow(result_chl), 0)
  expect_equal(nrow(result_sst), 0)
})

test_that("imosFiles type argument validates correctly", {
  expect_error(imosFiles("INVALID"), "'arg' should be one of")
})

test_that("CHL and SST wrappers are equivalent to direct calls", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "imos_test_wrappers")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  file.create(file.path(test_dir, "V.P1D.20230915T053000Z.aust.chl_gsm.nc"))
  file.create(file.path(test_dir, "20230915092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-3d_dn.nc"))

  expect_equal(
    imosFiles("CHL", test_dir),
    imosCHLFiles(test_dir)
  )
  expect_equal(
    imosFiles("SST", test_dir),
    imosSSTFiles(test_dir)
  )
})
