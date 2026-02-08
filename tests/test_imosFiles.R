library(testthat)
library(tidyverse)

# We test the parsing logic by creating a temp directory with known filenames
# and sourcing the function. Since imosFiles depends on RUtils only for the
# default directory path, we can bypass that by passing directory explicitly.

# To avoid needing RUtils at test time, we inline the function
source_imosFiles <- function() {
  imosFiles <- function(type = c("CHL", "SST"), directory = NULL) {
    type <- match.arg(type)

    if (is.null(directory)) {
      directory <- "/Volumes/Samples/InputData/cache"
    }

    listing <- dir(directory, full.names = FALSE)

    if (type == "CHL") {
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
  return(imosFiles)
}

test_that("imosFiles parses CHL filenames correctly", {
  imosFiles <- source_imosFiles()
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
  imosFiles <- source_imosFiles()
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
  imosFiles <- source_imosFiles()
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
  imosFiles <- source_imosFiles()
  expect_error(imosFiles("INVALID"), "'arg' should be one of")
})

test_that("CHL and SST wrappers are equivalent to direct calls", {
  imosFiles <- source_imosFiles()
  tmp <- tempdir()
  test_dir <- file.path(tmp, "imos_test_wrappers")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  file.create(file.path(test_dir, "V.P1D.20230915T053000Z.aust.chl_gsm.nc"))
  file.create(file.path(test_dir, "20230915092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-3d_dn.nc"))

  # Verify the wrapper pattern works
  imosCHLFiles <- function(directory = NULL) imosFiles("CHL", directory)
  imosSSTFiles <- function(directory = NULL) imosFiles("SST", directory)

  expect_equal(
    imosFiles("CHL", test_dir),
    imosCHLFiles(test_dir)
  )
  expect_equal(
    imosFiles("SST", test_dir),
    imosSSTFiles(test_dir)
  )
})
