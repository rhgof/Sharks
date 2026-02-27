library(testthat)
library(tidyverse)
library(RUtils)

# Ensure wd is project root so codeFile() resolves correctly
if (basename(getwd()) == "tests") setwd("..")
source(codeFile("chartDrumLineByRegion.R"))

# --- normalizeSdRegion tests ---

test_that("normalizeSdRegion maps long-form names to short canonical names", {
  long_names <- c(
    "Ballina to Lennox Head",
    "Barrenjoey - Barrenjoey to Warriewood",
    "Bondi - Bondi to Little Bay",
    "Central Coast South - Forresters to MacMasters",
    "Coffs Harbour - Coffs Harbour to Sawtell",
    "Eurobodalla - Surf Beach to Barlings Beach",
    "Kiama - Jones Beach to Gerringong",
    "Lake Macquarie -Blacksmiths to Frazer Park",
    "Manly - Manly to Turimetta",
    "Merimbula - Pambula to Tura Head",
    "Newcastle - Stockton to Redhead",
    "Port Macquarie \u2013 Port Macquarie to Tacking Point",
    "Shellharbour - Windang to Minnamurra",
    "Yamba - Iluka Bluff to Angourie Point"
  )
  expected <- c(
    "Ballina", "Barrenjoey", "Bondi", "Central Coast South",
    "Coffs Harbour", "Eurobodalla", "Kiama", "Lake Macquarie",
    "Manly", "Merimbula", "Newcastle", "Port Macquarie",
    "Shellharbour", "Yamba"
  )
  expect_equal(normalizeSdRegion(long_names), expected)
})

test_that("normalizeSdRegion passes through short names unchanged", {
  short_names <- c("Kingscliff", "Evans Head", "Forster", "Sydney East", "Sutherland")
  expect_equal(normalizeSdRegion(short_names), short_names)
})

test_that("normalizeSdRegion handles mixed long and short names", {
  mixed <- c("Ballina to Lennox Head", "Kingscliff", "Manly - Manly to Turimetta")
  expected <- c("Ballina", "Kingscliff", "Manly")
  expect_equal(normalizeSdRegion(mixed), expected)
})

# --- Chart function tests ---

# Minimal test data with both long and short SD_region names
test_data <- tibble(
  SD_region = c(
    "Ballina to Lennox Head", "Ballina to Lennox Head", "Kingscliff",
    "Newcastle - Stockton to Redhead", "Merimbula - Pambula to Tura Head"
  ),
  Species = c("White Shark", "Tiger Shark", "Bull Shark", "Dusky Whaler", "White Shark"),
  Date = c("01/01/2024", "15/01/2024", "01/02/2024", "15/02/2024", "01/03/2024")
)

test_that("chartDrumLineCaptureByRegion returns a ggplot", {
  chart <- chartDrumLineCaptureByRegion(
    test_data,
    title = "Test",
    subtitle = "Test subtitle"
  )
  expect_s3_class(chart, "ggplot")
})

test_that("chartDrumLineRegionTargetVsOther returns a ggplot", {
  chart <- chartDrumLineRegionTargetVsOther(
    test_data,
    title = "Test",
    subtitle = "Test subtitle"
  )
  expect_s3_class(chart, "ggplot")
})

test_that("region ordering is north to south (ascending latitude)", {
  coords <- read_csv(inputFile("sd_region_coordinates.csv"),
                     show_col_types = FALSE)

  chart <- chartDrumLineCaptureByRegion(
    test_data,
    title = "Test",
    subtitle = "Test subtitle"
  )

  # Extract factor levels from the chart data
  chart_data <- ggplot_build(chart)
  region_levels <- levels(chart$data$SD_region)

  # region_levels is bottom-to-top (reversed for coord_flip), so the last
  # element should be the northernmost region
  regions_top_to_bottom <- rev(region_levels)

  # Get latitudes for these regions
  region_lats <- coords |>
    filter(SD_region %in% regions_top_to_bottom) |>
    arrange(match(SD_region, regions_top_to_bottom)) |>
    pull(Latitude)

  # Latitudes should be descending (north to south = less negative to more negative)
  expect_true(all(diff(region_lats) <= 0),
              info = "Regions should be ordered north (top) to south (bottom)")
})
