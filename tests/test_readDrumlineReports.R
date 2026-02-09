library(testthat)
library(tidyverse)
library(pdftools)
library(RUtils)

# Ensure wd is project root so codeFile() resolves correctly
if (basename(getwd()) == "tests") setwd("..")
source(codeFile("readDrumlineReports.R"))

# --- Unit tests for helper functions ---

test_that("findColumnRanges detects headers on a data page", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  pages <- pdf_data(pdf_path)
  # Page 13 is first full data page
  col_ranges <- findColumnRanges(pages[[13]])

  expect_false(is.null(col_ranges))
  expect_true("col_starts" %in% names(col_ranges))
  expect_true("header_y" %in% names(col_ranges))
  expect_true(all(c("Date", "Time", "Species", "Sex", "Status", "Tags",
                     "SD_region", "Beach") %in% names(col_ranges$col_starts)))
})

test_that("findColumnRanges returns NULL for non-data pages", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  pages <- pdf_data(pdf_path)
  # Page 1 is a cover page, no table headers
  col_ranges <- findColumnRanges(pages[[1]])
  expect_null(col_ranges)
})

test_that("assignWordToColumn maps x-positions to correct columns", {
  # Simulated column starts matching observed page 13 positions
  col_starts <- c(
    Date = 45L, Time = 116L, Species = 155L, Size_cm = 242L,
    Sex = 282L, Status = 315L, Tags = 375L, SD_region = 412L, Beach = 485L
  )

  expect_equal(assignWordToColumn(45, col_starts), "Date")
  expect_equal(assignWordToColumn(118, col_starts), "Time")
  expect_equal(assignWordToColumn(155, col_starts), "Species")
  expect_equal(assignWordToColumn(185, col_starts), "Species")   # "Shark" in "White Shark"
  expect_equal(assignWordToColumn(243, col_starts), "Size_cm")
  expect_equal(assignWordToColumn(288, col_starts), "Sex")
  expect_equal(assignWordToColumn(315, col_starts), "Status")
  expect_equal(assignWordToColumn(376, col_starts), "Tags")
  expect_equal(assignWordToColumn(390, col_starts), "Tags")      # "A" in "ID, A"
  expect_equal(assignWordToColumn(412, col_starts), "SD_region")
  expect_equal(assignWordToColumn(442, col_starts), "SD_region") # "Head" in "Evans Head"
  expect_equal(assignWordToColumn(485, col_starts), "Beach")
  expect_equal(assignWordToColumn(520, col_starts), "Beach")     # multi-word beach
})

test_that("groupWordsIntoRows clusters by y-proximity", {
  words <- tibble(
    x = c(45L, 155L, 243L, 45L, 155L, 243L, 155L),
    y = c(85L, 85L, 85L, 120L, 120L, 120L, 134L),
    width = rep(20L, 7),
    height = rep(12L, 7),
    space = rep(FALSE, 7),
    text = c("date1", "sp1", "sz1", "date2", "sp2", "sz2", "cont")
  )

  groups <- groupWordsIntoRows(words, y_tolerance = 5)

  expect_length(groups, 3)
  expect_equal(nrow(groups[[1]]), 3)  # y=85
  expect_equal(nrow(groups[[2]]), 3)  # y=120
  expect_equal(nrow(groups[[3]]), 1)  # y=134 (continuation)
})

test_that("enrichDrumlineData adds all derived columns", {
  raw <- tibble(
    Date = c("30/07/2023", "16/10/2023"),
    Time = c("7:08", NA_character_),
    Species = c("White Shark", "Snapper"),
    Size_cm = c(263, 42),
    Sex = c("F", NA_character_),
    Status = c("Alive", "Dead"),
    Tags = c("ID, A", NA_character_),
    SD_region = c("Ballina", "Lake Macquarie"),
    Beach = c("Lighthouse Beach", "Hams Beach")
  )

  enriched <- enrichDrumlineData(raw, "test.pdf")

  # Check all derived columns exist
  expect_true(all(c("DateTime", "Year", "Month", "Species_Code",
                     "Target_Shark", "Has_ID_Tag", "Has_Acoustic_Tag",
                     "Source_PDF") %in% names(enriched)))

  # Year and Month
  expect_equal(enriched$Year, c(2023L, 2023L))
  expect_equal(enriched$Month, c(7L, 10L))

  # Species codes
  expect_equal(enriched$Species_Code, c("WHI", "SNP"))

  # Target shark
  expect_equal(enriched$Target_Shark, c(TRUE, FALSE))

  # Tags
  expect_equal(enriched$Has_ID_Tag, c(TRUE, FALSE))
  expect_equal(enriched$Has_Acoustic_Tag, c(TRUE, FALSE))

  # Source PDF
  expect_equal(enriched$Source_PDF, c("test.pdf", "test.pdf"))
})

test_that("enrichDrumlineData handles unknown species codes", {
  raw <- tibble(
    Date = "1/01/2024", Time = "10:00",
    Species = "Unicorn Shark", Size_cm = 100,
    Sex = "M", Status = "Alive", Tags = "ID",
    SD_region = "Test", Beach = "Test Beach"
  )

  enriched <- enrichDrumlineData(raw, "test.pdf")
  expect_equal(enriched$Species_Code, "UNK")
})

# --- Integration tests against real 2023-24 PDF ---

test_that("extractAppendix2 produces expected row count", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)

  # Should have a substantial number of rows (report covers ~1200 catches)
  expect_gt(nrow(result), 1100)
  expect_lt(nrow(result), 1400)
})

test_that("Species names are complete (not truncated)", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)

  # These multi-word species must appear complete, not truncated
  species <- unique(result$Species)
  expect_true("White Shark" %in% species)
  expect_true("Tiger Shark" %in% species)
  expect_true("Bull Shark" %in% species)
  expect_true("Shortfin Mako Shark" %in% species)
  expect_true("Smooth Hammerhead" %in% species)
  expect_true("Common Blacktip" %in% species)

  # Truncated names from the buggy Python version must NOT appear
  expect_false("White" %in% species)
  expect_false("Shortfin" %in% species)
  expect_false("Smooth" %in% species)
})

test_that("Size_cm is numeric and Sex is not swapped with Size", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)

  # Size should be numeric (not character), and in a plausible range
  expect_true(is.numeric(result$Size_cm))
  valid_sizes <- result |> filter(!is.na(Size_cm))
  expect_true(all(valid_sizes$Size_cm >= 10 & valid_sizes$Size_cm <= 500))

  # Sex should only be F, M, or NA — not a number
  valid_sex <- result |> filter(!is.na(Sex)) |> pull(Sex)
  expect_true(all(valid_sex %in% c("F", "M")))
})

test_that("SD_region does not contain beach names", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)

  # SD_region should be location names, not beach names
  sd_regions <- unique(result$SD_region)
  expect_false(any(str_detect(sd_regions, "Beach")))
  expect_false(any(str_detect(sd_regions, "Point$")))
})

test_that("Tags field parses correctly with ID and acoustic tags", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)

  # White Sharks should predominantly have ID, A tags
  white_sharks <- result |> filter(Species == "White Shark")
  expect_true(mean(white_sharks$Has_ID_Tag) > 0.9)
  expect_true(mean(white_sharks$Has_Acoustic_Tag) > 0.9)

  # Dead animals typically have no tags

  dead <- result |> filter(Status == "Dead")
  expect_true(mean(!dead$Has_ID_Tag) > 0.5)
})

test_that("Date range covers July 2023 to June 2024", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)
  dates <- dmy(result$Date)

  expect_true(min(dates, na.rm = TRUE) >= dmy("01/07/2023"))
  expect_true(max(dates, na.rm = TRUE) <= dmy("30/06/2024"))
})

test_that("readDrumlineReports returns all expected columns", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)

  expected_cols <- c("Date", "Time", "Species", "Size_cm", "Sex", "Status",
                     "Tags", "SD_region", "Beach", "DateTime", "Year", "Month",
                     "Species_Code", "Target_Shark", "Has_ID_Tag",
                     "Has_Acoustic_Tag", "Source_PDF")

  expect_true(all(expected_cols %in% names(result)))
})

test_that("No footer artifacts in extracted data", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)

  # No PUB references should appear anywhere
  all_text <- paste(result$Date, result$Time, result$Species, result$Tags,
                    result$SD_region, result$Beach, sep = " ")
  expect_false(any(str_detect(all_text, "PUB\\d")))

  # No page numbers as dates
  expect_true(all(str_detect(result$Date, "^\\d{1,2}/\\d{1,2}/\\d{4}$")))
})
