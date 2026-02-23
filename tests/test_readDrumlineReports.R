library(testthat)
library(tidyverse)
library(pdftools)
library(RUtils)

# Ensure wd is project root so codeFile() resolves correctly
if (basename(getwd()) == "tests") setwd("..")
source(codeFile("readDrumlineReports.R"))

# --- Unit tests for shared helper functions ---

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

test_that("assignWordToColumn handles words slightly left of header", {
  col_starts <- c(Date = 200L, Time = 247L, Species = 279L)

  # Date values sit 10-15px left of header in older PDFs
  expect_equal(assignWordToColumn(187, col_starts), "Date")
  expect_equal(assignWordToColumn(190, col_starts), "Date")
  expect_equal(assignWordToColumn(195, col_starts), "Date")
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

# --- Unit tests for format detection ---

test_that("detectPdfFormat identifies flat format", {
  # Simulate a page with "SD" and "Beach" in header
  page <- tibble(
    x = c(45L, 116L, 155L, 412L, 485L),
    y = rep(100L, 5),
    width = rep(20L, 5),
    height = rep(12L, 5),
    space = rep(FALSE, 5),
    text = c("Date", "Species", "Status", "SD", "Beach")
  )
  expect_equal(detectPdfFormat(list(page)), "flat")
})

test_that("detectPdfFormat identifies hierarchical format", {
  page <- tibble(
    x = c(75L, 200L, 279L, 453L),
    y = rep(175L, 4),
    width = rep(20L, 4),
    height = rep(12L, 4),
    space = rep(FALSE, 4),
    text = c("Location", "Date", "Species", "Status")
  )
  expect_equal(detectPdfFormat(list(page)), "hierarchical")
})

test_that("detectPdfFormat skips pages without headers", {
  cover_page <- tibble(
    x = c(100L, 200L),
    y = rep(50L, 2),
    width = rep(20L, 2),
    height = rep(12L, 2),
    space = rep(FALSE, 2),
    text = c("Annual", "Report")
  )
  data_page <- tibble(
    x = c(75L, 200L, 279L, 453L),
    y = rep(175L, 4),
    width = rep(20L, 4),
    height = rep(12L, 4),
    space = rep(FALSE, 4),
    text = c("Location", "Date", "Species", "Status")
  )
  expect_equal(detectPdfFormat(list(cover_page, data_page)), "hierarchical")
})

# --- Unit tests for hierarchical row classification ---

test_that("classifyRow identifies location rows", {
  date_x <- 200L
  # Left-margin text only, no date, no data-area dashes
  row <- tibble(x = c(75L, 115L), y = c(200L, 200L),
                text = c("Evans", "Head"))
  expect_equal(classifyRow(row, date_x), "location")
})

test_that("classifyRow identifies location rows with dashes in name", {
  date_x <- 200L
  # "Manly - Manly to Turimetta" — dash in LEFT margin, not data area
  row <- tibble(x = c(75L, 103L, 110L, 138L, 149L),
                y = rep(477L, 5),
                text = c("Manly", "-", "Manly", "to", "Turimetta"))
  expect_equal(classifyRow(row, date_x), "location")
})

test_that("classifyRow identifies beach_empty rows", {
  date_x <- 200L
  # Beach name + dashes in data area
  row <- tibble(x = c(75L, 224L, 262L, 272L, 387L, 428L, 449L, 510L),
                y = rep(417L, 8),
                text = c("Cabarita", "-", "-", "-", "-", "-", "-", "-"))
  expect_equal(classifyRow(row, date_x), "beach_empty")
})

test_that("classifyRow identifies beach_with_data rows", {
  date_x <- 200L
  # Beach name in left margin + date in data area
  row <- tibble(x = c(75L, 114L, 187L, 247L, 279L),
                y = rep(219L, 5),
                text = c("Hastings", "Beach", "31/07/2022", "14:39", "Shark"))
  expect_equal(classifyRow(row, date_x), "beach_with_data")
})

test_that("classifyRow identifies data rows", {
  date_x <- 200L
  # Date in data area, nothing in left margin
  row <- tibble(x = c(187L, 247L, 279L, 394L),
                y = rep(232L, 4),
                text = c("28/08/2022", "11:11", "White", "265"))
  expect_equal(classifyRow(row, date_x), "data")
})

test_that("classifyRow identifies drumline_count rows", {
  date_x <- 200L
  # "2 SMART drumlines" text
  row <- tibble(x = c(75L, 83L, 116L), y = rep(232L, 3),
                text = c("2", "SMART", "drumlines"))
  expect_equal(classifyRow(row, date_x), "drumline_count")
})

test_that("classifyRow returns skip for page numbers", {
  date_x <- 200L
  row <- tibble(x = 553L, y = 800L, text = "22")
  expect_equal(classifyRow(row, date_x), "skip")
})

# --- Unit tests for left margin and non-dash text extraction ---

test_that("extractLeftMarginText extracts words left of date column", {
  date_x <- 200L
  row <- tibble(x = c(75L, 114L, 187L, 247L), y = rep(219L, 4),
                text = c("Hastings", "Beach", "31/07/2022", "14:39"))
  expect_equal(extractLeftMarginText(row, date_x), "Hastings Beach")
})

test_that("extractLeftMarginText returns NA when no left words", {
  date_x <- 200L
  row <- tibble(x = c(187L, 247L), y = rep(232L, 2),
                text = c("31/07/2022", "14:39"))
  expect_true(is.na(extractLeftMarginText(row, date_x)))
})

test_that("extractNonDashText removes dashes from beach_empty rows", {
  row <- tibble(x = c(75L, 224L, 262L, 272L, 387L),
                y = rep(417L, 5),
                text = c("Cabarita", "-", "-", "-", "-"))
  expect_equal(extractNonDashText(row), "Cabarita")
})

test_that("extractNonDashText handles multi-word beach names", {
  row <- tibble(x = c(75L, 102L, 224L, 262L, 272L),
                y = rep(417L, 5),
                text = c("South", "Narrabeen", "-", "-", "-"))
  expect_equal(extractNonDashText(row), "South Narrabeen")
})

# --- Unit tests for classifyAndExtractRows context tracking ---

test_that("classifyAndExtractRows carries location context to data rows", {
  date_x <- 200L
  col_starts <- c(Date = 200L, Time = 247L, Species = 279L, Size_cm = 394L,
                  Sex = 429L, Status = 453L, Tags = 508L)

  location_row <- tibble(x = c(75L, 115L), y = rep(200L, 2),
                         width = rep(20L, 2), height = rep(12L, 2),
                         space = rep(FALSE, 2),
                         text = c("Evans", "Head"))
  beach_row <- tibble(x = c(75L, 98L, 195L, 250L, 279L, 394L, 429L, 453L, 508L),
                      y = rep(220L, 9),
                      width = rep(20L, 9), height = rep(12L, 9),
                      space = rep(FALSE, 9),
                      text = c("Main", "Beach", "1/08/2022", "10:00", "White", "265", "F", "Alive", "ID"))
  data_row <- tibble(x = c(195L, 250L, 279L, 305L, 394L, 429L, 453L, 508L),
                     y = rep(240L, 8),
                     width = rep(20L, 8), height = rep(12L, 8),
                     space = rep(FALSE, 8),
                     text = c("2/08/2022", "11:00", "Tiger", "Shark", "300", "M", "Alive", "ID"))

  result <- classifyAndExtractRows(
    list(location_row, beach_row, data_row),
    col_starts, NA_character_, NA_character_
  )

  expect_equal(length(result$records), 2)
  expect_equal(result$records[[1]]$SD_region, "Evans Head")
  expect_equal(result$records[[1]]$Beach, "Main Beach")
  expect_equal(result$records[[2]]$SD_region, "Evans Head")
  expect_equal(result$records[[2]]$Beach, "Main Beach")
})

test_that("classifyAndExtractRows concatenates multi-line location names", {
  col_starts <- c(Date = 200L, Time = 247L, Species = 279L, Size_cm = 394L,
                  Sex = 429L, Status = 453L, Tags = 508L)

  line1 <- tibble(x = c(75L, 105L, 116L), y = rep(524L, 3),
                  width = rep(20L, 3), height = rep(12L, 3),
                  space = rep(FALSE, 3),
                  text = c("Ballina", "to", "Lennox"))
  line2 <- tibble(x = 75L, y = 537L,
                  width = 20L, height = 12L,
                  space = FALSE,
                  text = "Head")

  result <- classifyAndExtractRows(
    list(line1, line2),
    col_starts, NA_character_, NA_character_
  )

  expect_equal(result$current_location, "Ballina to Lennox Head")
})

test_that("classifyAndExtractRows concatenates beach name after beach_empty", {
  col_starts <- c(Date = 200L, Time = 247L, Species = 279L, Size_cm = 394L,
                  Sex = 429L, Status = 453L, Tags = 508L)

  beach_empty <- tibble(x = c(75L, 102L, 224L, 262L, 272L, 387L, 428L, 449L, 510L),
                        y = rep(417L, 9),
                        width = rep(20L, 9), height = rep(12L, 9),
                        space = rep(FALSE, 9),
                        text = c("South", "Narrabeen", "-", "-", "-", "-", "-", "-", "-"))
  continuation <- tibble(x = 75L, y = 430L,
                         width = 20L, height = 12L,
                         space = FALSE,
                         text = "Beach")

  result <- classifyAndExtractRows(
    list(beach_empty, continuation),
    col_starts, "Manly", NA_character_
  )

  expect_equal(result$current_beach, "South Narrabeen Beach")
  expect_equal(result$current_location, "Manly")  # unchanged
})

test_that("classifyAndExtractRows handles cross-page location continuations", {
  col_starts <- c(Date = 200L, Time = 247L, Species = 279L, Size_cm = 394L,
                  Sex = 429L, Status = 453L, Tags = 508L)

  # Simulating second page starting with a location continuation
  line2 <- tibble(x = 75L, y = 81L,
                  width = 20L, height = 12L,
                  space = FALSE,
                  text = "Beach")

  # prev_type from previous page was "location"
  result <- classifyAndExtractRows(
    list(line2),
    col_starts, "Manly - Manly to Turimetta", NA_character_,
    prev_type = "location"
  )

  expect_equal(result$current_location, "Manly - Manly to Turimetta Beach")
})

# --- Unit tests for findColumnRangesHierarchical ---

test_that("findColumnRangesHierarchical detects hierarchical header", {
  page <- tibble(
    x = c(75L, 200L, 247L, 279L, 394L, 429L, 453L, 508L, 394L),
    y = c(175L, 175L, 175L, 175L, 166L, 175L, 175L, 175L, 182L),
    width = rep(20L, 9),
    height = rep(12L, 9),
    space = rep(FALSE, 9),
    text = c("Location", "Date", "Time", "Species", "Size", "Sex", "Status", "Tags", "(cm)")
  )

  result <- findColumnRangesHierarchical(page)

  expect_false(is.null(result))
  expect_true(all(c("Date", "Time", "Species", "Size_cm", "Sex", "Status", "Tags")
                  %in% names(result$col_starts)))
  # Location should NOT be in col_starts (it's the left-margin context)
  expect_false("Location" %in% names(result$col_starts))
  expect_equal(result$date_x, 200L)
})

test_that("findColumnRangesHierarchical returns NULL for flat header", {
  page <- tibble(
    x = c(45L, 116L, 155L, 412L, 485L),
    y = rep(100L, 5),
    width = rep(20L, 5),
    height = rep(12L, 5),
    space = rep(FALSE, 5),
    text = c("Date", "Time", "Species", "SD", "Beach")
  )
  # No "Location" word, so should return NULL
  expect_null(findColumnRangesHierarchical(page))
})

# --- Integration tests against real 2023-24 PDF (flat format) ---

test_that("findColumnRanges detects headers on a data page", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")
  pages <- pdf_data(pdf_path)
  col_ranges <- findColumnRanges(pages[[13]])

  expect_false(is.null(col_ranges))
  expect_true("col_starts" %in% names(col_ranges))
  expect_true("header_y" %in% names(col_ranges))
  expect_true(all(c("Date", "Time", "Species", "Sex", "Status", "Tags",
                     "SD_region", "Beach") %in% names(col_ranges$col_starts)))
})

test_that("findColumnRanges returns NULL for non-data pages", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")
  pages <- pdf_data(pdf_path)
  col_ranges <- findColumnRanges(pages[[1]])
  expect_null(col_ranges)
})

test_that("extractAppendix2 produces expected row count", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)
  expect_gt(nrow(result), 1100)
  expect_lt(nrow(result), 1400)
})

test_that("Species names are complete (not truncated) in 2023-24 PDF", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)
  species <- unique(result$Species)
  expect_true("White Shark" %in% species)
  expect_true("Tiger Shark" %in% species)
  expect_true("Bull Shark" %in% species)
  expect_true("Shortfin Mako Shark" %in% species)
  expect_true("Smooth Hammerhead" %in% species)
  expect_true("Common Blacktip" %in% species)

  # Truncated names must NOT appear
  expect_false("White" %in% species)
  expect_false("Shortfin" %in% species)
  expect_false("Smooth" %in% species)
})

test_that("Size_cm is numeric and Sex is not swapped with Size", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)
  expect_true(is.numeric(result$Size_cm))
  valid_sizes <- result |> filter(!is.na(Size_cm))
  expect_true(all(valid_sizes$Size_cm >= 10 & valid_sizes$Size_cm <= 500))

  valid_sex <- result |> filter(!is.na(Sex)) |> pull(Sex)
  expect_true(all(valid_sex %in% c("F", "M")))
})

test_that("SD_region does not contain beach names in 2023-24 PDF", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)
  sd_regions <- unique(result$SD_region)
  expect_false(any(str_detect(sd_regions, "Beach")))
  expect_false(any(str_detect(sd_regions, "Point$")))
})

test_that("Tags field parses correctly with ID and acoustic tags", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  white_sharks <- result |> filter(Species == "White Shark")
  expect_true(mean(white_sharks$Has_ID_Tag) > 0.9)
  expect_true(mean(white_sharks$Has_Acoustic_Tag) > 0.9)

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

test_that("No footer artifacts in extracted data", {
  pdf_path <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- extractAppendix2(pdf_path)
  all_text <- paste(result$Date, result$Time, result$Species, result$Tags,
                    result$SD_region, result$Beach, sep = " ")
  expect_false(any(str_detect(all_text, "PUB\\d")))
  expect_true(all(str_detect(result$Date, "^\\d{1,2}/\\d{1,2}/\\d{4}$")))
})

# --- Integration tests against 2022-23 PDF (hierarchical) ---

test_that("extractTable3 produces expected row count for 2022-23 PDF", {
  pdf_path <- "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  expect_gt(nrow(result), 1300)
  expect_lt(nrow(result), 1600)
})

test_that("2022-23 PDF has correct date range", {
  pdf_path <- "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  dates <- dmy(result$Date)
  expect_true(min(dates, na.rm = TRUE) >= dmy("01/07/2022"))
  expect_true(max(dates, na.rm = TRUE) <= dmy("30/06/2023"))
})

test_that("2022-23 PDF has SD_region and Beach for all rows", {
  pdf_path <- "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  expect_equal(sum(is.na(result$SD_region)), 0)
  expect_equal(sum(is.na(result$Beach)), 0)
})

test_that("Species names are complete in 2022-23 PDF", {
  pdf_path <- "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  species <- unique(result$Species)
  expect_true("White Shark" %in% species)
  expect_true("Tiger Shark" %in% species)
  expect_true("Bull Shark" %in% species)

  # No single-word truncations
  expect_false("White" %in% species)
  expect_false("Tiger" %in% species)
})

# --- Integration tests against 2021-22 PDF (hierarchical) ---

test_that("extractTable3 produces expected row count for 2021-22 PDF", {
  pdf_path <- "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  expect_gt(nrow(result), 850)
  expect_lt(nrow(result), 1100)
})

test_that("2021-22 PDF has correct date range", {
  pdf_path <- "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  dates <- dmy(result$Date)
  expect_true(min(dates, na.rm = TRUE) >= dmy("01/07/2021"))
  expect_true(max(dates, na.rm = TRUE) <= dmy("30/06/2022"))
})

test_that("2021-22 PDF has SD_region and Beach for all rows", {
  pdf_path <- "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  expect_equal(sum(is.na(result$SD_region)), 0)
  expect_equal(sum(is.na(result$Beach)), 0)
})

test_that("2021-22 location names are not truncated", {
  pdf_path <- "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf"
  skip_if(!file.exists(pdf_path), "PDF not available")

  result <- readDrumlineReports(pdf_path)
  locations <- unique(result$SD_region)

  # Multi-word locations must be complete
  expect_true("Ballina to Lennox Head" %in% locations)
  expect_true("Evans Head" %in% locations)

  # Truncated single-word fragments must NOT appear as locations
  expect_false("Head" %in% locations)
  expect_false("Beach" %in% locations)
  expect_false("Bay" %in% locations)
})

# --- Cross-format consistency tests ---

test_that("readDrumlineReports returns identical column structure for all formats", {
  pdfs <- c(
    "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf",
    "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf",
    "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  )
  available <- pdfs[file.exists(pdfs)]
  skip_if(length(available) < 2, "Need at least 2 PDFs for cross-format test")

  results <- map(available, readDrumlineReports)
  col_names <- map(results, names)

  # All should have the same columns
  for (i in 2:length(col_names)) {
    expect_equal(col_names[[1]], col_names[[i]],
                 label = sprintf("Column mismatch: %s vs %s",
                                 basename(available[1]), basename(available[i])))
  }
})

test_that("All 3 PDFs can be concatenated without column issues", {
  pdfs <- c(
    "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf",
    "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf",
    "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
  )
  available <- pdfs[file.exists(pdfs)]
  skip_if(length(available) < 3, "Need all 3 PDFs for concatenation test")

  results <- map(available, readDrumlineReports)
  combined <- list_rbind(results)

  expect_gt(nrow(combined), 3000)
  expect_equal(sum(is.na(combined$Date)), 0)
  expect_equal(sum(is.na(combined$SD_region)), 0)

  # Date range should span all 3 years
  dates <- dmy(combined$Date)
  expect_true(min(dates, na.rm = TRUE) < dmy("01/01/2022"))
  expect_true(max(dates, na.rm = TRUE) > dmy("01/01/2024"))
})

test_that("detectPdfFormat correctly identifies real PDFs", {
  pdfs <- c(
    flat = "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf",
    hier1 = "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf",
    hier2 = "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf"
  )

  for (name in names(pdfs)) {
    skip_if(!file.exists(pdfs[name]), paste("PDF not available:", name))
    pages <- pdf_data(pdfs[name])
    fmt <- detectPdfFormat(pages)
    expected <- if (name == "flat") "flat" else "hierarchical"
    expect_equal(fmt, expected, label = sprintf("Format for %s", name))
  }
})
