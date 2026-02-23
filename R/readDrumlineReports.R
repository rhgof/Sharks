library(pdftools)
library(tidyverse)

# Species code mapping
SPECIES_CODES <- c(
  "White Shark" = "WHI",
  "Tiger Shark" = "TIG",
  "Bull Shark" = "BUL",
  "Dusky Whaler" = "DUS",
  "Bronze Whaler" = "BRO",
  "Common Blacktip" = "CBT",
  "Greynurse Shark" = "GRN",
  "Smooth Hammerhead" = "SMH",
  "Scalloped Hammerhead" = "SCH",
  "Scalloped Hammer Head" = "SCH",
  "Great Hammerhead" = "GHH",
  "Shortfin Mako Shark" = "SMA",
  "Shortfin Mako" = "SMA",
  "Mako" = "SMA",
  "Sandbar Whaler" = "SAN",
  "Sandbar Shark" = "SAN",
  "Thresher Shark" = "THR",
  "Spinner Shark" = "SPN",
  "Manta Ray" = "MAN",
  "Black Ray" = "BLR",
  "Black/Bull Ray" = "BLR",
  "Loggerhead Turtle" = "LOG",
  "Loggerhead turtle" = "LOG",
  "Leatherback Turtle" = "LBT",
  "Snapper" = "SNP",
  "Tailor" = "TLR",
  "Seal" = "SEL",
  "Queensland Groper" = "QGR",
  "Sevengill Shark" = "SVG",
  "School Shark" = "SCL",
  "Blue Shark" = "BLU",
  "Gummy Shark" = "GUM",
  "Black Marlin" = "BKM",
  "Whitespotted Guitarfish" = "WSG",
  "Whaler - unidentified" = "WHL",
  "Unidentified Whaler" = "WHL",
  "Unidentified whaler" = "WHL",
  "Broadnose shark" = "BNS",
  "Lemon Shark" = "LEM",
  "Silky Whaler" = "SLK"
)

TARGET_SHARKS <- c("White Shark", "Tiger Shark", "Bull Shark")

# Default PDF path for the 2023-24 report
# https://www.sharksmart.nsw.gov.au/technology-trials-and-research/smart-drumlines/nsw-current-program-smart-drumline-data
DEFAULT_PDF <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"

# All available drumline report PDFs (oldest to newest)
DRUMLINE_PDFS <- c(
  "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf",
  "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf",
  "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"
)

#' Read drumline reports from a NSW SMART drumline PDF
#'
#' Auto-detects PDF format (flat Appendix 2 vs hierarchical Table 3) and
#' extracts catch data using coordinate-based word parsing via
#' pdftools::pdf_data(). Returns a tibble with raw and derived columns.
#'
#' @param pdf_path Path to a drumline report PDF. Defaults to the 2023-24 report.
#' @return A tibble with columns: Date, Time, Species, Size_cm, Sex, Status,
#'   Tags, SD_region, Beach, DateTime, Year, Month, Species_Code,
#'   Target_Shark, Has_ID_Tag, Has_Acoustic_Tag, Source_PDF
readDrumlineReports <- function(pdf_path = NULL) {
  if (is.null(pdf_path)) pdf_path <- DEFAULT_PDF
  stopifnot(file.exists(pdf_path))

  pages <- pdf_data(pdf_path)
  format <- detectPdfFormat(pages)

  raw <- switch(format,
    flat = extractAppendix2(pdf_path),
    hierarchical = extractTable3(pdf_path, pages)
  )

  enriched <- enrichDrumlineData(raw, pdf_path)

  # Validation summary
  message(sprintf("Extracted %d rows from %s", nrow(enriched), basename(pdf_path)))
  species_counts <- enriched |>
    count(Species, sort = TRUE)
  message("\nSpecies counts (for validation vs Appendix 1):")
  message(paste(capture.output(print(species_counts, n = Inf)), collapse = "\n"))

  enriched
}

#' Extract Appendix 2 data from a single PDF
#'
#' @param pdf_path Path to the PDF file
#' @return A tibble with 9 raw columns
extractAppendix2 <- function(pdf_path) {
  pages <- pdf_data(pdf_path)

  all_rows <- map(seq_along(pages), function(i) {
    page <- pages[[i]]
    extractPageData(page, page_num = i)
  }) |>
    list_rbind()

  all_rows
}

#' Detect whether a PDF uses flat (Appendix 2) or hierarchical (Table 3) format
#'
#' Scans page headers for "Location" (hierarchical) vs "SD"/"Beach" (flat).
#'
#' @param pages List of page tibbles from pdf_data()
#' @return "flat" or "hierarchical"
detectPdfFormat <- function(pages) {
  for (page in pages) {
    date_words <- page |> filter(text == "Date")
    if (nrow(date_words) == 0) next

    header_y <- date_words$y[1]
    header_words <- page |>
      filter(abs(y - header_y) <= 3) |>
      pull(text)

    if ("Location" %in% header_words) return("hierarchical")
    if ("SD" %in% header_words || "Beach" %in% header_words) return("flat")
  }

  stop("Could not detect PDF format: no recognizable table header found")
}

# ---- Hierarchical (Table 3) extraction for 2021-22 and 2022-23 PDFs ----

#' Extract Table 3 data from a hierarchical-format PDF
#'
#' The older PDFs use Location/Beach as left-margin group labels rather than
#' columns. This function carries Location and Beach context across rows
#' and pages to produce the same 9-column output as extractAppendix2().
#'
#' @param pdf_path Path to the PDF file
#' @param pages Pre-loaded pages from pdf_data() (optional, avoids re-reading)
#' @return A tibble with 9 raw columns matching extractAppendix2() output
extractTable3 <- function(pdf_path, pages = NULL) {
  if (is.null(pages)) pages <- pdf_data(pdf_path)

  # Find the first page with a hierarchical header
  header_info <- NULL
  header_page <- NA_integer_
  for (i in seq_along(pages)) {
    header_info <- findColumnRangesHierarchical(pages[[i]])
    if (!is.null(header_info)) {
      header_page <- i
      break
    }
  }

  if (is.null(header_info)) {
    stop("Could not find Table 3 header in ", basename(pdf_path))
  }

  # Process all pages from header onward, carrying location/beach context
  current_location <- NA_character_
  current_beach <- NA_character_
  prev_type <- "skip"
  all_records <- list()

  for (i in header_page:length(pages)) {
    page <- pages[[i]]

    # On the header page, filter below header; on subsequent pages, use all data
    if (i == header_page) {
      min_y <- header_info$header_y + 10
    } else {
      min_y <- 0
    }

    # Exclude footer text (page numbers at very bottom)
    data_words <- page |>
      filter(y > min_y, y < max(page$y) - 5)

    if (nrow(data_words) == 0) next

    row_groups <- groupWordsIntoRows(data_words, y_tolerance = 5)

    result <- classifyAndExtractRows(
      row_groups, header_info$col_starts,
      current_location, current_beach, prev_type
    )

    all_records <- c(all_records, result$records)
    current_location <- result$current_location
    current_beach <- result$current_beach
    prev_type <- result$prev_type
  }

  if (length(all_records) == 0) return(tibble())

  list_rbind(all_records)
}

#' Find column x-boundaries for a hierarchical (Table 3) header
#'
#' Expects columns: Location, Date, Time, Species, Size (cm), Sex, Status, Tags.
#' Returns the 7 data column positions (excluding Location, which is used for
#' left-margin labels).
#'
#' @param page_data A tibble from pdf_data() for one page
#' @return A list with col_starts and header_y, or NULL if no header found
findColumnRangesHierarchical <- function(page_data) {
  date_words <- page_data |> filter(text == "Date")
  if (nrow(date_words) == 0) return(NULL)

  header_y <- date_words$y[1]
  header_words <- page_data |>
    filter(abs(y - header_y) <= 3) |>
    arrange(x)

  # Must have "Location" to confirm hierarchical format
  header_texts <- header_words$text
  if (!("Location" %in% header_texts)) return(NULL)
  if (!all(c("Date", "Species", "Status") %in% header_texts)) return(NULL)

  get_x <- function(word) {
    w <- header_words |> filter(text == word)
    if (nrow(w) == 0) return(NA_integer_)
    w$x[1]
  }

  date_x <- get_x("Date")
  time_x <- get_x("Time")
  species_x <- get_x("Species")
  sex_x <- get_x("Sex")
  status_x <- get_x("Status")
  tags_x <- get_x("Tags")

  # Size header may be on a different y-line
  size_words <- page_data |>
    filter(text == "Size" | text == "(cm)")
  size_x <- if (nrow(size_words) > 0) size_words$x[1] else NA_integer_

  col_starts <- c(
    Date = date_x,
    Time = time_x,
    Species = species_x,
    Size_cm = size_x,
    Sex = sex_x,
    Status = status_x,
    Tags = tags_x
  )

  col_starts <- col_starts[!is.na(col_starts)]

  list(
    col_starts = col_starts,
    header_y = header_y,
    date_x = date_x
  )
}

#' Classify a row group from a hierarchical table
#'
#' @param row A tibble of words for one visual row
#' @param date_x The x-position of the Date column header
#' @return One of: "location", "beach_empty", "beach_with_data", "data",
#'   "drumline_count", "skip"
classifyRow <- function(row, date_x) {
  has_smart <- any(str_detect(row$text, "SMART|drumline|drumlines"))
  if (has_smart) return("drumline_count")

  has_date <- any(str_detect(row$text, "^\\d{1,2}/\\d{1,2}/\\d{2,4}$"))
  has_left_margin <- any(row$x < date_x - 20)
  # Dashes in the DATA area (right side) indicate an empty beach row;
  # dashes in the left margin are part of location names (e.g., "Manly - Manly to Turimetta")
  data_area <- row |> filter(x >= date_x - 20)
  has_data_dash <- any(data_area$text %in% c("-", "\u2013"))

  if (has_date && has_left_margin) return("beach_with_data")
  if (has_date && !has_left_margin) return("data")
  if (has_left_margin && has_data_dash) return("beach_empty")
  if (has_left_margin && !has_date && !has_data_dash) return("location")

  "skip"
}

#' Extract left-margin text (location/beach name) from a row
#'
#' @param row A tibble of words for one visual row
#' @param date_x The x-position of the Date column header
#' @return The concatenated left-margin text, or NA if none
extractLeftMarginText <- function(row, date_x) {
  left_words <- row |>
    filter(x < date_x - 20) |>
    arrange(x) |>
    pull(text)

  if (length(left_words) == 0) return(NA_character_)
  paste(left_words, collapse = " ")
}

#' Extract all non-dash text from a row (for beach_empty rows)
#'
#' Beach names on "empty" rows (all dashes for data) can extend past the
#' date column x-position. This extracts the beach name by taking all words
#' that are not dashes or en-dashes.
#'
#' @param row A tibble of words for one visual row
#' @return The concatenated non-dash text, or NA if none
extractNonDashText <- function(row) {
  words <- row |>
    filter(!text %in% c("-", "\u2013")) |>
    arrange(x) |>
    pull(text)

  if (length(words) == 0) return(NA_character_)
  paste(words, collapse = " ")
}

#' Classify and extract records from row groups in hierarchical format
#'
#' Iterates row groups, classifies each, and maintains Location/Beach context.
#' For data rows, assigns words to columns and builds records.
#'
#' @param row_groups List of tibbles from groupWordsIntoRows()
#' @param col_starts Named vector of data column x-positions
#' @param current_location Current SD_region context (carried across pages)
#' @param current_beach Current Beach context (carried across pages)
#' @param prev_type Row type from the last row of the previous page (for cross-page continuations)
#' @return A list with: records, current_location, current_beach, prev_type
classifyAndExtractRows <- function(row_groups, col_starts, current_location, current_beach,
                                   prev_type = "skip") {
  date_x <- col_starts[["Date"]]
  records <- list()

  for (row in row_groups) {
    row_type <- classifyRow(row, date_x)

    if (row_type == "location") {
      # Location names can extend past the date column x-position,
      # so use ALL text on the row (not just left-margin)
      row_text <- paste(row$text[order(row$x)], collapse = " ")
      if (prev_type == "location") {
        # Multi-line location name continuation
        current_location <- paste(current_location, row_text)
      } else if (prev_type == "beach_empty") {
        # Continuation of a beach name (e.g., "South Narrabeen" + "Beach")
        current_beach <- paste(current_beach, row_text)
      } else {
        current_location <- row_text
        current_beach <- NA_character_
      }

    } else if (row_type == "beach_empty") {
      # Beach names with dashes can also extend past the date column
      current_beach <- extractNonDashText(row)

    } else if (row_type == "beach_with_data") {
      current_beach <- extractLeftMarginText(row, date_x)
      record <- buildHierarchicalRecord(row, col_starts, current_location, current_beach)
      if (!is.null(record)) records <- c(records, list(record))

    } else if (row_type == "data") {
      record <- buildHierarchicalRecord(row, col_starts, current_location, current_beach)
      if (!is.null(record)) records <- c(records, list(record))

    } else if (row_type == "drumline_count") {
      # Drumline count rows sometimes also contain catch data
      has_date <- any(str_detect(row$text, "^\\d{1,2}/\\d{1,2}/\\d{2,4}$"))
      if (has_date) {
        record <- buildHierarchicalRecord(row, col_starts, current_location, current_beach)
        if (!is.null(record)) records <- c(records, list(record))
      }
    }
    # "skip" rows are ignored

    prev_type <- row_type
  }

  list(
    records = records,
    current_location = current_location,
    current_beach = current_beach,
    prev_type = prev_type
  )
}

#' Build a single record from a hierarchical data row
#'
#' Assigns words to columns using x-positions, then finalizes with
#' the carried Location/Beach context.
#'
#' @param row A tibble of words for one visual row
#' @param col_starts Named vector of data column x-positions
#' @param sd_region The current SD_region (Location) context
#' @param beach The current Beach context
#' @return A 1-row tibble with 9 columns, or NULL if row has no date
buildHierarchicalRecord <- function(row, col_starts, sd_region, beach) {
  # Only use words in the data column area (at or right of Date x)
  # Data values can sit up to ~15px left of the header position
  date_x <- col_starts[["Date"]]
  data_words <- row |> filter(x >= date_x - 20)

  if (nrow(data_words) == 0) return(NULL)

  # Assign each word to a column
  word_cols <- map_chr(data_words$x, ~ assignWordToColumn(.x, col_starts))
  data_words <- data_words |> mutate(column = word_cols)

  # Reuse finalizeRecord for the 7 data columns, then inject SD_region and Beach
  record <- finalizeRecord(data_words)

  # finalizeRecord produces NAs for SD_region/Beach since those columns
  # don't exist in the hierarchical col_starts. Override with context values.
  record$SD_region <- sd_region
  record$Beach <- beach

  record
}

#' Extract data rows from a single PDF page
#'
#' @param page_data A tibble from pdf_data() for one page (x, y, width, height, text, space)
#' @param page_num Page number (for diagnostics)
#' @return A tibble with 9 raw columns, or a 0-row tibble if page has no data
extractPageData <- function(page_data, page_num = NA) {
  col_ranges <- findColumnRanges(page_data)
  if (is.null(col_ranges)) return(tibble())

  # Get all words below the header area, excluding footers (page numbers, PUB refs)
  header_y <- col_ranges$header_y
  footer_y <- page_data |>
    filter(str_detect(text, "^PUB\\d")) |>
    pull(y)
  max_y <- if (length(footer_y) > 0) min(footer_y) - 1 else Inf

  data_words <- page_data |>
    filter(y > header_y + 10, y < max_y)

  if (nrow(data_words) == 0) return(tibble())

  # Group words into rows by y-proximity
  row_groups <- groupWordsIntoRows(data_words, y_tolerance = 5)

  # Identify which row groups start a new data record (date in first column)
  # vs continuation lines (multi-word species/beach that wrap)
  records <- assembleRecords(row_groups, col_ranges)

  records
}

#' Find column x-boundaries from the header row
#'
#' Looks for a row containing "Date", "Time", "Species", etc.
#' Returns column midpoints and ranges for assigning words.
#'
#' @param page_data A tibble from pdf_data() for one page
#' @return A list with column boundaries, or NULL if no header found
findColumnRanges <- function(page_data) {
  # Find the row with "Date" — this defines the header
  date_words <- page_data |> filter(text == "Date")
  if (nrow(date_words) == 0) return(NULL)

  header_y <- date_words$y[1]

  # Get all words on the header row (within y_tolerance)
  header_words <- page_data |>
    filter(abs(y - header_y) <= 3) |>
    arrange(x)

  # We need at minimum: Date, Species, Status
  header_texts <- header_words$text
  if (!all(c("Date", "Species", "Status") %in% header_texts)) return(NULL)

  # Extract x-positions of known column headers
  get_x <- function(word) {
    w <- header_words |> filter(text == word)
    if (nrow(w) == 0) return(NA_integer_)
    w$x[1]
  }

  date_x <- get_x("Date")
  time_x <- get_x("Time")
  species_x <- get_x("Species")
  sex_x <- get_x("Sex")
  status_x <- get_x("Status")
  tags_x <- get_x("Tags")
  sd_x <- get_x("SD")         # "SD" is first word of "SD region"
  beach_x <- get_x("Beach")

  # Size header is on a different y-line (above the main header), find it
  # by looking for "(cm)" or "Size" near the Species-Sex x range
  size_words <- page_data |>
    filter(text == "Size" | text == "(cm)")
  size_x <- if (nrow(size_words) > 0) size_words$x[1] else NA_integer_

  # Build column boundary list: for each column, define [min_x, max_x)
  # We use midpoints between consecutive headers as boundaries
  col_starts <- c(
    Date = date_x,
    Time = time_x,
    Species = species_x,
    Size_cm = size_x,
    Sex = sex_x,
    Status = status_x,
    Tags = tags_x,
    SD_region = sd_x,
    Beach = beach_x
  )

  # Remove NAs and sort
  col_starts <- col_starts[!is.na(col_starts)]

  list(
    col_starts = col_starts,
    header_y = header_y
  )
}

#' Group words into rows by y-coordinate proximity
#'
#' Words on the same visual line may have slightly different y values.
#' This clusters them into rows.
#'
#' @param data_words A tibble of words (must have x, y, text columns)
#' @param y_tolerance Maximum y-pixel difference to group words into same row
#' @return A list of tibbles, each representing one visual row
groupWordsIntoRows <- function(data_words, y_tolerance = 5) {
  if (nrow(data_words) == 0) return(list())

  # Sort by y then x
  sorted <- data_words |> arrange(y, x)

  # Cluster by y-proximity
  sorted <- sorted |>
    mutate(
      y_gap = c(0, diff(y)),
      row_group = cumsum(y_gap > y_tolerance)
    )

  sorted |>
    group_by(row_group) |>
    group_split() |>
    map(~ .x |> select(-y_gap, -row_group) |> arrange(x))
}

#' Assign a word to a column based on its x-position
#'
#' @param word_x The x-coordinate of the word
#' @param col_starts Named vector of column start x-positions
#' @return The column name this word belongs to
assignWordToColumn <- function(word_x, col_starts) {
  sorted <- sort(col_starts)
  col_names <- names(sorted)

  # Find the rightmost column whose start_x <= word_x
  # Allow tolerance for data words sitting slightly left of their header
  candidates <- which(sorted <= word_x + 15)
  if (length(candidates) == 0) return(NA_character_)

  col_names[max(candidates)]
}

#' Assemble complete records from row groups
#'
#' A record starts with a row containing a date in the Date column.
#' Subsequent rows without a date are continuation lines (multi-word
#' species names, beach names, or SD regions that wrap).
#'
#' @param row_groups List of tibbles from groupWordsIntoRows()
#' @param col_ranges Column boundary info from findColumnRanges()
#' @return A tibble with 9 raw columns
assembleRecords <- function(row_groups, col_ranges) {
  if (length(row_groups) == 0) return(tibble())

  col_starts <- col_ranges$col_starts
  records <- list()
  current_record <- NULL

  for (row in row_groups) {
    # Classify each word into a column
    word_cols <- map_chr(row$x, ~ assignWordToColumn(.x, col_starts))
    row <- row |> mutate(column = word_cols)

    # Check if this row starts a new record (has a date-like value in Date column)
    date_words <- row |> filter(column == "Date")
    has_date <- nrow(date_words) > 0 &&
      any(str_detect(date_words$text, "^\\d{1,2}/\\d{1,2}/\\d{4}$"))

    # Skip non-data text (page numbers, "APPENDIX 2", etc.)
    if (!has_date && is.null(current_record)) {
      # Could be a continuation line with no preceding record — skip
      next
    }

    if (has_date) {
      # Save previous record if it exists
      if (!is.null(current_record)) {
        records <- c(records, list(finalizeRecord(current_record)))
      }
      # Start new record
      current_record <- row |> filter(!is.na(column))
    } else {
      # Continuation line — append words to current record
      continuation_words <- row |> filter(!is.na(column))
      if (!is.null(current_record) && nrow(continuation_words) > 0) {
        current_record <- bind_rows(current_record, continuation_words)
      }
    }
  }

  # Don't forget the last record
  if (!is.null(current_record)) {
    records <- c(records, list(finalizeRecord(current_record)))
  }

  if (length(records) == 0) return(tibble())

  list_rbind(records)
}

#' Finalize a single record from its collected words
#'
#' Concatenates multi-word values within each column.
#'
#' @param record_words A tibble of words with a `column` field
#' @return A 1-row tibble with 9 columns
finalizeRecord <- function(record_words) {
  collapse_col <- function(col_name) {
    words <- record_words |>
      filter(column == col_name) |>
      arrange(y, x) |>
      pull(text)
    if (length(words) == 0) return(NA_character_)
    paste(words, collapse = " ")
  }

  raw_date <- collapse_col("Date")
  raw_time <- collapse_col("Time")
  raw_species <- collapse_col("Species")
  raw_size <- collapse_col("Size_cm")
  raw_sex <- collapse_col("Sex")
  raw_status <- collapse_col("Status")
  raw_tags <- collapse_col("Tags")
  raw_sd <- collapse_col("SD_region")
  raw_beach <- collapse_col("Beach")

  # Convert NA strings to actual NA
  clean_na <- function(x) if (!is.na(x) && x == "NA") NA_character_ else x

  tibble(
    Date = raw_date,
    Time = clean_na(raw_time),
    Species = raw_species,
    Size_cm = suppressWarnings(as.numeric(raw_size)),
    Sex = clean_na(raw_sex),
    Status = raw_status,
    Tags = clean_na(raw_tags),
    SD_region = raw_sd,
    Beach = raw_beach
  )
}

#' Enrich raw drumline data with derived columns
#'
#' @param df A tibble with 9 raw columns from extractAppendix2()
#' @param pdf_path The source PDF path (for Source_PDF column)
#' @return The tibble with additional derived columns
enrichDrumlineData <- function(df, pdf_path = NA_character_) {
  df |>
    mutate(
      DateTime = dmy_hm(paste(Date, Time), quiet = TRUE),
      Year = as.integer(str_extract(Date, "\\d{4}$")),
      Month = as.integer(str_extract(Date, "(?<=/)\\d{1,2}(?=/)")),
      Species_Code = unname(SPECIES_CODES[Species]),
      Species_Code = if_else(is.na(Species_Code) & !is.na(Species), "UNK", Species_Code),
      Target_Shark = Species %in% TARGET_SHARKS,
      Has_ID_Tag = !is.na(Tags) & str_detect(Tags, "ID"),
      Has_Acoustic_Tag = !is.na(Tags) & str_detect(Tags, "\\bA\\b"),
      Source_PDF = basename(pdf_path)
    )
}
