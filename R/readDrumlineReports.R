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
  "Great Hammerhead" = "GHH",
  "Shortfin Mako Shark" = "SMA",
  "Sandbar Whaler" = "SAN",
  "Thresher Shark" = "THR",
  "Spinner Shark" = "SPN",
  "Manta Ray" = "MAN",
  "Black Ray" = "BLR",
  "Loggerhead Turtle" = "LOG",
  "Snapper" = "SNP",
  "Tailor" = "TLR",
  "Seal" = "SEL",
  "Queensland Groper" = "QGR"
)

TARGET_SHARKS <- c("White Shark", "Tiger Shark", "Bull Shark")

# Default PDF path for the 2023-24 report
DEFAULT_PDF <- "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf"

#' Read drumline reports from the 2023-24 NSW SMART drumline PDF
#'
#' Extracts Appendix 2 catch data using coordinate-based word parsing
#' via pdftools::pdf_data(). Returns a tibble with raw and derived columns.
#'
#' @param pdf_path Path to the 2023-24 PDF. Defaults to the Inputs/ copy.
#' @return A tibble with columns: Date, Time, Species, Size_cm, Sex, Status,
#'   Tags, SD_region, Beach, DateTime, Year, Month, Species_Code,
#'   Target_Shark, Has_ID_Tag, Has_Acoustic_Tag, Source_PDF
readDrumlineReports <- function(pdf_path = NULL) {
  if (is.null(pdf_path)) pdf_path <- DEFAULT_PDF
  stopifnot(file.exists(pdf_path))

  raw <- extractAppendix2(pdf_path)
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
  # Allow a small tolerance (word can be slightly left of header)
  candidates <- which(sorted <= word_x + 10)
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
