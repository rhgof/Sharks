library(tidyverse)
library(RUtils)

source(codeFile("readDrumlineReports.R"))

# CSV output filenames keyed by report period start date
DRUMLINE_CSV_NAMES <- c(
  "Inputs/NSW-SMART-Shark-Management-Alert-in-Real-Time-Drumlines-Report-July-2021-June-2022.pdf" = "20210701-drumline_cleaned.csv",
  "Inputs/NSW-SMART-drumline-report-1-July-2022-30-June-2023.pdf" = "20220701-drumline_cleaned.csv",
  "Inputs/2023-24-NSW-SMART-drumline-annual-report.pdf" = "20230701-drumline_cleaned.csv"
)

#' Process all drumline report PDFs and write per-year CSVs
#'
#' Extracts data from each PDF, writes a dated CSV per report, and returns
#' the combined tibble of all years.
#'
#' @param pdf_paths Character vector of PDF paths. Defaults to DRUMLINE_PDFS.
#' @return A tibble with all years combined
processDrumlineReports <- function(pdf_paths = DRUMLINE_PDFS) {
  all_data <- map(pdf_paths, function(pdf_path) {
    data <- readDrumlineReports(pdf_path)

    csv_name <- DRUMLINE_CSV_NAMES[pdf_path]
    if (is.na(csv_name)) {
      csv_name <- paste0(tools::file_path_sans_ext(basename(pdf_path)), "_cleaned.csv")
    }
    csv_path <- inputFile(csv_name)

    write_csv(data, csv_path)
    message(sprintf("Wrote %d rows to %s", nrow(data), csv_name))

    data
  }) |>
    list_rbind()

  message(sprintf("\nTotal: %d rows across %d reports", nrow(all_data), length(pdf_paths)))
  all_data
}
