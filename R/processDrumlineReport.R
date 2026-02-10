library(tidyverse)
library(RUtils)

source(codeFile("readDrumlineReports.R"))

#' Process drumline report: extract data from PDF and write CSV
#'
#' @return A tibble with the extracted and enriched drumline data
processDrumlineReport <- function() {
  drumline_data <- readDrumlineReports()

  write_csv(drumline_data, inputFile("Appendix_2_cleaned.csv"))
  message(sprintf("Wrote %d rows to %s", nrow(drumline_data), inputFile("Appendix_2_cleaned.csv")))

  drumline_data
}
