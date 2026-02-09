library(tidyverse)
library(RUtils)

source(codeFile("readDrumlineReports.R"))

drumline_data <- readDrumlineReports()

write_csv(drumline_data, inputFile("Appendix_2_cleaned.csv"))
message(sprintf("Wrote %d rows to %s", nrow(drumline_data), inputFile("Appendix_2_cleaned.csv")))
