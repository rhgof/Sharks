library(tidyverse)

library(RUtils)

source(codeFile("processDrumlineReport.R"))
source(codeFile("chartDrumLineCaptures.R"))

{
  # Extract data from PDF and write CSV
  drumline_data <- processDrumlineReport()

  # All species by week
  weekly_chart <- chartDrumLineCaptureByPeriod(
    drumline_data,
    period = "week",
    title = "NSW SMART Drumline Captures by Week",
    subtitle = "All species, 2023-24 season"
  )
  saveHorizChart("drumline_all_species_weekly", weekly_chart)

  # All species by month
  monthly_chart <- chartDrumLineCaptureByPeriod(
    drumline_data,
    period = "month",
    title = "NSW SMART Drumline Captures by Month",
    subtitle = "All species, 2023-24 season"
  )
  saveHorizChart("drumline_all_species_monthly", monthly_chart)

  # Target sharks vs other by week
  target_chart <- chartDrumLineTargetVsOther(
    drumline_data,
    period = "week",
    title = "NSW SMART Drumline: Target Sharks vs Other Species",
    subtitle = "White, Tiger, and Bull sharks vs all other captures, 2023-24 season"
  )
  saveHorizChart("drumline_target_vs_other_weekly", target_chart)

  message("Saved 3 charts to Outputs/")
}
