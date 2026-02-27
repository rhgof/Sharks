library(tidyverse)

library(RUtils)

source(codeFile("processDrumlineReport.R"))
source(codeFile("chartDrumLineCaptures.R"))
source(codeFile("chartDrumLineByRegion.R"))

{
  # Extract data from PDF and write CSV
  drumline_data <- processDrumlineReports()

  year_range <- range(drumline_data$Year, na.rm = TRUE)
  season_label <- if (year_range[1] == year_range[2]) {
    paste0(year_range[1], " season")
  } else {
    paste0(year_range[1], "-", substr(year_range[2], 3, 4), " seasons")
  }

  # All species by week
  weekly_chart <- chartDrumLineCaptureByPeriod(
    drumline_data,
    period = "week",
    title = "NSW SMART Drumline Captures by Week",
    subtitle = paste("All species,", season_label)
  )
  saveHorizChart("drumline_all_species_weekly", weekly_chart)

  # All species by month
  monthly_chart <- chartDrumLineCaptureByPeriod(
    drumline_data,
    period = "month",
    title = "NSW SMART Drumline Captures by Month",
    subtitle = paste("All species,", season_label)
  )
  saveHorizChart("drumline_all_species_monthly", monthly_chart)

  # Target sharks vs other by week
  target_chart <- chartDrumLineTargetVsOther(
    drumline_data,
    period = "week",
    title = "NSW SMART Drumline: Target Sharks vs Other Species",
    subtitle = paste("White, Tiger, and Bull sharks vs all other captures,", season_label)
  )
  saveHorizChart("drumline_target_vs_other_weekly", target_chart)

  # Captures by SD region (north to south)
  region_chart <- chartDrumLineCaptureByRegion(
    drumline_data,
    title = "NSW SMART Drumline Captures by Region",
    subtitle = paste("All species,", season_label)
  )
  saveHorizChart("drumline_captures_by_region", region_chart)

  # Target sharks vs other by region
  region_target_chart <- chartDrumLineRegionTargetVsOther(
    drumline_data,
    title = "NSW SMART Drumline: Target Sharks by Region",
    subtitle = paste("White, Tiger, and Bull sharks vs all other captures,", season_label)
  )
  saveHorizChart("drumline_target_vs_other_by_region", region_target_chart)

  message("Saved 5 charts to Outputs/")
}
