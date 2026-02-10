library(tidyverse)
library(patchwork)
library(terra)

library(RUtils)

source(codeFile("readSharkDB.R"))
source(codeFile("downloadCHLFiles.R"))
source(codeFile("downloadSSTFiles.R"))
source(codeFile("readIMOSFileListing.R"))
source(codeFile("usefulExtents.R"))
source(codeFile("rastUtilities.R"))
source(codeFile("chartSST.R"))
source(codeFile("chartCHL.R"))

shark_incidents <- readSharkDB()

earliest_sat_date <- ymd("2002-07-01")

incidents <- shark_incidents |>
  filter(Incident.Date >= earliest_sat_date) |>
  filter(State == "NSW") |>
  filter(Shark.common.name == "white shark")

incidents <- incidents |>
  filter(year(Incident.Date) %in% c(2015:2026))

# Pre-download and cache all satellite data (30-day window)
for (i in 1:nrow(incidents)) {
  incident <- incidents[i, ]
  downloadSSTFilesDate(incident$Incident.Date, daysPrior = 30)
  downloadCHLFilesDate(incident$Incident.Date, daysPrior = 30)
}

range_degrees <- 1
attack_degrees <- 0.2

attack_profiles <- NULL
trace_days <- 30
for (i in 1:nrow(incidents)) {
  inc <- incidents[i, ]
  message(paste(inc$UIN, inc$Incident.Date, inc$Location))
  date <- inc$Incident.Date
  lat <- inc$Latitude
  lon <- inc$Longitude

  for (d in seq.Date(date - days(trace_days), date, by = "day")) {
    trace_date <- as_date(d)
    message(trace_date)

    result <- extractOceanographicStats(
      date = trace_date, lat = lat, lon = lon,
      chl_days_prior = 3, sst_days_prior = 3,
      range_degrees = range_degrees, attack_degrees = attack_degrees
    )
    if (is.null(result)) next

    attack_profile <- result$stats |>
      mutate(
        UIN = inc$UIN, IncidentDate = date, State = inc$State,
        Location = inc$Location, Lat = lat, Lon = lon,
        SharkCommonName = inc$Shark.common.name,
        .before = 1
      ) |>
      mutate(TraceDate = trace_date)

    attack_profiles <- attack_profiles |>
      bind_rows(attack_profile)
  }
}

write_csv(attack_profiles, inputFile(paste0("SharkIncidents-SST-CHL-Traces", range_degrees, "Deg.csv")))
