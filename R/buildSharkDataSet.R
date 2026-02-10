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
  filter(UIN %in% c(1193))

# Pre-download and cache all satellite data
for (i in 1:nrow(incidents)) {
  incident <- incidents[i, ]
  downloadSSTFilesDate(incident$Incident.Date, daysPrior = 3)
  downloadCHLFilesDate(incident$Incident.Date, daysPrior = 6)
}

range_degrees <- 1
attack_degrees <- 0.2

attack_profiles <- NULL
for (i in 1:nrow(incidents)) {
  inc <- incidents[i, ]
  message(paste(inc$UIN, inc$Incident.Date, inc$Location))
  date <- inc$Incident.Date
  lat <- inc$Latitude
  lon <- inc$Longitude

  result <- extractOceanographicStats(
    date = date, lat = lat, lon = lon,
    chl_days_prior = 6, sst_days_prior = 3,
    range_degrees = range_degrees, attack_degrees = attack_degrees
  )
  if (is.null(result)) next

  chl_rast <- result$chl_rast
  sst_rast <- result$sst_rast
  s <- result$stats

  # --- CHL chart ---
  title <- paste(inc$Incident.Date, str_to_title(inc$Location))
  subtitle <- paste(
    "Chlorophyll Concentration mg/m^3: Mean", round(s$CHL_mean, 1),
    "Max", round(s$CHL_max, 1), "Min", round(s$CHL_min, 1),
    "\nNear Attack: Mean", round(s$CHLAttack_mean, 1),
    "Max", round(s$CHLAttack_max, 1), "Min", round(s$CHLAttack_min, 1)
  )
  chl_chart <- chartCHL(chl_rast, title, subtitle, useLog = TRUE) +
    geom_point(aes(x = lon, y = lat), size = 3, color = "black") +
    geom_rect(aes(xmin = lon - attack_degrees, xmax = lon + attack_degrees,
                  ymin = lat - attack_degrees, ymax = lat + attack_degrees),
              linewidth = 0.1, color = "black", fill = NA)

  # --- SST chart ---
  set.names(sst_rast, "sea_surface_temperature")
  title <- paste(inc$Incident.Date, str_to_title(inc$Location))
  subtitle <- paste(
    "Sea Surface Temperature \u00B0C: Mean", round(s$SST_mean, 1),
    "Max", round(s$SST_max, 1), "Min", round(s$SST_min, 1),
    "\nNear Attack: Mean", round(s$SSTAttack_mean, 1),
    "Max", round(s$SSTAttack_max, 1), "Min", round(s$SSTAttack_min, 1)
  )
  sst_chart <- chartSST(sst_rast, title, subtitle) +
    geom_point(aes(x = lon, y = lat), size = 3, color = "black") +
    geom_rect(aes(xmin = lon - attack_degrees, xmax = lon + attack_degrees,
                  ymin = lat - attack_degrees, ymax = lat + attack_degrees),
              linewidth = 0.1, color = "black", fill = NA)

  # --- Combined chart ---
  combined_chart <- sst_chart + chl_chart
  save_file <- paste0(
    str_replace_all(
      paste(inc$Incident.Date, "SST-CHL",
            paste0(round(range_degrees, 1), "-degree"),
            str_replace_all(str_to_title(inc$Location), ",", ""),
            sep = "_"),
      " ", "-"),
    ".png"
  )
  saveHorizChart(save_file, combined_chart)

  # --- Build profile row ---
  attack_profile <- result$stats |>
    mutate(
      UIN = inc$UIN, IncidentDate = date, State = inc$State,
      Location = inc$Location, Lat = lat, Lon = lon,
      SharkCommonName = inc$Shark.common.name,
      .before = 1
    )

  attack_profiles <- attack_profiles |>
    bind_rows(attack_profile)
}

write_csv(attack_profiles, inputFile(paste0("SharkIncidents-SST-CHL-", range_degrees, "Deg.csv")))
