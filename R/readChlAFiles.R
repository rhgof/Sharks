library(tidyverse)
library(RUtils)

# Reference
# https://oceancolor.gsfc.nasa.gov/about/
# https://hermes.acri.fr/index.php?class=archive

ncFiles <- function(directory = NULL,ObsPeriod = "month") {
  if (is.null(directory)) {
    directory = "/Volumes/Samples/InputData/Hermes"
  }

 listing <- dir(directory,full.names = FALSE)

 # File Naming
 # L3m_20041201-20041231__211308274_4_GSM-MERMODSWF_CHL1_MO_00.nc
 # L3m_20041101-20041130__211308274_4_AVW-MERMODSWF_CHL1_8D_00.nc

   theFiles <- as_tibble(listing) |>
     rename(FileName = value) |>
     filter(str_detect(FileName,"^L3m") ) |>
     mutate(Path = directory) |>
     mutate(StartDate = str_extract(FileName, "_(\\d{8})",group=1)) |>
     mutate(Gridkm = str_extract(FileName, "_(\\d{1,2})_",group=1)) |>
     mutate(MethodShort = str_extract(FileName, "_([:alpha:]+)-[:alpha:]+_",group=1)) |>
     mutate(MethodLong = str_extract(FileName, "_([:alpha:]+-[:alpha:]+)_",group=1)) |>
     mutate(Period = str_extract(FileName,"_([:alnum:]+)_\\d+\\.nc",group=1)) |>
     mutate(Channel = str_extract(FileName,"_([:alpha:]+)\\d*_[:alnum:]+_\\d+\\.nc",group=1))

     theFiles <- theFiles |>
       mutate(EndDate = case_when (
         Period == "8D" ~ str_extract(FileName, "-(\\d{8})__",group=1),
         Period == "MO" ~ str_extract(FileName, "-(\\d{8})__",group=1),
         Period == "DAY" ~ str_extract(FileName, "_(\\d{8})",group=1),
         .default = str_extract(FileName, "-(\\d{8})__",group=1)
       )
       )

  # Type Conversions and set up Intervals for Search eg:
  # filter(theFiles,(today()-months(4)) %within% Interval)

  theFiles <- theFiles |>
    mutate(StartDate =  ymd(StartDate)) |>
    mutate(EndDate = ymd(EndDate)) |>
    mutate(Interval = StartDate %--% EndDate) |>
    mutate(PeriodLong = case_when(
      Period == "8D" ~ "8 Days",
      Period == "MO" ~ "Monthly",
      Period == "DAY" ~ "Daily",
      .default =  "Unknown"
    )) |>
    mutate(Gridkm = as.integer(Gridkm)) |>
    mutate(Title = paste(paste0(StartDate," to ",EndDate),paste0(Gridkm,"km"),PeriodLong,MethodShort,Channel))

  # check number for each date
  countFiles <- theFiles |>
    group_by(StartDate) |>
    reframe(n=n())

  return (theFiles)

}
