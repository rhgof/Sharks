library(tidyverse)
library(RUtils)

source(codeFile("getnetcdfFile.R"))

# https://portal.aodn.org.au/search

# get netcdf files for the date and daysPrior days

downloadSSTFilesDate <-function(theDate, daysPrior = 1, period="1d") {

# old
# urlPath = "https://data.aodn.org.au/"

  urlPath = "https://thredds.aodn.org.au/thredds/fileServer/"
  fileNameTemplate = "IMOS/SRS/SST/ghrsst/L3SM-<PERIOD>/dn/<YEAR>/<YEAR><MONTH><DAY>092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-<PERIOD>_dn.nc"

  fileTemplate = str_c(urlPath,fileNameTemplate)

#    if (year(theDate) >=2026 ) {
#     fileTemplate = "https://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SST/ghrsst/L3SM-<PERIOD>/dn/<YEAR>/<YEAR><MONTH><DAY>092000-ABOM-L3S_GHRSST-SSTfnd-MultiSensor-<PERIOD>_dn.nc"
#
#   } else {
#
#    if (period =="1d") {
#     fileTemplate <- "https://data.aodn.org.au/IMOS/SRS/SST/ghrsst/L3S-1d/dn/<YEAR>/<YEAR><MONTH><DAY>092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn.nc"
#   } else {
#     fileTemplate <- "https://data.aodn.org.au/IMOS/SRS/SST/ghrsst/L3S-6d/dn/<YEAR>/<YEAR><MONTH><DAY>212000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-6d_dn.nc"
#   }
# }

  for (i in c(0:daysPrior)) {
    downloadDate = theDate - days(i)

    y = format(downloadDate,"%Y")
    m = format(downloadDate,"%m")
    d = format(downloadDate,"%d")
    ncURL = str_replace_all(fileTemplate,"<YEAR>",y)
    ncURL = str_replace_all(ncURL,"<MONTH>",m)
    ncURL = str_replace_all(ncURL,"<DAY>",d)
    ncURL = str_replace_all(ncURL,"<PERIOD>",period)
    ncURL
    getNetcdfFileName(ncURL)
  }
}

testIMOS <- function() {
#  url = "https://data.aodn.org.au/?prefix=IMOS/SRS/SST/ghrsst/L3S-1d/dn/2023/"
 url = "https://thredds.aodn.org.au/thredds/catalog/IMOS/SRS/SST/ghrsst/L3S-1d/dn/2023/catalog.html"
    d <- url(description = url)
  l <- read_lines(d)

l
}
