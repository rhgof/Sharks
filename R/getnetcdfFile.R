library(RUtils)

getNetcdfFileName <- function(urlPath,extDrive = TRUE) {

  baseName = basename(urlPath)
  fileName = NULL
  #print(urlPath)
  tryCatch(
    expr = {
      fileName <- readCachedFile(urlPath, uniqueName = baseName,period = years(1), extDrive)
    },

    error = function(e) {
      print(paste("Error:",baseName,"does not exist -",e))
      # should try
      fileName = NULL
    },
    warning = function(e) {
      print(paste("Warning:",baseName,"does not exist- ",e))
      # should try
      fileName = NULL
    },

    finally = {}
  )

  return(fileName)
}
