library(reprex)

reprex({
  R.version.string
  library(terra)
  packageVersion("terra")

  r <- rast(nrows=180, ncols=360, nlyrs=1, xmin=-180, xmax=180, ymin=-90, ymax=90)
  crs(r) <- "EPSG:7844"
  r
  set.names(r, "Test")
  names(r)

  units(r) <-"t/s"
  units(r)

  varnames(r) <-"TestVar"
  varnames(r)

  longnames(r) <- "TestLong"
  longnames(r)

  rc <- crop(r,ext(c(140,150,-50,-10)))
  rc
  # EQUAL
  names(rc) == names(r)
  units(rc) == units(r)
  crs(rc) == crs(r)
  # NOT EQUAL
  varnames(rc) == varnames(r)
  longnames(rc) == longnames(r)
}
)

reprex( {
  library(terra)
  theRast <- rast("/Volumes/Samples/InputData/cache/A.P1D.20230821T053000Z.aust.chl_gsm.nc")
  theRast

  ln <- longnames(theRast)
  vn <- varnames(theRast)
  un <- units(theRast)

  theRast <- crop(theRast,ext(c(140,150,-50,-10)))
  theRast

  # Tests
  longnames(theRast) == ln
  units(theRast) == un
  varnames(theRast) == vn

  longnames(theRast) <- ln
  varnames(theRast) <- vn
  units(theRast) <- un

  # More Test
  longnames(theRast) == ln
  units(theRast) == un
  varnames(theRast) == vn

  theRast
})


reprex({
  library(terra)

  theFiles = c("/Volumes/Samples/InputData/cache/A.P1D.20230821T053000Z.aust.chl_gsm.nc",
               "/Volumes/Samples/InputData/cache/A.P1D.20230822T053000Z.aust.chl_gsm.nc",
               "/Volumes/Samples/InputData/cache/A.P1D.20230823T053000Z.aust.chl_gsm.nc"
               )
  theRast = rast(theFiles)
  theRast
  theUnits = units(theRast)
  longNames <- longnames(theRast)
  vn <-varnames(theRast)

  theRast<-crop(theRast,ext(c(140,150,-50,-10)))
  theRast

  varnames(theRast) <- vn
  longnames(theRast) <- longNames
  units(theRast) <- theUnits

  theRast

})
