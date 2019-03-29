nameAndBringOn <- function(ras, name, RTM = NULL){
  
  # Bring into memory, name and if a RTM is passed, mask non-NA inside it to 0
  names(ras) <- name
  ras[] <- ras[]
  if (!is.null(RTM)){
    ras <- postProcess(ras, rasterToMatch = RTM, filename2 = NULL, destinationPath = tempdir(), useCache = FALSE)
    rasVals <- raster::getValues(ras)
    valsRTM <- raster::getValues(RTM)
    rasVals[is.na(rasVals) & valsRTM == 1] <- 0
    ras <- raster::setValues(x = ras, values = rasVals)
  }
  return(ras)
}