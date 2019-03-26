createDynamicLayersRSF <- function(ageMap,
                                   biomassMap,
                                   biomassMapName,
                                   oldBurnTime,
                                   oldBurnName,
                                   newBurnName,
                                   anthropogenicLayer,
                                   anthropogenicLayerName,
                                   waterRaster,
                                   waterRasterName,
                                   RTM){

  biomassMap <- nameAndBringOn(ras = biomassMap, name = biomassMapName, RTM = RTM)
  anthropogenicLayer <- nameAndBringOn(ras = anthropogenicLayer, name = anthropogenicLayerName, RTM = RTM)
  waterRaster <- nameAndBringOn(ras = waterRaster, name = waterRasterName, RTM = RTM)

  burnStk <- raster::stack(burnFromAge(ageMap = ageMap, oldBurnTime = oldBurnTime, 
                                       newBurnName = newBurnName, oldBurnName = oldBurnName))

  burnStk <- raster::stack(lapply(X = seq_len(raster::nlayers(burnStk)), FUN = function(nLay){
    ras <- nameAndBringOn(ras = burnStk[[nLay]], name = names(burnStk)[nLay], RTM = RTM)
    return(ras)
  }))

   # 2. Make sure all rasters are in the same extent
  tryCatch(expr = {
    
    dynamicStack <- raster::stack(burnStk, biomassMap, anthropogenicLayer, waterRaster)
    return(dynamicStack)
    
  }, error = function(e){
    message("One or more layers have a different extent and/or crs. Trying to fix with postProcess...")

    exts <- c(raster::extent(biomassMap), 
                   raster::extent(burnStk), 
                   raster::extent(anthropogenicLayer), 
                   raster::extent(waterRaster))
    names(exts) <- c("biomassMap", "burnStk", "anthropogenicLayer", "waterRaster")
    
    tbl <- outer(exts, exts, Vectorize(all.equal))
    whichNot <- unlist(lapply(X = seq_len(length(exts)), function(res){
      r <- if (isTRUE(tbl[, 1][[res]])) NULL else names(exts)[res]
      return(r)
    }))
    
    message(paste0("The following layers don't match the base Deciduous (biomassMap) and will be fixed: ", crayon::magenta(whichNot)))
    fixedLayers <- raster::stack(lapply(X = whichNot, FUN = function(badLay){
      fxL <- reproducible::postProcess(x = get(badLay), rasterToMatch = biomassMap,
                                                      destinationPath = tempdir(), filename2 = NULL)
      return(fxL)
    }
    ))
    fineStacks <- setdiff(c("burnStk", "biomassMap", "anthropogenicLayer", "waterRaster"), whichNot)
    fineStacks <- raster::stack(lapply(X = fineStacks, FUN = function(r){
      ras <- get(r)
      return(ras)
    }))
    
    dynamicStack <- raster::stack(fineStacks, fixedLayers)
    return(dynamicStack)
    }
  )
}
