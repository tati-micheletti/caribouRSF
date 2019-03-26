createShrubHerbLayers <- function(landCoverECCC, reclassLCC05, layerName, includeCrops = FALSE){
  className <- unique(reclassLCC05$ECCC_Description[grepl(pattern = layerName, x = reclassLCC05$ECCC_Description)])
  if (!isTRUE(includeCrops))
    className <- className[!grepl(x = className, pattern = "cropland")]
  shrubHerbLayer <- raster(landCoverECCC)
  valsshrubHerbs <- raster::getValues(landCoverECCC)
  valshrubHerbFromTable <- unique(reclassLCC05[ECCC_Description == className, classesECCC])
  valsshrubHerbs[(valsshrubHerbs != valshrubHerbFromTable) & !is.na(valsshrubHerbs)] <- 0
  valsshrubHerbs[valsshrubHerbs == valshrubHerbFromTable] <- 1
  shrubHerbLayer <- raster::setValues(x = shrubHerbLayer, values = valsshrubHerbs)
  names(shrubHerbLayer) <- layerName
  return(shrubHerbLayer)
}