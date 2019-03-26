burnFromAge <- function(ageMap,
                        oldBurnTime,
                        newBurnName,
                        oldBurnName){
  
  newBurn <- oldBurn <- ageMap
  newAge <- oldAge <- getValues(ageMap)
  
  newAge[newAge > oldBurnTime] <- -1
  newAge[!is.na(newAge) & newAge >= 0] <- 1
  newBurn <- setValues(newBurn, newAge)
  newBurn[newBurn == -1] <- 0
  names(newBurn) <- newBurnName
  
  oldAge[oldAge <= oldBurnTime] <- -1
  oldAge[!is.na(oldAge) & oldAge > 0] <- 1
  oldBurn <- setValues(oldBurn, oldAge)
  oldBurn[oldBurn == -1] <- 0
  names(oldBurn) <- oldBurnName
  
  return(list(newBurn, oldBurn))
}