calculateRSF <- function(Elevation, 
                         Vrug,
                         Shrub, 
                         Herb, 
                         RoadDensity, 
                         Deciduous, 
                         Water, 
                         RecentBurn,
                         OldBurn,
                         caribouModels){
  
  if (is.na(Deciduous)) return(list(meanResponse = NA, sdResponse = NA))
    resp <- eval(parse(text = caribouModels))
    return(list(meanResponse = mean(resp), sdResponse = sd(resp)))
}
