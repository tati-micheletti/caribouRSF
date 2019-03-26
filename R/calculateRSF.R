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


# calculateRSF <- function(pixel, coeffTable, caribouModels, 
#                          modelType, maxPixel, printMessage = TRUE){
# pix <- c(1, 100, 300, 800, seq(1000, 10000, by = 1000), seq(10000, maxPixel, by = 10000), maxPixel)
#   if (printMessage & pixel %in% pix)
#     message(paste0("Starting RSF for pixel ", pixel, " of ", maxPixel))
#   if (!is.na(sum(coeffTable[pixel,]))){
#     attach(coeffTable[pixel,])
#     on.exit(detach(coeffTable[pixel,]))
#     resp <- eval(parse(text = caribouModels[[modelType]]))
#     dtResp <- data.table::data.table(meanResponse = mean(resp),
#                                      sdResponse = sd(resp))
#     return(dtResp)
#   } else {
#     dtResp <- data.table::data.table(meanResponse = NA,
#                                      sdResponse = NA)
#     return(dtResp)
#   }
# }