reclassifyRastersDirect <- function(rasterToReclassify, table, originalCol, reclassifiedTo){
  browser()
  m <- matrix(data = c(table[[originalCol]], table[[reclassifiedTo]]), ncol = 3, byrow = FALSE)
  
}
  