makeReclassifyMatrix <- function(table, originalCol, reclassifiedTo){
  # Matrix with 3 columnd: from X to Y reclass to Z
  m <- matrix(data = c(table[[originalCol]], table[[originalCol]], table[[reclassifiedTo]]), ncol = 3, byrow = FALSE)
  return(m)
}
