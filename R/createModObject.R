createModObject <- function(data, sim, pathInput, time){
  dt <- NULL
  if (!is.null(sim[[data]])){
    dt <- sim[[data]]
  } else {
    if (!suppliedElsewhere(object = data, sim = sim)){
      message(crayon::yellow(paste0(data, " not supplied by another module.", 
                                    " Will try using files in inputPath(sim)")))
      if (length(list.files(pathInput, 
                            recursive = TRUE)) == 0)
        stop(paste0("Please place the data in the input folder ", pathInput))
      dataName <- grepMulti(x = list.files(pathInput, 
                                           recursive = TRUE), 
                            patterns = c(data, paddedFloatToChar(time, padL = 3)))
      if (length(dataName) == 0){
        dt <- NULL
      } else {
        dt <- readRDS(file.path(pathInput, dataName))
      }
      if (!is.null(dt)) message(paste0(data, " loaded from " , 
                                       crayon::magenta(file.path(pathInput, dataName)),
                                       " for year ", time))
    }
  }
  return(dt)  
}
