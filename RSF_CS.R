# Load SpaDES
library("SpaDES")
library("raster")

setwd("/mnt/data/Micheletti/NWT/modules/caribouRSF/")
# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(file.path(dirname(dirname(getwd())), "functions"), full.names = TRUE), FUN = source))

options("spades.recoverMode" = 1)

# Set a storage project folder
workDirectory <- getwd()
message("Your current temporary directory is ", tempdir())

setPaths(modulePath = file.path(dirname(getwd())), 
         inputPath = file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/"), 
         outputPath = file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/"),
         cachePath = file.path("/mnt/data/Micheletti/NWT/cache/"))
getPaths() # shows where the 4 relevant paths are

parameters <- list(
  "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
  "predictionInterval" = 10
)
modules <- list("caribouRSF")
.objects <- list()
inputs <- list()
outputs <- list()

if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)

env <- environment()

lapply(X = c(2011, 2100), FUN = function(YEAR){ #, 2100
  
  outputsRSF <- data.frame(
    objectName = "predictedPresenceProbability",
    saveTime = YEAR)

  times <- list(start = YEAR, end = YEAR) # 2100
  runName <- paste0("RSF_CS_SCFM_V2", YEAR)
  assign(x = runName, value = simInitAndSpades(times = times,
                                               params = parameters, 
                                               modules = modules,
                                               outputs = outputsRSF,
                                               objects = .objects,
                                               debug = 2), envir = env)
  saveRDS(object = get(runName),
          file = file.path(SpaDES.core::getPaths()$outputPath, 
                           paste0(runName,
                                  toupper(format(Sys.time(), "%d%b%y")))))
  rm(runName, envir = env)
})
