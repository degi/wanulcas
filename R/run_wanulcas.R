library(profvis)


### HOW TO RUNNING WANULCAS ###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("wanulcas.R")
# profvis({
output <- run_wanulcas(500)
# })
