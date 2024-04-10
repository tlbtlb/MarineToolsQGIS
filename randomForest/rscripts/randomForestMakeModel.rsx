##User scripts=group
##rfmakemodel=name
##Layer=string
##BC_test=string
##InFormula=string
##ntree=string
##rscriptsSource=string

inLayer <- names(Layer)
FRFFD <- paste0(rscriptsSource,"\\rscripts\\FitRandomForestForDataframe.r")
PM    <- paste0(rscriptsSource,"\\rscripts\\PredictModel.r")
Ut    <- paste0(rscriptsSource,"\\rscripts\\Utils.r")
source(FRFFD)
source(PM)
source(Ut)


library(sf)
library(randomForest)
trainingData = read_sf(Layer)
resultTim <- FitRandomForestForDataframe(InFormula, trainingData, ntree, mtry=NULL, rPackage = "randomForest", outputModelFile=BC_test)

