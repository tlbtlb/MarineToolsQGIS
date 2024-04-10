##User scripts=group
##rfpredict=name
##BC_test=string
##Points=string
##OutPoints=string
##cutoff=string
##rscriptsSource=string

FRFFD <- paste0(rscriptsSource,"\\rscripts\\FitRandomForestForDataframe.r")
PM    <- paste0(rscriptsSource,"\\rscripts\\PredictModel.r")
Ut    <- paste0(rscriptsSource,"\\rscripts\\Utils.r")
source(FRFFD)
source(PM)
source(Ut)


load(BC_test)
library(sf)
library(randomForest)
newData = read_sf(Points)

resultList <- PredictModelForDataframe(model, modelMetadata, trainingData, newData=newData, ignoreOutOfRangeValues=FALSE, cutoff="0.5")

names(resultList) <- "RF_predict"
oneList <- resultList[1]
oneFrame <- as.data.frame(oneList)
oneFrameAll <- replace(oneFrame, is.na(oneFrame), -1.0)
newList <- list(oneFrameAll)

predPoints <- cbind(newData, newList)
st_write(predPoints,OutPoints)
