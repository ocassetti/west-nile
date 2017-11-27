library(doMC)
registerDoMC(cores = 5)

wnvAgg <- readRDS("src/wnvAgg.RDS")
testAugmented <- readRDS("src/testAugmented.RDS")


wnvAgg$WnvPresent <- factor(sapply(wnvAgg$WnvPresent, function(x){
  retVal <- 'N'
  if(x == 1){
  retVal <- 'Y'
  }
  
  retVal
}))
intersect(colnames(wnvAgg), colnames(testAugmented) )
setdiff(colnames(wnvAgg), colnames(testAugmented) )
inputDataAgg <- wnvAgg[,c(colnames(testAugmented), "WnvPresent")]

library(caret)

idxList <- createDataPartition(inputDataAgg$WnvPresent, p = .75, list = FALSE)
modelTrainingDf <- inputDataAgg[ idxList,]
modelTestingDf  <- inputDataAgg[-idxList,]

gbmGrid <-  expand.grid(mtry=sqrt(ncol(inputDataAgg)))


fitControl <- trainControl( method = "repeatedcv",
                            number = 10,
                            repeats = 5, 
                            classProbs = TRUE,
                            selectionFunction = "oneSE")

gbmFit1 <- train(WnvPresent ~ ., data = inputDataAgg, 
                 method = "rf", 
                 trControl = fitControl,
                 metric="Kappa",
                 nTrain = 0.5,
                 ntree = 3000,
                 verbose = TRUE)


years <- unique(inputDataAgg$Year)

modelYear <- sapply(years, function(year){
  
  train(WnvPresent ~ ., data = inputDataAgg[inputDataAgg$Year == year, ], 
        method = "rf", 
        trControl = fitControl,
        metric="Kappa",
        nTrain = 0.5,
        parallel=TRUE,
        verbose = TRUE)
})



saveRDS(modelYear, "modelYear.RDS")

table(predict(gbmFit1, modelTestingDf), modelTestingDf$WnvPresent)

saveRDS(gbmFit1, "rf.RDS")


table(predict(gbmFit1, modelTestingDf) , modelTestingDf$WnvPresent)

predicted <- predict(gbmFit1, testAugmented)

predicted <- as.numeric(predicted) - 1

write.csv(data.frame(Id=as.character(seq(1, length(predicted))), WnvPresent=as.character(predicted)), "result.csv", row.names = FALSE)
