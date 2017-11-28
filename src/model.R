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
selectedVars <- setdiff(colnames(testAugmented), c("Date"))
inputDataAgg <- wnvAgg[,c(selectedVars, "WnvPresent")]

library(caret)

idxList <- createDataPartition(inputDataAgg$WnvPresent, p = .75, list = FALSE)
modelTrainingDf <- inputDataAgg[ idxList,]
modelTestingDf  <- inputDataAgg[-idxList,]

gbmGrid <-  expand.grid(mtry=sqrt(ncol(inputDataAgg)))


fitControl <- trainControl( method = "repeatedcv",
                            number = 10,
                            repeats = 5, 
                            classProbs = TRUE,
                            #selectionFunction = "oneSE"
                            selectionFunction="tolerance",
                            verboseIter = TRUE,
                            summaryFunction = twoClassSummary
                            )
# nodesize
# importance

gbmFit1 <- train(WnvPresent ~ ., data = inputDataAgg, 
                 method = "rf", 
                 trControl = fitControl,
                 metric="ROC",
                 #nTrain = 0.5,
                 nodesize=2,
                 #importance=TRUE,
                 ntree = 500,
                 do.trace=TRUE,
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

saveRDS(gbmFit1, "rf-1.RDS")

#gbmFit1 <- readRDS("rf.RDS")
table(predict(gbmFit1, inputDataAgg) , inputDataAgg$WnvPresent)

predicted <- predict(gbmFit1, testAugmented)

predicted <- as.numeric(predicted) - 1

write.csv(data.frame(Id=as.character(seq(1, length(predicted))), WnvPresent=as.character(predicted)), "result.csv", row.names = FALSE)

varImpPlot(gbmFit1$finalModel)


library(randomForest)
library(rpart)
nomosquitoModel <- rpart(NumMosquitos~., wnvAgg[, c(selectedVars, 'NumMosquitos')], method="anova", control = rpart.control(minsplit = 10))


testAugmented$NumMosquitos <- predict(nomosquitoModel, testAugmented)

fitControl <- trainControl( method = "repeatedcv",
                            number = 10,
                            repeats = 5, 
                            classProbs = TRUE,
                            #selectionFunction = "oneSE"
                            selectionFunction="tolerance",
                            verboseIter = TRUE,
                            summaryFunction = twoClassSummary
)

rfFitMosquitos <- train(WnvPresent ~ ., data = wnvAgg[, c(selectedVars, 'NumMosquitos', "WnvPresent")], 
                 method = "rf", 
                 trControl = fitControl,
                 metric="ROC",
                 #nTrain = 0.5,
                 nodesize=2,
                 #importance=TRUE,
                 ntree = 500,
                 do.trace=TRUE,
                 verbose = TRUE)


table(predict(rfFitMosquitos, wnvAgg[, c(selectedVars, 'NumMosquitos', "WnvPresent")]) , inputDataAgg$WnvPresent)

predicted <- predict(rfFitMosquitos, testAugmented)

predicted <- as.numeric(predicted) - 1

write.csv(data.frame(Id=as.character(seq(1, length(predicted))), WnvPresent=as.character(predicted)), "result-model-count.csv", row.names = FALSE)

saveRDS(rfFitMosquitos, "rfFitMosquitos.RDS")

varImpPlot(rfFitMosquitos$finalModel)
