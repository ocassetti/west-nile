
wnvAgg <- readRDS("src/wnvAgg.RDS")
testAugmented <- readRDS("src/testAugmented.RDS")


inputDataAgg <- wnvAgg[,c(colnames(testAugmented), "WnvPresent")]

library(caret)

idxList <- createDataPartition(inputDataAgg$WnvPresent, p = .75, list = FALSE)
modelTrainingDf <- inputDataAgg[ idxList,]
modelTestingDf  <- inputDataAgg[-idxList,]

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

fitControl <- trainControl( method = "repeatedcv",
                            number = 10,
                            repeats = 10)


gbmFit1 <- train(WnvPresent ~ ., data = modelTrainingDf, 
                 method = "gbm", 
                 trControl = fitControl,
                 tuneGrid = gbmGrid,
                 verbose = FALSE)
gbmFit1

