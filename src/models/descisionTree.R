library(caret);library(rpart);library(randomForest);
source('src//multiClassLogisticRegression.R')
source('src//preProcess//preProcess.R')
source('src//graphics//clusterPlot.R')
source('src/preProcess/filter.R')
source('src//preProcess//topNImportantFeature.R')

data <- getCleanData()
set.seed(1234)

trainIndex <- createDataPartition(data[,ncol(data)], p=0.5, list=FALSE)
trainData <- data[trainIndex, ]
xTrain <- trainData[,-ncol(trainData)]
yTrain <- trainData[,ncol(trainData)]
trainData <- xTrain
trainData$y <- yTrain

testData <- data[-trainIndex,]
xTest <- testData[,-ncol(testData)]
yTest <- testData[,ncol(testData)]


modelRpart <- rpart(y ~ ., data = trainData)
predDist <- predict(modelRpart, newdata = xTest)
index <- apply(predDist, 1, which.max)
preds <- colnames(predDist)[index]
confusionMatrix(preds, yTest)
