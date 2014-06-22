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

testData <- data[-trainIndex,]
xTest <- testData[,-ncol(testData)]
yTest <- testData[,ncol(testData)]

modelSvm <- svm(x = xTrain, y = yTrain)
preds <- predict(modelSvm, newdata = xTest)
confusionMatrix(preds, yTest)$overall[1]
