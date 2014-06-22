library(caret);data(iris);
source('src//multiClassLogisticRegression.R')

data <- iris
trainIndex <- createDataPartition(data$Species, p=0.8, list=FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex,]

x <- trainData[,-ncol(trainData)]
y <- trainData[,ncol(trainData)]


lrModels <- multiClassLR(x, y)
preds <- multiClassLRPredict(lrModels = lrModels, x = testData[,-ncol(testData)])
confusionMatrix(preds, testData[,ncol(testData)])