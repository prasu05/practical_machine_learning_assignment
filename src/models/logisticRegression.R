library(caret);library(rpart);library(randomForest);
library(gridExtra)
source('src//multiClassLogisticRegression.R')
source('src//preProcess//preProcess.R')
source('src//graphics//clusterPlot.R')
source('src/preProcess/filter.R')
source('src//preProcess//topNImportantFeature.R')

data <- getCleanData()
set.seed(1234)

trainIndex <- createDataPartition(data[,ncol(data)], p=0.5, list=FALSE)
trainData <- data[trainIndex, ]
xTrain <- normalize(trainData[,-ncol(trainData)])
yTrain <- trainData[,ncol(trainData)]

testData <- data[-trainIndex,]
xTest <- normalize(testData[,-ncol(testData)])
yTest <- testData[,ncol(testData)]

modelLr <- multiClassLR(x = xTrain, y = yTrain)
preds <- multiClassLRPredict(model = modelLr, x = xTest)
cm <- confusionMatrix(preds, yTest)
# png("cm_logistic_regression.png")
# p<-tableGrob(cm$table)
# grid.arrange(p)
# dev.off()

topNFeatures <- topN(num = 4, modelLr[[1]])
featurePlot(x=trainData[,topNFeatures],
            y = trainData[,ncol(trainData)],
            plot='density')

