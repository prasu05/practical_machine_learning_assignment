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

modelRf <- randomForest(x = xTrain, 
                        y = yTrain,
                        importance = TRUE)

preds <- predict(modelRf, xTest)
cm <- confusionMatrix(preds, yTest)
# png("cm_random_forest.png")
# p<-tableGrob(cm$table)
# grid.arrange(p)
# dev.off()

test_assignment_data <- read.csv('data/pml-testing.csv')
ids <- test_assignment_data$problem_id
tad <- test_assignment_data[,names(trainData[,-ncol(trainData)])]
results <- predict(modelRf, tad)

answers = rep("A", 20)
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(results)
dim(test_assignment_data)
topnNFeatures <- modelRf$importance[,"A"][order(modelRf$importance[,"A"], decreasing = TRUE)][4]
featurePlot(x=trainData[,topNFeatures],
            y = trainData[,ncol(trainData)],
            plot='density')