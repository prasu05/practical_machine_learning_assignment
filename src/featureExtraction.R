source('src//preProcess//filter.R')
source('src//graphics//clusterPlot.R')
data <- filter()
sampleData <- randomSubset(data, 1000)
svd1 = svd(scale(sampleData[, -c(53)]))

top <- order(svd1$v[, 2], decreasing = TRUE)[1:3]
distanceMatrix <- dist(sampleData[, top])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sampleData$classe))
