
checkNa <- function(list){
    sum(is.na(list)) > 1
}

getCleanData <- function(){
    data <- read.csv('data/pml-training.csv')
    data <- data[,-c(1:7)]
    features <- names(data)
    Nas <- t(sapply(data, checkNa))
    data <- data[,!Nas]
    
    nsv <- nearZeroVar(data)
    data <- data[,-nsv]
}

randomSubset <- function(data, num){
    randomIndex <- sample(nrow(data), num)
    data[randomIndex, ]
}