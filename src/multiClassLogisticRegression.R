library(caret);
binaryLR <- function(yVal, x, y, formula = NULL){
    traindata <- x
    traindata$y <- (y == yVal)
    if(is.null(formula)){
        formula <- y ~ .
    }
    glm(formula, data=traindata, family = binomial)
}

multiClassLR <- function(x, y, formula = NULL){
    yVals <- unique(y)
    lrModels <- lapply(yVals, binaryLR, x = x, y = y, formula = formula)
    names(lrModels) <- yVals
    lrModels
}

binaryLRProbs <- function(binaryLR, x){
    round(predict(binaryLR, x, type='response'), 3)
}

multiClassLRProbs <- function(lrModels, x){
    as.data.frame(lapply(lrModels, binaryLRProbs, x))
}

multiClassLRPredict <- function(model, x, dist=FALSE){
    res <- multiClassLRProbs(model, x)
    if(dist){
        res   
    }
    else{
        index <- apply(res, 1, which.max)
        names(res)[index]
    }    
}