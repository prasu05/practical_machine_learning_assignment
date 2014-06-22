topN <- function(num, model){
    importance <- varImp(model, useModel = FALSE)
    features <- rownames(importance)
    topN <- order(importance$Overall, decreasing = TRUE)[1:num]
    features[topN]
}
