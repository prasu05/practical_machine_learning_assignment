normalize <- function(data){
    normObj <- preProcess(data, method = c('center', 'scale'))
    predict(normObj, data)
}