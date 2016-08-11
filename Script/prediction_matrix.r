# Extracting as interaction matrix
prediction_matrix <- function(S1, predictions, predict.only) {
    predict.matrix <- matrix(nrow = length(S1), ncol = length(S1), data = 0, dimnames = list(S1,S1))

    for(i in 1:length(S1)) {
        predict <- unlist(strsplit(predictions[i, 'resource_predictions'], " \\|\\ ")) # list of resource predicted for consumer i
        empirical <- unlist(strsplit(predictions[i, 'resource_empirical'], " \\|\\ ")) # list of resource observed for consumer i

        if(length(predict) == 0) {
          NULL
        } else {
            for(j in 1:length(predict)) {
                predict.matrix[predict[j],i] <- 1
            }#j
        }#if

        if(length(empirical) == 0) {
          NULL
        } else {
            for(j in 1:length(empirical)) {
                if(predict.only == TRUE) {
                    break
                } else {
                    predict.matrix[empirical[j],i] <- 1
                } #if
            }#j
        }#if
    }#i
    return(predict.matrix)
}#prediction_matrix function end
