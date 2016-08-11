# Extracting as interaction matrix
prediction_matrix <- function(S1, predictions, predict.only = FALSE, empirical.only = FALSE) {
    # predict.only: only predictions will be used to recreate the prediction matrix
    # empirical.only: only empirical data will be used to recreate the prediction matrix, corresponding to the contribution of the interaction catalog
    # rownames need to be taxa names
    predict.matrix <- matrix(nrow = length(S1), ncol = length(S1), data = 0, dimnames = list(S1,S1))

    for(i in 1:length(S1)) {
        predict <- unlist(strsplit(predictions[i, 'resource_predictions'], " \\|\\ ")) # list of resource predicted for consumer i
        empirical <- unlist(strsplit(predictions[i, 'resource_empirical'], " \\|\\ ")) # list of resource observed for consumer i

        if(length(predict) == 0) {
          NULL
        } else {
            for(j in 1:length(predict)) {
                if(empirical.only == TRUE) {
                    break
                } else { # empirical.only == FALSE
                    predict.matrix[predict[j],i] <- 1
                } #if
            }#j
        }#if

        if(length(empirical) == 0) {
          NULL
        } else {
            for(j in 1:length(empirical)) {
                if(predict.only == TRUE) {
                    break
                } else { # predict.only == FALSE
                    predict.matrix[empirical[j],i] <- 1
                } #if
            }#j
        }#if
    }#i
    return(predict.matrix)
}#prediction_matrix function end
