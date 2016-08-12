# ---------------------------------------------------------------------------
# Two-way Tanimoto algorithm: prediction accuracy for 2-Tanimoto_analysis.r
# ---------------------------------------------------------------------------

tanimoto_accuracy <- function(Tanimoto_analysis, predict.only = FALSE, empirical.only = FALSE) {
    load("./RData/interactions_source.RData")
    source("./Script/prediction_matrix.r")
    source("./Script/empirical_matrix.r")
    source("./Script/prediction_accuracy.r")


    accuracy <- matrix(ncol = 12, nrow = length(Tanimoto_analysis) * length(Tanimoto_analysis[[1]]) * length(Tanimoto_analysis[[1]][[1]]) * length(Tanimoto_analysis[[1]][[1]][[1]]), data = 0, dimnames = list(c(), c('MW','K','wt','Cm','a','b','c','d','TSS','ScoreY1','ScoreY0','FSS')))
    iteration <- 1
    for(n in 1: length(Tanimoto_analysis)) { #loop through MW values
        for(m in 1: length(Tanimoto_analysis[[1]])) { # loop through K values
            for(i in 1:length(Tanimoto_analysis[[1]][[1]])){ #1st loop for all types of wt values
                for(j in 1:length(Tanimoto_analysis[[1]][[1]][[1]])) { #2nd loop for all C[i]
                    # Arguments:
                    S1 <- Tanimoto_analysis[[n]][[m]][[i]][[j]][, 'consumer']
                    predictions <- Tanimoto_analysis[[n]][[m]][[i]][[j]]
                    interactions_source <- interactions_sources
                    source <- names(Tanimoto_analysis[[n]][[m]][[i]])[j]

                    accuracy[iteration, 'MW'] <- names(Tanimoto_analysis)[n]
                    accuracy[iteration, 'K'] <- names(Tanimoto_analysis[[n]])[m]
                    accuracy[iteration, 'wt'] <- names(Tanimoto_analysis[[n]][[m]])[i]
                    accuracy[iteration, 'Cm'] <- names(Tanimoto_analysis[[n]][[m]][[i]])[j]
                    accuracy[iteration, 5:12] <- prediction_accuracy(predicted = prediction_matrix(S1 = S1, predictions = predictions, predict.only = predict.only, empirical.only = empirical.only),
                                                        empirical = empirical_matrix(S1 = S1, interactions_source = interactions_source, source = source))

                    iteration <- iteration + 1
                    remove(S1, predictions, interactions_source, source)
                }#j
            }#i
        }#m
    }#n

    return(accuracy)

}#Tanimoto_accuracy function
