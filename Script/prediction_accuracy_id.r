# Prediction efficiency in two ways, plus identity of taxa for which we obtain a, b, c or d
prediction_accuracy_id <- function(predicted, empirical) {
    # Parameters:
    #   predicted   matrix of predicted interactions
    #   empirical   matrix of empirical interactions

    #   Output      vector of a, b, c, d, and TSS
    #       a       number of links predicted (1) and observed (1)
    #       b       number predicted (1) but not observed (0)
    #       c       number predicted absent (0) but observed (1)
    #       d       number of predicted absent (0) and observed absent (0)
    #       TSS     TSS = (ad-bc)/[(a+c)(b+d)]
    #                   "[...] quantifies the proportion of prediction success relative to false predictions
    #                   and returns values ranging between 1 (perfect predictions) and 1 (inverted forecast)
    #                   (Allouche, Tsoar & Kadmon 2006)." Gravel et al. 2013
    #       ScoreY1 Fraction of 1 correctly predicted a / (a + c)
    #       ScoreY0 Fraction of 0 correctly predicted d / (b + d)
    #       FSS     FSS = ScoreY1 + ScoreY0 / sum(a, b, c, d)^2

    if(identical(colnames(predicted), colnames(empirical)) == FALSE ||
        identical(rownames(predicted), rownames(empirical)) == FALSE ||
        identical(dim(predicted), dim(empirical)) == FALSE) {
            print('matrices need to have same dimensions and row and column names')
            break
    }

    efficiency <- numeric(8)
    aa <- bb <- cc <- dd <-  numeric(2)
    names(efficiency) <- c('a','b','c','d','TSS','ScoreY1','ScoreY0','FSS')

    for(i in 1:ncol(predicted)){
        for(j in 1:nrow(predicted)) {
            if(predicted[i,j] == 1 && empirical[i,j] == 1) {
                efficiency[1] <- efficiency[1] + 1
                aa <- rbind(aa,c(i,j))
            } else if(predicted[i,j] == 1 && empirical[i,j] == 0) {
                efficiency[2] <- efficiency[2] + 1
                bb <- rbind(bb,c(i,j))
            } else if(predicted[i,j] == 0 && empirical[i,j] == 1) {
                efficiency[3] <- efficiency[3] + 1
                cc <- rbind(cc,c(i,j))
            } else if(predicted[i,j] == 0 && empirical[i,j] == 0) {
                efficiency[4] <- efficiency[4] + 1
                dd <- rbind(dd,c(i,j))
            }
        }
    }

    a <- efficiency[1]
    b <- efficiency[2]
    c <- efficiency[3]
    d <- efficiency[4]

    efficiency[5] <- ((a * d) - (b * c)) / ((a + c) * (b + d))  # TSS
    efficiency[6] <- a / (a + c)                                # ScoreY1
    efficiency[7] <- d / (b + d)                                # ScoreY0
    efficiency[8] <- (a + d) / sum(a, b, c, d)    # FSS

    efficiency.id <- vector('list',5)
    efficiency.id[[1]] <- efficiency
    efficiency.id[[2]] <- aa
    efficiency.id[[3]] <- bb
    efficiency.id[[4]] <- cc
    efficiency.id[[5]] <- dd

    return(efficiency.id)
}
