# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#   2. Tanimoto analysis for multiple parameter values
# -----------------------------------------------------------------------------

# Evaluating the effects of multiple parameters on the efficiency of the algorithm

# -----------------------------------------------------------------------------
# FILES:
#   RData <- file = "RData/Tanimoto_analysis.RData" XXX adjust name with type of analysis
#   Script  <- file = "Script/2-1-Tanomoto_analysis.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# PARAMETERS:

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------

similarity_cons_res <- tanimoto_analysis(min.tx = 45,
                                        K.values = 8,
                                        MW = 1,
                                        WT =  c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                                        minimum_threshold = 0.3,
                                        filename = 'similarity_cons_res')

# Catalog vs predictions
accuracy  <- vector('list', 3)
names(accuracy) <- c('Catalog', 'Predict', 'Algorithm')
accuracy[[1]] <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis, empirical.only = TRUE)
accuracy[[2]] <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis, predict.only = TRUE)
accuracy[[3]] <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis)

#Figure
pdf(paste('./Article/',filename,'.pdf',sep=''),width=7,height=7)

# Plots
par(mfrow=c(2,2))
# Graph
for(j in 9:12) {
        eplot(xmin = -0.09, xmax = 1.09)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        foodwebs <- names(Tanimoto_analysis[[1]][[1]][[1]])
        names <- c('TSS','Score y', 'Score -y', 'Accuracy score')
        col <- c("#FF8822","#449955","#2288FF")
        # col <- c("#FF000088","#00FF0088","#0000FF88")
        # col <- gray.colors(11, start = 0, end = 0.8, gamma = 2.2, alpha = NULL) # grey scale ramp
        # col <- sample(colours(), length(foodwebs))

        # Axes
            # rect(0, 0, 1, 1, col = "#eeeeee", border = NA)
            axis(side = 1, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02)
            axis(side = 2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02)
            axis(side = 3, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 1.02)
            axis(side = 4, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 1.02)
            # abline(v = seq(0,6,by = 2), col = "white", lty = 2)
            # abline(h = seq(1,2,by = 1), col = "white", lty = 2)

            mtext(text = names[j-8], side = 2, line = 2, at = 0.5, font = 2, cex = 1)
            mtext(text = "Similarity weight", side = 1, line = 2, at = 0.5, font = 2, cex = 1)

        for(i in 1:length(accuracy)) {
            accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
            # hack: we draw arrows but with very special "arrowheads" for error bars
            arrows(accuracy_mean[, 1], accuracy_mean[, 2][,1] - accuracy_mean[, 2][, 2], accuracy_mean[, 1], accuracy_mean[, 2][, 1] + accuracy_mean[, 2][, 2], length=0.05, angle=90, code=3, col = col[i])
            points(x = accuracy_mean[, 1], y = accuracy_mean[, 2][, 1], cex = 1.5, pch = 22, col = col[i])
        } #i

        ## Add legend
        if(j == 12) {
            legend(0.45, 0.3, lwd = 3, col = col, legend = names(accuracy), bty = 'n', y.intersp = 1)
        }
} #j
dev.off()

save(x = Tanimoto_analysis, file = paste('./Analyses/',filename,'.RData',sep=''))
