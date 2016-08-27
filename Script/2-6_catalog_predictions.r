# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#   2. Analysis iteratively removing information from the catalog
# -----------------------------------------------------------------------------

# Evaluating algorithm accuracy ~ # of taxa in the catalog
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
load("./RData/interactions_source.RData")
filename = 'catalog_predictions'

catalog_predictions0 <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = 0,
                                            nb_iter = 1,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  0.5,
                                            minimum_threshold = 0.3,
                                            filename = 'catalog_predictions0')

catalog_predictions1 <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = 100,
                                            nb_iter = 1,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  0.5,
                                            minimum_threshold = 0.3,
                                            filename = 'catalog_predictions1')

catalog_predictions <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = c(10,20,40,60,80),
                                            nb_iter = 100,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  0.5,
                                            minimum_threshold = 0.3,
                                            filename = filename)

# Catalog vs predictions
accuracy <- accuracy0 <- accuracy1 <-  vector('list', 3)
names(accuracy) <- names(accuracy0) <- names(accuracy1) c('Catalog', 'Predict', 'Algorithm')
accuracy[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions, empirical.only = TRUE)
accuracy[[2]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions, predict.only = TRUE)
accuracy[[3]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions)

accuracy0[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions0, empirical.only = TRUE)
accuracy0[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions0, predict.only = TRUE)
accuracy0[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions0)

accuracy1[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions1, empirical.only = TRUE)
accuracy1[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions1, predict.only = TRUE)
accuracy1[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions1)

accuracy[[1]] <- rbind(accuracy[[1]], accuracy0[[1]], accuracy1[[1]])
accuracy[[2]] <- rbind(accuracy[[2]], accuracy0[[2]], accuracy1[[2]])
accuracy[[3]] <- rbind(accuracy[[3]], accuracy0[[3]], accuracy1[[3]])


#Figure
pdf(paste('./Article/',filename,'.pdf',sep=''),width=7,height=7)

# Plots
par(mfrow=c(2,2))
# Graph
for(j in 9:12) {
        eplot(xmin = -0.09, xmax = 1.09)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        # foodwebs <- names(similarity_cons_res_blind[[1]][[1]][[1]])
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
