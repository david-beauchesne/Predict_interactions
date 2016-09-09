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
filename1 = 'catalog_predictions2'
filename2 = 'catalog_predictions3'


catalog_predictions0 <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = 0,
                                            nb_iter = 1,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  c(0.5,1),
                                            minimum_threshold = 0.3,
                                            filename = 'catalog_predictions0')

catalog_predictions1 <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = 100,
                                            nb_iter = 1,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  c(0.5,1),
                                            minimum_threshold = 0.3,
                                            filename = 'catalog_predictions1')

catalog_predictions2 <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = c(10,20,40,60,80),
                                            nb_iter = 50,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  c(0.5,1),
                                            minimum_threshold = 0.3,
                                            filename = filename1)

catalog_predictions3 <- catalog_predictions(comm_id = TRUE,
                                            community = "Kortsch2015_arctic",
                                            percent_remove = c(30,50,70,90),
                                            nb_iter = 50,
                                            K.values = 8,
                                            MW = 1,
                                            WT =  c(0.5,1),
                                            minimum_threshold = 0.3,
                                            filename = filename2)


# Catalog vs predictions

accuracy <- accuracy0 <- accuracy1 <- accuracy2 <-  vector('list', 3)
names(accuracy) <- names(accuracy0) <- names(accuracy1) <- names(accuracy2) <- c('Catalog', 'Predict', 'Algorithm')
accuracy[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions2, empirical.only = TRUE)
accuracy[[2]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions2, predict.only = TRUE)
accuracy[[3]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions2)

accuracy0[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions0, empirical.only = TRUE)
accuracy0[[2]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions0, predict.only = TRUE)
accuracy0[[3]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions0)

accuracy1[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions1, empirical.only = TRUE)
accuracy1[[2]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions1, predict.only = TRUE)
accuracy1[[3]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions1)

accuracy2[[1]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions3, empirical.only = TRUE)
accuracy2[[2]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions3, predict.only = TRUE)
accuracy2[[3]] <- catalog_predictions_accuracy(Tanimoto_analysis = catalog_predictions3)

accuracy[[1]] <- rbind(accuracy[[1]], accuracy0[[1]], accuracy1[[1]], accuracy2[[1]])
accuracy[[2]] <- rbind(accuracy[[2]], accuracy0[[2]], accuracy1[[2]], accuracy2[[2]])
accuracy[[3]] <- rbind(accuracy[[3]], accuracy0[[3]], accuracy1[[3]], accuracy2[[3]])

percent_remove = c(0,10,20,30,40,50,60,70,80,90,100)
nb_iter = 50
K.values = 8
MW = 1
WT =  c(0.5,1)
minimum_threshold = 0.3

nb.pts <- length(percent_remove)

#Figure version 1
pdf(paste('./Article/','catalog_predictions','.pdf',sep=''),width=6,height=8)
j = 14 #'Score'[y]
        eplot(xmin = -1, xmax = 100 + 1, ymax = 3.6)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        col <- c("#FF8822",'#5ED275','#9CCBFF')
        col2 <- c("#FF8822",'#275A31','#0077FF')
        col3 <- c("#FF8822","#449955","#2288FF")

        # Axes
            axis(side = 1, at = seq(0, 100, by = 10), labels = FALSE, las = 1, pos = -0.05)
            axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -1)
            axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -1)
            axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -1)
            axis(side = 3, at = seq(0, 100, by = 10), labels = FALSE, las = 1, pos = 1.05 + 2.5)
            axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = 100 + 1)
            axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = 100 + 1)
            axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = 100 + 1)

            abline(h = c(1.125,2.375), col = "black", lty = 2)
            mtext(text = expression('Score'[y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            mtext(text = expression(paste("Percent of taxa removed from ", italic(S0), ' (%)')), side = 1, line = 2, at = 50, font = 2, cex = 1)
            mtext(text = seq(0, 100, by = 10), side = 1, line = 0, at = seq(0, 100, by = 10), font = 1, cex = 0.75)
            mtext(text = seq(0, 100, by = 10), side = 3, line = -0.5, at = seq(0, 100, by = 10), font = 1, cex = 0.75)
            text(x = 5, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col3[1], adj = 0)
            text(x = 5, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col3[2], adj = 0)
            text(x = 5, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col3[3], adj = 0)


        it <- 0
        for(i in 1:length(accuracy)) {
        # for(i in 2) {
            points(x = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '0.5'), 'pc_rm']), y = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '0.5'), j]) + it, cex = 0.5, pch = 1, col = col[i])
            points(x = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '1'), 'pc_rm']), y = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '1'), j]) + it, cex = 0.5, pch = 1, col = col2[i])

            lines(lowess(x = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '0.5'), 'pc_rm']), y = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '0.5'), j]) + it), col = col[i])
            lines(lowess(x = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '1'), 'pc_rm']), y = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '1'), j]) + it), col = col2[i])

            if(i == 2 || i == 3) {
                text(x = 90, y = 0.9 + it, labels = expression(paste(italic('w'[t]), ' = 0.5')), col = col[i], font = 1, cex = 0.75)
                text(x = 90, y = 0.8 + it, labels = expression(paste(italic('w'[t]), ' = 1')), col = col2[i], font = 1, cex = 0.75)
            }

            it <- it + 1.25
        } #i
dev.off()

#Figure version 2
pdf(paste('./Article/','catalog_predictions2','.pdf',sep=''),width=6,height=8)
j = 14 #'Score'[y]
        eplot(xmin = -1, xmax = 100 + 1, ymax = 3.6)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        col <- c("#FF8822",'#5ED275','#9CCBFF')
        col2 <- c("#FF8822",'#275A31','#0077FF')
        col3 <- c("#FF8822","#449955","#2288FF")

        # Axes
            axis(side = 1, at = seq(0, 100, by = 10), labels = FALSE, las = 1, pos = -0.05)
            axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -1, cex.axis = 0.75, font.axis = 1)
            axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -1, cex.axis = 0.75, font.axis = 1)
            axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -1, cex.axis = 0.75, font.axis = 1)
            axis(side = 3, at = seq(0, 100, by = 10), labels = FALSE, las = 1, pos = 1.05 + 2.5)
            axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = 100 + 1, cex.axis = 0.75, font.axis = 1)
            axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = 100 + 1, cex.axis = 0.75, font.axis = 1)
            axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = 100 + 1, cex.axis = 0.75, font.axis = 1)

            abline(h = c(1.125,2.375), col = "black", lty = 2)
            mtext(text = expression('Score'[y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            mtext(text = expression(paste("Percent of taxa removed from ", italic(S0), ' (%)')), side = 1, line = 2, at = 50, font = 2, cex = 1)
            mtext(text = seq(0, 100, by = 10), side = 1, line = 0, at = seq(0, 100, by = 10), font = 1, cex = 0.75)
            mtext(text = seq(0, 100, by = 10), side = 3, line = -0.5, at = seq(0, 100, by = 10), font = 1, cex = 0.75)
            text(x = 5, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col3[1], adj = 0)
            text(x = 5, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col3[2], adj = 0)
            text(x = 5, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col3[3], adj = 0)


        it <- 0
        for(i in 1:length(accuracy)) {
            if(i == 2 || i == 3) {
                accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'pc_rm']) + as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
                accuracy_mean <- accuracy_mean[order(accuracy_mean[,2]), ]
                # hack: we draw arrows but with very special "arrowheads" for error bars

                arrows(seq(0,100,by=10), accuracy_mean[which(accuracy_mean[, 2] == '0.5'), 3][,1] - accuracy_mean[which(accuracy_mean[, 2] == '0.5'), 3][, 2]+it, seq(0,100,by=10), accuracy_mean[which(accuracy_mean[, 2] == '0.5'), 3][, 1] + accuracy_mean[which(accuracy_mean[, 2] == '0.5'), 3][, 2]+it, length=0.025, angle=90, code=3, col = col[i])
                points(x = seq(0,100,by=10), y = accuracy_mean[which(accuracy_mean[, 2] == '0.5'), 3][, 1]+it, cex = 0.75, pch = 22, col = col[i])

                arrows(seq(0,100,by=10), accuracy_mean[which(accuracy_mean[, 2] == '1'), 3][,1] - accuracy_mean[which(accuracy_mean[, 2] == '1'), 3][, 2]+it, seq(0,100,by=10), accuracy_mean[which(accuracy_mean[, 2] == '1'), 3][, 1] + accuracy_mean[which(accuracy_mean[, 2] == '1'), 3][, 2]+it, length=0.025, angle=90, code=3, col = col2[i])
                points(x = seq(0,100,by=10), y = accuracy_mean[which(accuracy_mean[, 2] == '1'), 3][, 1]+it, cex = 0.75, pch = 22, col = col2[i])

                lines(lowess(x = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '0.5'), 'pc_rm']), y = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '0.5'), j]) + it), col = col[i])
                lines(lowess(x = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '1'), 'pc_rm']), y = as.numeric(accuracy[[i]][which(accuracy[[i]][, 'wt'] == '1'), j]) + it), col = col2[i])

                text(x = 90, y = 0.9 + it, labels = expression(paste(italic('w'[t]), ' = 0.5')), col = col[i], font = 1, cex = 0.75)
                text(x = 90, y = 0.8 + it, labels = expression(paste(italic('w'[t]), ' = 1')), col = col2[i], font = 1, cex = 0.75)
                } else {
                    accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'pc_rm']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
                    accuracy_mean <- accuracy_mean[order(accuracy_mean[,1]), ]
                    # hack: we draw arrows but with very special "arrowheads" for error bars

                    arrows(seq(0,100,by=10), accuracy_mean[, 2][,1] - accuracy_mean[, 2][, 2]+it, seq(0,100,by=10), accuracy_mean[, 2][, 1] + accuracy_mean[, 2][, 2]+it, length=0.025, angle=90, code=3, col = col3[i])
                    points(x = seq(0,100,by=10), y = accuracy_mean[, 2][, 1]+it, cex = 0.75, pch = 22, col = col3[i])

                    lines(lowess(x = as.numeric(accuracy[[i]][, 'pc_rm']), y = as.numeric(accuracy[[i]][, j]) + it), col = col3[i])
                }

            it <- it + 1.25
        } #i
dev.off()
