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
filename <- 'multiple_parameters2'


multiple_parameters2 <- tanimoto_analysis(filename = filename,
                                            min.tx = 45,
                                            K.values = c(2,4,6,8),
                                            MW = 1,
                                            WT = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                                            blind = FALSE,
                                            minimum_threshold = 0.3)

WT = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
K.values = c(2,4,6,8)
MW = 1

# Catalog vs predictions
accuracy  <- vector('list', 3)
names(accuracy) <- c('Catalog', 'Predict', 'Algorithm')
accuracy[[1]] <- tanimoto_accuracy(Tanimoto_analysis = multiple_parameters2, empirical.only = TRUE)
accuracy[[2]] <- tanimoto_accuracy(Tanimoto_analysis = multiple_parameters2, predict.only = TRUE)
accuracy[[3]] <- tanimoto_accuracy(Tanimoto_analysis = multiple_parameters2)

#Figure
pdf(paste('./Article/',filename,'.pdf',sep=''),width=12,height=7)
# Plots
par(mfrow=c(2,2))
# layout(matrix(c(1,2,5,5,3,4), 3, 2, byrow = TRUE), heights = c(4.5,1,4.5))

nb.pts <- length(unique(accuracy[[1]][,'MW'])) * length(unique(accuracy[[1]][,'K'])) * length(unique(accuracy[[1]][,'wt']))

# Graph
for(j in 9:12) {
        eplot(xmin = -0.09, xmax = nb.pts, ymax = 3.5)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        foodwebs <- names(multiple_parameters2[[1]][[1]][[1]])
        names <- c('TSS','Score y', 'Score -y', 'Accuracy score')
        col <- c("#FF8822","#449955","#2288FF")
        # col <- c("#FF000000","#00FF0088","#0000FF88")
        # col <- gray.colors(11, start = 0, end = 0.8, gamma = 2.2, alpha = NULL) # grey scale ramp
        # col <- sample(colours(), length(foodwebs))

        # Axes
            axis(side = 1, at = seq(0, nb.pts, by = length(WT) * length(K.values)) + 0.5, labels = FALSE, las = 1, pos = -0.02) #MW
            axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -0.02)
            axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.02)
            axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.02)
            axis(side = 3, at = seq(0, nb.pts, by = length(K.values)) + 0.5, labels = FALSE, las = 1, pos = 1.02 + 2.5) #wt
            axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.02) + 1)
            axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.02) + 1)
            axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.02) + 1)

            abline(v = seq(length(K.values)+0.5,(nb.pts-length(K.values))+0.5,by = length(K.values)), col = "grey", lty = 2)
            # abline(v = seq((length(WT) * length(K.values))+0.5, (nb.pts - (length(WT) * length(K.values)))+0.5, by = length(WT) * length(K.values)), col = "blue", lty = 2)
            abline(h = c(1.125,2.375), col = "black", lty = 2)


            mtext(text = names[j-8], side = 2, line = 2, at = 1.75, font = 2, cex = 1)
            mtext(text = "Similarity weight", side = 3, line = 2, at = 25, font = 2, cex = 1)
            mtext(text = "Minimum weight", side = 1, line = 2, at = 25, font = 2, cex = 1)
            mtext(text = MW, side = 1, line = 1, at = seq(nb.pts/length(MW), nb.pts, by = nb.pts/length(MW)) - ((nb.pts/length(MW)) / 2) + 0.5, font = 1, cex = 0.75)
            mtext(text = rep(WT, times = length(WT)), side = 3, line = 1, at = seq((nb.pts/length(MW))/length(WT), nb.pts, by = ((nb.pts/length(MW)) / length(WT))) - ((nb.pts/length(MW)) / length(WT) / 2) + 0.5, font = 1, cex = 0.75)
            text(x = 1, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col[1], adj = 0)
            text(x = 1, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col[2], adj = 0)
            text(x = 1, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col[3], adj = 0)

        it <- 0
        for(i in 1:length(accuracy)) {
            accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'K']) + as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
            accuracy_mean <- accuracy_mean[order(accuracy_mean[,2]), ]
            # hack: we draw arrows but with very special "arrowheads" for error bars
            arrows(seq(1,nb.pts), accuracy_mean[, 3][,1] - accuracy_mean[, 3][, 2]+it, seq(1,nb.pts), accuracy_mean[, 3][, 1] + accuracy_mean[, 3][, 2]+it, length=0.025, angle=90, code=3, col = col[i])
            points(x = seq(1,nb.pts), y = accuracy_mean[, 3][, 1]+it, cex = 0.75, pch = 22, col = col[i])
            it <- it + 1.25
        } #i

        # ## Add legend
        # if(j == 9) {
        #     legend(0.5, 0.5, lwd = 3, col = col, legend = names(accuracy), bty = 'n', y.intersp = 1)
        # }
} #j

dev.off()


#Figure2
pdf(paste('./Article/',filename,'.pdf',sep=''),width=6,height=8)
# Plots
par(mfrow=c(3,1), mar=c(4,4,4,4))
# layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), heights = c(4,4,4))

nb.pts <- length(unique(accuracy[[1]][,'MW'])) * length(unique(accuracy[[1]][,'K'])) * length(unique(accuracy[[1]][,'wt']))

# Graph
for(j in c(10,11,9)) {
    j = 9
        eplot(xmin = -1, xmax = nb.pts+1, ymax = 3.6)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        foodwebs <- names(multiple_parameters2[[1]][[1]][[1]])
        col <- c("#FF8822","#449955","#2288FF")

        # Axes
            axis(side = 1, at = seq(0, nb.pts, by = length(K.values)), labels = FALSE, las = 1, pos = -0.2) #wt
            axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
            axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
            axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
            axis(side = 3, at = seq(0, nb.pts, by = 1), labels = FALSE, las = 1, pos = 1.2 + 2.5) #K.values
            axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
            axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
            axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))

            abline(v = seq(length(K.values),(nb.pts-length(K.values)),by = length(K.values)), col = "grey", lty = 2)
            # abline(v = seq((length(WT) * length(K.values))+0.5, (nb.pts - (length(WT) * length(K.values)))+0.5, by = length(WT) * length(K.values)), col = "blue", lty = 2)
            abline(h = c(1.125,2.375), col = "black", lty = 2)

            if(j == 9) {
                mtext(text = 'TSS', side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            } else if(j == 10) {
                mtext(text = expression('Score'[y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            } else if(j == 11) {
                mtext(text = expression('Score'[-y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            }

            mtext(text = "K values", side = 3, line = 2, at = nb.pts/2, font = 1.5, cex = 1)
            mtext(text = "Similarity weight", side = 1, line = 2.5, at = nb.pts/2, font = 1.5, cex = 1)
            mtext(text = rep(K.values, times = length(WT)), side = 3, line = 1, at = seq(0.5, nb.pts-0.5, by = 1), font = 1, cex = 0.75)
            mtext(text = rep(WT, times = length(WT)), side = 1, line = 1.5, at = seq((nb.pts/length(MW))/length(WT), nb.pts, by = ((nb.pts/length(MW)) / length(WT))) - ((nb.pts/length(MW)) / length(WT) / 2) + 0.5, font = 1, cex = 0.75)
            text(x = 1, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col[1], adj = 0)
            text(x = 1, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col[2], adj = 0)
            text(x = 1, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col[3], adj = 0)

        it <- 0
        for(i in 1:length(accuracy)) {
            accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'K']) + as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
            accuracy_mean <- accuracy_mean[order(accuracy_mean[,2]), ]
            # hack: we draw arrows but with very special "arrowheads" for error bars
            arrows(seq(1,nb.pts)-0.5, accuracy_mean[, 3][,1] - accuracy_mean[, 3][, 2]+it, seq(1,nb.pts)-0.5, accuracy_mean[, 3][, 1] + accuracy_mean[, 3][, 2]+it, length=0.025, angle=90, code=3, col = col[i])
            points(x = seq(1,nb.pts)-0.5, y = accuracy_mean[, 3][, 1]+it, cex = 0.75, pch = 22, col = col[i])
            it <- it + 1.25
        } #i
} #j

dev.off()


# ----- Force brute, pas le temps de gérer le problème de loop
#Figure2
cairo_pdf(paste('./Article/',filename,'.pdf',sep=''),width=6,height=8)
# Plots
par(mfrow=c(3,1), mar=c(4,4,4,4))
# layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), heights = c(4,4,4))

nb.pts <- length(unique(accuracy[[1]][,'MW'])) * length(unique(accuracy[[1]][,'K'])) * length(unique(accuracy[[1]][,'wt']))

# Graph 1
    j = 10
        eplot(xmin = -1, xmax = nb.pts+1, ymax = 3.6)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        foodwebs <- names(multiple_parameters2[[1]][[1]][[1]])
        col <- c("#FF8822","#449955","#2288FF")

        # Axes
            axis(side = 1, at = seq(0, nb.pts, by = length(K.values)), labels = FALSE, las = 1, pos = -0.2) #wt
            axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
            axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
            axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
            axis(side = 3, at = seq(0, nb.pts, by = 1), labels = FALSE, las = 1, pos = 1.2 + 2.5) #K.values
            axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
            axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
            axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))

            abline(v = seq(length(K.values),(nb.pts-length(K.values)),by = length(K.values)), col = "grey", lty = 2)
            # abline(v = seq((length(WT) * length(K.values))+0.5, (nb.pts - (length(WT) * length(K.values)))+0.5, by = length(WT) * length(K.values)), col = "blue", lty = 2)
            abline(h = c(1.125,2.375), col = "black", lty = 2)

            if(j == 9) {
                mtext(text = 'TSS', side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            } else if(j == 10) {
                mtext(text = expression('Score'[y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            } else if(j == 11) {
                mtext(text = expression('Score'[-y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
            }

            mtext(text = "K values", side = 3, line = 2, at = nb.pts/2, font = 1.5, cex = 1)
            mtext(text = "Similarity weight", side = 1, line = 2.5, at = nb.pts/2, font = 1.5, cex = 1)
            mtext(text = rep(K.values, times = length(WT)), side = 3, line = 1, at = seq(0.5, nb.pts-0.5, by = 1), font = 1, cex = 0.75)
            mtext(text = rep(WT, times = length(WT)), side = 1, line = 1.5, at = seq((nb.pts/length(MW))/length(WT), nb.pts, by = ((nb.pts/length(MW)) / length(WT))) - ((nb.pts/length(MW)) / length(WT) / 2) + 0.5, font = 1, cex = 0.75)
            text(x = 1, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col[1], adj = 0)
            text(x = 1, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col[2], adj = 0)
            text(x = 1, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col[3], adj = 0)

        it <- 0
        for(i in 1:length(accuracy)) {
            accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'K']) + as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
            accuracy_mean <- accuracy_mean[order(accuracy_mean[,2]), ]
            # hack: we draw arrows but with very special "arrowheads" for error bars
            arrows(seq(1,nb.pts)-0.5, accuracy_mean[, 3][,1] - accuracy_mean[, 3][, 2]+it, seq(1,nb.pts)-0.5, accuracy_mean[, 3][, 1] + accuracy_mean[, 3][, 2]+it, length=0.025, angle=90, code=3, col = col[i])
            points(x = seq(1,nb.pts)-0.5, y = accuracy_mean[, 3][, 1]+it, cex = 0.75, pch = 22, col = col[i])
            it <- it + 1.25
        } #i

        # Graph 2
            j = 11
                eplot(xmin = -1, xmax = nb.pts+1, ymax = 3.6)
                par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
                foodwebs <- names(multiple_parameters2[[1]][[1]][[1]])
                col <- c("#FF8822","#449955","#2288FF")

                # Axes
                    axis(side = 1, at = seq(0, nb.pts, by = length(K.values)), labels = FALSE, las = 1, pos = -0.2) #wt
                    axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
                    axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
                    axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
                    axis(side = 3, at = seq(0, nb.pts, by = 1), labels = FALSE, las = 1, pos = 1.2 + 2.5) #K.values
                    axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
                    axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
                    axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))

                    abline(v = seq(length(K.values),(nb.pts-length(K.values)),by = length(K.values)), col = "grey", lty = 2)
                    # abline(v = seq((length(WT) * length(K.values))+0.5, (nb.pts - (length(WT) * length(K.values)))+0.5, by = length(WT) * length(K.values)), col = "blue", lty = 2)
                    abline(h = c(1.125,2.375), col = "black", lty = 2)

                    if(j == 9) {
                        mtext(text = 'TSS', side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
                    } else if(j == 10) {
                        mtext(text = expression('Score'[y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
                    } else if(j == 11) {
                        mtext(text = expression('Score'[-y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
                    }

                    mtext(text = "K values", side = 3, line = 2, at = nb.pts/2, font = 1.5, cex = 1)
                    mtext(text = "Similarity weight", side = 1, line = 2.5, at = nb.pts/2, font = 1.5, cex = 1)
                    mtext(text = rep(K.values, times = length(WT)), side = 3, line = 1, at = seq(0.5, nb.pts-0.5, by = 1), font = 1, cex = 0.75)
                    mtext(text = rep(WT, times = length(WT)), side = 1, line = 1.5, at = seq((nb.pts/length(MW))/length(WT), nb.pts, by = ((nb.pts/length(MW)) / length(WT))) - ((nb.pts/length(MW)) / length(WT) / 2) + 0.5, font = 1, cex = 0.75)
                    text(x = 1, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col[1], adj = 0)
                    text(x = 1, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col[2], adj = 0)
                    text(x = 1, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col[3], adj = 0)

                it <- 0
                for(i in 1:length(accuracy)) {
                    accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'K']) + as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
                    accuracy_mean <- accuracy_mean[order(accuracy_mean[,2]), ]
                    # hack: we draw arrows but with very special "arrowheads" for error bars
                    arrows(seq(1,nb.pts)-0.5, accuracy_mean[, 3][,1] - accuracy_mean[, 3][, 2]+it, seq(1,nb.pts)-0.5, accuracy_mean[, 3][, 1] + accuracy_mean[, 3][, 2]+it, length=0.025, angle=90, code=3, col = col[i])
                    points(x = seq(1,nb.pts)-0.5, y = accuracy_mean[, 3][, 1]+it, cex = 0.75, pch = 22, col = col[i])
                    it <- it + 1.25
                } #i

                # Graph 3
                    j = 9
                        eplot(xmin = -1, xmax = nb.pts+1, ymax = 3.6)
                        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
                        foodwebs <- names(multiple_parameters2[[1]][[1]][[1]])
                        col <- c("#FF8822","#449955","#2288FF")

                        # Axes
                            axis(side = 1, at = seq(0, nb.pts, by = length(K.values)), labels = FALSE, las = 1, pos = -0.2) #wt
                            axis(side = 2, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
                            axis(side = 2, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
                            axis(side = 2, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = -0.2)
                            axis(side = 3, at = seq(0, nb.pts, by = 1), labels = FALSE, las = 1, pos = 1.2 + 2.5) #K.values
                            axis(side = 4, at = seq(0, 1, by = 0.25), labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
                            axis(side = 4, at = seq(0, 1, by = 0.25)+1.25, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))
                            axis(side = 4, at = seq(0, 1, by = 0.25)+2.5, labels = seq(0, 1, by = 0.25), las = 1, pos = (nb.pts + 0.2))

                            abline(v = seq(length(K.values),(nb.pts-length(K.values)),by = length(K.values)), col = "grey", lty = 2)
                            # abline(v = seq((length(WT) * length(K.values))+0.5, (nb.pts - (length(WT) * length(K.values)))+0.5, by = length(WT) * length(K.values)), col = "blue", lty = 2)
                            abline(h = c(1.125,2.375), col = "black", lty = 2)

                            if(j == 9) {
                                mtext(text = 'TSS', side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
                            } else if(j == 10) {
                                mtext(text = expression('Score'[y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
                            } else if(j == 11) {
                                mtext(text = expression('Score'[-y]), side = 2, line = 2, at = 1.75, font = 1.5, cex = 1)
                            }

                            mtext(text = "K values", side = 3, line = 2, at = nb.pts/2, font = 1.5, cex = 1)
                            mtext(text = "Similarity weight", side = 1, line = 2.5, at = nb.pts/2, font = 1.5, cex = 1)
                            mtext(text = rep(K.values, times = length(WT)), side = 3, line = 1, at = seq(0.5, nb.pts-0.5, by = 1), font = 1, cex = 0.75)
                            mtext(text = rep(WT, times = length(WT)), side = 1, line = 1.5, at = seq((nb.pts/length(MW))/length(WT), nb.pts, by = ((nb.pts/length(MW)) / length(WT))) - ((nb.pts/length(MW)) / length(WT) / 2) + 0.5, font = 1, cex = 0.75)
                            text(x = 1, y = 0.15, labels = 'Catalog', font = 2, cex = 1, col = col[1], adj = 0)
                            text(x = 1, y = 1.40, labels = 'Predictions', font = 2, cex = 1, col = col[2], adj = 0)
                            text(x = 1, y = 2.65, labels = 'Algorithm', font = 2, cex = 1, col = col[3], adj = 0)

                        it <- 0
                        for(i in 1:length(accuracy)) {
                            accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'K']) + as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
                            accuracy_mean <- accuracy_mean[order(accuracy_mean[,2]), ]
                            # hack: we draw arrows but with very special "arrowheads" for error bars
                            arrows(seq(1,nb.pts)-0.5, accuracy_mean[, 3][,1] - accuracy_mean[, 3][, 2]+it, seq(1,nb.pts)-0.5, accuracy_mean[, 3][, 1] + accuracy_mean[, 3][, 2]+it, length=0.025, angle=90, code=3, col = col[i])
                            points(x = seq(1,nb.pts)-0.5, y = accuracy_mean[, 3][, 1]+it, cex = 0.75, pch = 22, col = col[i])
                            it <- it + 1.25
                        } #i


dev.off()
