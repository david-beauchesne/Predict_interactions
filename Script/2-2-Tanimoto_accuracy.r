# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#   2. Evaluation of analysis accuracy + tables and figures
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#           Script <- file = 'Script/2-2-Tanimoto_accuracy.r'
#           RData <- file = 'RData/Tanimoto_accuracy.RData'
#           Figures <- file = ''
#           Tables <- file = ''
# -----------------------------------------------------------------------------

load("RData/Tanimoto_analysis.RData")

Tanimoto_accuracy <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis)

# Creating an empty plot
eplot <- function(x, y) {
  plot(x = x, y = y, bty = "n",ann = FALSE,xaxt = "n",yaxt = "n",type = "n",bg = "grey", ylim = c(-0.09,1.09), xlim = c(-0.09,1.09))
}

pdf("Article/results4.pdf",width=7,height=7)
# Plots
par(mfrow=c(2,2))
# Graph
for(j in 8:11) {
    eplot(x = accuracy[, 'wt'], y = accuracy[, j])
    par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
    # foodwebs <- to.verify
    # col <- c('blue','green','black','red','yellow','darkgrey','orange','brown','grey','green','darkgreen','darkblue')
    col <- gray.colors(11, start = 0, end = 0.8, gamma = 2.2, alpha = NULL)
    names <- c('TSS','Score y', 'Score -y', 'Accuracy score')
    # sample(colours(), length(foodwebs))
    # cols <- c("#FF000088","#00FF0088","#0000FF88")
    # cols2 <- c("#FF0000","#00FF00","#0000FF")

    # Axes
    # rect(0, 0, 1, 1, col = "#eeeeee", border = NA)
    axis(side = 1, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 0)
    axis(side = 2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.09)
    axis(side = 3, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 1)
    axis(side = 4, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 1.09)
    # abline(v = seq(0,6,by = 2), col = "white", lty = 2)
    # abline(h = seq(1,2,by = 1), col = "white", lty = 2)

    #
    mtext(text = names[j-7], side = 2, line = 2, at = 0.5, font = 2, cex = 1)
    mtext(text = "Similarity weight", side = 1, line = 2, at = 0.5, font = 2, cex = 1)

    for(i in 1:12) {
        x <- as.numeric(names(Tanimoto_analysis[[1]]))
        y <- numeric()
        for(k in 1:6) {
            y <- c(y,mean(as.numeric(accuracy[which(accuracy[, 'K'] == i & accuracy[, 'wt'] == names(Tanimoto_analysis[[1]])[k]), j][-5])))
            # mean(as.numeric(accuracy[which(accuracy[, 'K'] == i & accuracy[, 'wt'] == names(Tanimoto_analysis[[1]])[k]), j][-5]))
        }

        points(x = x, y = y, bg = col[i], cex = 1.25, pch = 18, col = col[i])
        lines(x = x, y = y, col = col[i], lwd = 0.5)

        # points(x = accuracy[which(accuracy[, 'K'] == i), 'wt'], y = accuracy[which(accuracy[, 'K'] == i), j], bg = col[i], cex = 1.25, pch = 18, col = col[i])
        # lines(x = accuracy[which(accuracy[, 'K'] == i), 'wt'], y = accuracy[which(accuracy[, 'K'] == i), j], col = col[i], lwd = 0.5)
    }

    # boxplot(formula = as.numeric(accuracy[, j]) ~ accuracy[, 'wt'],
    #         data = accuracy,
    #         boxwex = 0.075,
    #         axes = FALSE,
    #         add = TRUE,
    #         at = seq(0, 1, by = 0.1))
}
dev.off()
