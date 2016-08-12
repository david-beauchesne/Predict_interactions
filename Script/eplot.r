# Creating an empty plot
eplot <- function(x = 1, y = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1) {
  plot(x = x, y = y, bty = "n",ann = FALSE,xaxt = "n",yaxt = "n",type = "n",bg = "grey", ylim = c(ymin,ymax), xlim = c(xmin,xmax))
}
