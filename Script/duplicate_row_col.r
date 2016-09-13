dupl_sp <- function(z){
  #Combining duplicate names in food webs
      x <- rownames(z)
      x2 <- colnames(z)
      y <- z
      y <- aggregate(y~x,FUN=sum)
      rownames(y) <- y[,1]
      y <- y[,2:ncol(y)]
      colnames(y) <- x2
      y <- aggregate(t(y)~x2,FUN=sum)
      rownames(y) <- y[,1]
      y <-t(y[,2:ncol(y)])
      for(i in 1:nrow(y)){
        for(j in 1:ncol(y)){
          if(y[i,j] > 0) {y[i,j] <- 1} else {NULL}
        }
      }
      z <- y
      return(z)
    }
