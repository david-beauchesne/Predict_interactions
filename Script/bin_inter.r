#Extract binary interactions from diet matrix
  bin_inter <- function(x) {

    bin.inter <- matrix(nrow=ncol(x)*nrow(x),ncol=3,data=NA)
    colnames(bin.inter) = c("Predator","FeedInter","Prey")

    for(i in 1:ncol(x)){
      for(j in 1:nrow(x)){
        bin.inter[((i*nrow(x))-nrow(x))+j,1] <- colnames(x)[i]
        bin.inter[((i*nrow(x))-nrow(x))+j,2] <- as.matrix(x[j,i])
        bin.inter[((i*nrow(x))-nrow(x))+j,3] <- rownames(x)[j]
      }
    }

    return(bin.inter)
  }
