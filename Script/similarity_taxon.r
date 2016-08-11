#Similarity matrix as a single functions
similarity_taxon <- function(S0, wt) {
    # Note: the similarity on the diagonal has to be set to 1 since it's all the same species.

    similarity.matrix <- matrix(nrow = nrow(S0), ncol = nrow(S0), dimnames = list(S0[, 'taxon'], S0[, 'taxon']))

    taxonomy <- vector("list",nrow(S0))
    resource <- vector("list",nrow(S0))
    for(i in 1:nrow(S0)) {
      taxonomy[[i]] <- unlist(strsplit(S0[i, 'taxonomy'], " \\|\\ "))
      if(length(which(taxonomy[[i]] == "NA")) > 0) {
          taxonomy[[i]] <- taxonomy[[i]][-which(taxonomy[[i]] == "NA")]
      }
      resource[[i]] <- unlist(strsplit(S0[i, 'resource'], " \\|\\ "))
    }

    pb <- txtProgressBar(min = 0,max = nrow(S0), style = 3)

    for(i in 1:nrow(S0)){
      for(j in i:nrow(S0)){ #No need to evaluate both side of the matrix diagonal for the similarity matrix
          similarity.matrix[i,j] <- similarity.matrix[j,i] <- tanimoto_traits(resource_x = resource[[i]],
                                                                              resource_y = resource[[j]],
                                                                              trait_x = taxonomy[[i]],
                                                                              trait_y = taxonomy[[j]],
                                                                              trait_weight = wt
                                                                              )
      }#j
    setTxtProgressBar(pb, i)
    }#i
    close(pb)

    diag(similarity.matrix) <- 1

  return(similarity.matrix)
}#similarity_taxon function
