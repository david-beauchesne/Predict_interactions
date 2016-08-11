#Similarity matrix as a single functions
similarity_taxon_predict <- function(S0, S1, wt, similarity.matrix) {

    # Parameters:
    #   S0                  A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource'] (if blind = TRUE, it )
    #   S1                  The subset of S0 where we want to predict new preys, string vector
    #   wt                  Weight given to traits (including phylogeny) when computing similarities.
    #   similarity.matrix   Similarity.matrix for interaction catalogue (measured by function: similarity_taxon)
    #
    # Output
    #   A matrix of dimensions S x S with similarity measurements for all combinations of S[i,j]

    # Note: the similarity on the diagonal has to be set to 1 since it's all the same species.

    to.recalculate <- which(S0[, 'taxon'] %in% S1)
    similarity.matrix[, to.recalculate] <- similarity.matrix[to.recalculate, ] <- 0

    taxonomy <- vector("list",nrow(S0))
    resource <- vector("list",nrow(S0))
    for(i in 1:nrow(S0)) {
      taxonomy[[i]] <- unlist(strsplit(S0[i, 'taxonomy'], " \\|\\ "))
      if(length(which(taxonomy[[i]] == "NA")) > 0) {
          taxonomy[[i]] <- taxonomy[[i]][-which(taxonomy[[i]] == "NA")]
      }
      resource[[i]] <- unlist(strsplit(S0[i, 'resource'], " \\|\\ "))
    }


  # pb <- txtProgressBar(min = (nrow(similarity.matrix) - length(taxa.add)), max = ncol(similarity.matrix), style = 3)

  for(i in to.recalculate){
      for(j in 1:ncol(similarity.matrix)){ #No need to evaluate both side of the matrix diagonal for the similarity matrix
          similarity.matrix[i,j] <- similarity.matrix[j,i] <- tanimoto_traits(resource_x = resource[[i]],
                                                                              resource_y = resource[[j]],
                                                                              trait_x = taxonomy[[i]],
                                                                              trait_y = taxonomy[[j]],
                                                                              trait_weight = wt
                                                                              )
      }#j
  # setTxtProgressBar(pb, i)
  }#i
  # close(pb)
  diag(similarity.matrix) <- 1
  return(similarity.matrix)
}#similarity_taxon_predict function
