#Similarity matrix as a single functions
similarity_full_algorithm <- function(S0, S1, wt, taxa) {
    # Note: the similarity on the diagonal has to be set to 1 since it's all the same species.
    # taxa is either resource or consumer

    similarity.matrix <- matrix(nrow = nrow(S0), ncol = 1, dimnames = list(S0[, 'taxon'], S1))

    taxonomy <- vector("list",nrow(S0))
    taxon <- vector("list",nrow(S0))

    for(i in 1:nrow(S0)) {
      taxonomy[[i]] <- unlist(strsplit(S0[i, 'taxonomy'], " \\|\\ "))
      if(length(which(taxonomy[[i]] == "NA")) > 0) {
          taxonomy[[i]] <- taxonomy[[i]][-which(taxonomy[[i]] == "NA")]
      }

      if(taxa == 'consumer') {
          taxon[[i]] <- unlist(strsplit(S0[i, 'resource'], " \\|\\ "))
      } else if(taxa == 'resource') {
          taxon[[i]] <- unlist(strsplit(S0[i, 'consumer'], " \\|\\ "))
      }
    }

    # pb <- txtProgressBar(min = 0,max = nrow(S0), style = 3)

    for(i in 1:nrow(S0)){
        similarity.matrix[i,1] <- tanimoto_traits(resource_x = taxon[[which(S0[, 'taxon'] == S1)]],
                                                    resource_y = taxon[[i]],
                                                    trait_x = taxonomy[[which(S0[, 'taxon'] == S1)]],
                                                    trait_y = taxonomy[[i]],
                                                    trait_weight = wt
                                                    )
    # setTxtProgressBar(pb, i)
    }#i
    # close(pb)

  return(similarity.matrix)
}#similarity_taxon function
