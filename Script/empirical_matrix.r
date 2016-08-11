# Extracting observed interaction matrix
empirical_matrix <- function(S1, interactions_source, source) {
    # Parameters:
    #   S1                      Set of taxa in predicted community structure
    #   interactions_source     Binary interactions with source identified
    #   source                  Source of the empirical community

    #   Output                  Food web matrix with dimensions S1 x S1 with consumers as columns
    empirical.matrix <- matrix(nrow = length(S1), ncol = length(S1), data = 0, dimnames = list(S1,S1))
    interactions <- interactions_sources[which(interactions_sources[, 'source'] == source), ]
    inter <- which(interactions[, 'inter'] == 1)

    for(i in inter) {
        empirical.matrix[interactions[i, 'resource'], interactions[i, 'consumer']] <- 1
    }
    return(empirical.matrix)
}
