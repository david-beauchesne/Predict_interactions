# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    1. Evaluating similarity of consumers and resources
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
# // TODO: Evaluate similarity based on set of consumers for resources
# // TODO: Look into proximity graphs for better performance
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------

# Measuring the similarity with multiple weights for all taxa in interaction catalogue
# Will be better once we code for proximity graphs
    wt <- seq(0, 1, by = 0.1)
    similarity.matrices <- vector('list',11)
    names(similarity.matrices) <- seq(0, 1, by = 0.1)
    for(i in 1:length(wt)) {
        similarity.matrices[[i]] <- similarity_taxon(S0 = S0_catalog, wt = wt[i])
        save(x = similarity.matrices, file = "RData/Similarity.matrices.RData")
    }
    save(x = similarity.matrices, file = "RData/Similarity.matrices.RData")
