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
load("./RData/S0_catalog.RData")
# Weight values for 2-way similarity measurements
    wt <- seq(0, 1, by = 0.1)

# 1st is for similarity measured from set of resources and taxonomy, for consumers
    similarity.consumers <- vector('list',11)
    names(similarity.consumers) <- wt
    for(i in 1:length(wt)) {
        similarity.consumers[[i]] <- similarity_taxon(S0 = S0_catalog, wt = wt[i], taxa = 'consumer')
        save(x = similarity.consumers, file = "./RData/Similarity_consumers.RData")
    }
    save(x = similarity.consumers, file = "./RData/Similarity_consumers.RData")

# 2nd is for similarity measured from set of consumers and taxonomy, for resources
    similarity.resources <- vector('list',11)
    names(similarity.resources) <- wt
    for(i in 1:length(wt)) {
        similarity.resources[[i]] <- similarity_taxon(S0 = S0_catalog, wt = wt[i], taxa = 'resource')
        save(x = similarity.resources, file = "./RData/Similarity_resources.RData")
    }
    save(x = similarity.resources, file = "./RData/Similarity_resources.RData")
