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
load("./RData/Tanimoto_data.RData")

# Measuring the similarity with multiple weights for all taxa in interaction catalogue
# Will be better once we code for proximity graphs

# S0: A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource', 'consumer', 'non-consumer']
# Format interaction catalogue to fit this table format
    S0_catalog <- matrix(nrow = nrow(Tanimoto_data[[1]]), ncol = 6, data = "", dimnames = list(Tanimoto_data[[1]][, 'taxon'], c('taxon', 'taxonomy', 'resource', 'non-resource', 'consumer', 'non-consumer')))
    S0_catalog[, 1] <- Tanimoto_data[[1]][, 'taxon']
    S0_catalog[, 2] <- Tanimoto_data[[1]][, 'kingdom | phylum | class | order | family | genus | species']
    # From binary interactions catalogue with consumer, resources, interaction or non-interaction
    for(k in 1:nrow(Tanimoto_data[[3]])) {
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 3] <- Tanimoto_data[[3]][k, 'resource']
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 4] <- Tanimoto_data[[3]][k, 'non-resource']
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 5] <- Tanimoto_data[[6]][k, 'consumer']
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 6] <- Tanimoto_data[[6]][k, 'non-consumer']

    }

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
