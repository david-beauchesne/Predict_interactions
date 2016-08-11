tanimoto_traits <- function(resource_x, resource_y, trait_x, trait_y, trait_weight) {
    # The Tanimoto similarity with traits measures the similarity between vectors of traits as well as similarity between vectors of resources
    # tanimotot(x; y;wt) = wttanimoto(xt; yt) + (1 􀀀 wt)tanimoto(xi; yi);
    # If trait_weight == 1, only traits are used for similarity measurement
    # If trait_weight == 0, only shared resources measures the similarity between species

    # !!! Does it make sense to measure taxonomic proximity linearly like this? I need to look into taxonomic proximity measurements in packages like rphylo !!!


    return(trait_weight * tanimoto(trait_x, trait_y) + (1 - trait_weight) * tanimoto(resource_x, resource_y))
}
