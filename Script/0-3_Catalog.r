load("./RData/Tanimoto_data.RData")
# S0: A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource', 'consumer', 'non-consumer']
# Format interaction catalogue to fit this table format
    S0_catalog <- matrix(nrow = nrow(Tanimoto_data[[1]]), ncol = 6, data = "", dimnames = list(Tanimoto_data[[1]][, 'taxon'], c('taxon', 'taxonomy', 'resource', 'non-resource', 'consumer', 'non-consumer')))
    S0_catalog[, 1] <- Tanimoto_data[[1]][, 'taxon']
    S0_catalog[, 2] <- Tanimoto_data[[1]][, 'kingdom | phylum | class | order | family | genus | species']
    # From binary interactions catalogue with consumer, resources, interaction or non-interaction
    for(k in 1:nrow(Tanimoto_data[[3]])) {
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 3] <- Tanimoto_data[[3]][k, 'resource']
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 4] <- Tanimoto_data[[3]][k, 'non-resource']
    }
    for(k in 1:nrow(Tanimoto_data[[6]])) {
        S0_catalog[Tanimoto_data[[6]][k, 'resource'], 5] <- Tanimoto_data[[6]][k, 'consumer']
        S0_catalog[Tanimoto_data[[6]][k, 'resource'], 6] <- Tanimoto_data[[6]][k, 'non-consumer']
    }

    save(x = S0_catalog, file = "./RData/S0_catalog.RData")
