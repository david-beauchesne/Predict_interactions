catalog_predictions <- function(min.tx = 45, K.values, MW, WT, minimum_threshold, similarity = 'both', filename, percent_remove = 0, nb_iter = 1, comm_id = FALSE, community) {

    # -----------------------------------------------------------------------------
    # # PARAMETERS:
    #     filename                name of file under which to same the results of the predictions
    #     min.tx                  minimal number of taxon for empirical food webs to be included in the analysis
    #     K.values                Kc and Kr values to test in the KNN algorithm
    #     MW                      Minimum weight for candidate resources to be included as predictions
    #     WT                      Weights for the two-way Tanimoto algorithm
    #     blind                   Whether the analysis whould be blind, i.e. no a priori information for taxa in catalog
    #     minimum_threshold       Minimum similarity threshold for similar taxa to be considered as candidate resources
    #     similarity              String character either being c('consumer', 'resource', 'both') for the similarity measurements
    #
    # # OUTPUT:
    #     tanimoto_analysis       List of predictions for all parameters tested
    # -----------------------------------------------------------------------------
    load("./RData/Tanimoto_data.RData")
    load("./RData/interactions_source.RData")
    load("./RData/S0_catalog.RData")

    if(similarity == 'both') { # For similarity matrices already evaluated
        suppressMessages(load("./RData/Similarity_consumers.RData"))
        suppressMessages(load("./RData/Similarity_resources.RData"))
    } else if(similarity == 'consumer') {
        suppressMessages(load("./RData/Similarity_consumers.RData"))
    } else if (similarity == 'resource') {
        suppressMessages(load("./RData/Similarity_resources.RData"))
    }

    # setting up the analyses for multiple communities
    # Data for communities on which to test the algorithm
        Cm <- unique(interactions_sources[, 'source'])
        communities <- vector("list", length(Cm))
        names(communities) <- Cm

        # Taxa list per community to predict
            for(i in 1:length(communities)) {
                Ci <- which(interactions_sources[, 'source'] == Cm[i])
                S1 <- unique(c(interactions_sources[Ci, 'consumer'], interactions_sources[Ci, 'resource']))

                if(length(which(!S1 %in% S0_catalog)) > 0) {
                    print('Taxa in C[i] are not all included in taxa list S0')
                    break
                }

                communities[[i]] <- S1
            }

    # Substracting GloBI interactions for this portion
        Cm.lg <- numeric()
        for(i in 1:length(communities)) {
            Cm.lg <- c(Cm.lg,length(communities[[i]]))
        }

        if(comm_id == FALSE) {
            to.delete <- c(which(Cm.lg < min.tx), which(Cm.lg > 1000)) # > 1000 is for GloBI
            Cm <- Cm[-to.delete]
            for(i in rev(to.delete)) {
                communities[[i]] <- NULL
            }
            names(communities) <- Cm
        } else { # comm_id = TRUE
            to.delete <- which(!Cm %in% community)
            Cm <- Cm[-to.delete]
            for(i in rev(to.delete)) {
                communities[[i]] <- NULL
            }
            names(communities) <- Cm
        }

    # Setting up lists to store the results
    # weights
        wt.init <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        wt.remove <- which(!wt.init %in% WT)# selecting a subset of similarity matrices
        for(i in rev(wt.remove)) {
            if(similarity == 'both') { # For similarity matrices already evaluated
                similarity.consumers[[i]] <- NULL
                similarity.resources[[i]] <- NULL
            } else if(similarity == 'consumer') {
                similarity.consumers[[i]] <- NULL
            } else if (similarity == 'resource') {
                similarity.resources[[i]] <- NULL
            }
        }

        sim.wt <- WT
        Tanimoto_analysis <- vector("list",length(sim.wt))
        names(Tanimoto_analysis) <- sim.wt
        for(i in 1:length(sim.wt)) {
            Tanimoto_analysis[[i]] <- vector("list", length(Cm))
            names(Tanimoto_analysis[[i]]) <- Cm
        }

        # List to store results of multiple K values
        K <- vector("list", length(K.values))
        for(i in 1:length(K.values)) {
            K[[i]] <- Tanimoto_analysis
        }
        Tanimoto_analysis <- K
        names(Tanimoto_analysis) <- K.values
        remove(K)

        #Minimum weight
        min.wt <- vector("list", length(MW))
        for(i in 1:length(MW)) {
            min.wt[[i]] <- Tanimoto_analysis
        }
        Tanimoto_analysis <- min.wt
        names(Tanimoto_analysis) <- MW
        remove(min.wt)

        #Number of iterations
        iter <- vector('list', nb_iter)
            for(i in 1:nb_iter) {
                iter[[i]] <- Tanimoto_analysis
            }
        Tanimoto_analysis <- iter
        names(Tanimoto_analysis) <- seq(1,nb_iter)
        remove(iter)


        # Percent remove in communities
        pc_rm <- vector('list', length(percent_remove))
            for(i in 1:length(percent_remove)) {
                pc_rm[[i]] <- Tanimoto_analysis
            }
        Tanimoto_analysis <- pc_rm
        names(Tanimoto_analysis) <- percent_remove
        remove(pc_rm)

    # Initial time save for temporary saving in case analysis fails mid process
    file.to.save <- serialNext("./Analyses/Tanimoto_temp/Tanimoto_analysis.RData")
    save(x = Tanimoto_analysis, file = file.to.save)

    iteration <- 1
    init.time <- Sys.time()
    pb <- txtProgressBar(min = 0,max = length(percent_remove) * nb_iter * length(Cm) * length(WT) * length(K.values) * length(MW), style = 3)
    for(p in 1:length(percent_remove)){
        percent_rm <- percent_remove[p]
        for(o in 1:nb_iter){
            for(n in 1:length(MW)) {
                mw <- MW[n]
                for(m in 1:length(K.values)) {

                    # Tanimoto analysis with different weights for different communities
                        # Parameters:
                            Kc <- K.values[m]  # Integer, how many neighbors to select for consumers
                            Kr <-  K.values[m]  # Integer, how many neighbors to select for resources
                            # MW = minimum_weight  # Mimimum weight to accept a candidate as a prey
                        #   wt  Weight of traits in similarity measurement
                        #   S0  A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource']
                        #   S1  The subset of S0 where we want to predict new preys, string vector with taxa name

                        # Output:
                        #   A vector of sets of resources for each taxon
                        for(i in 1:length(WT)){ #1st loop for all types of wt values
                            wt <- WT[i]
                            for(j in 1:length(Cm)) { #2nd loop for all C[i]
                                S1 <- communities[[j]]
                                S0 <- S0_catalog

                                if(similarity == 'both') { # For similarity matrices already evaluated
                                    similarity.consumer <- similarity.consumers[[i]]
                                    similarity.resource <- similarity.resources[[i]]
                                } else if(similarity == 'consumer') {
                                    similarity.consumer <- similarity.consumers[[i]]
                                } else if (similarity == 'resource') {
                                    similarity.resource <- similarity.resources[[i]]
                                }

                                # setting up the iterative process to evaluate the accuracy ~ # taxa in catalog
                                # removing a certain percentage of the # of species for which there are interactions as consumers described in the original food web.

                                # inter_Cm <- unique(subset(interactions_sources[, 'consumer'], interactions_sources[, 'source'] == Cm[j] & interactions_sources[, 'inter'] == "1")) # Species for which there are interactions as consumer in Cm[j]

                                interactions <- interactions_sources[-which(interactions_sources[, 'source'] == Cm[j]), 1:3] # interaction catalog without interactions coming from Cm[j]

                                # inter_Cm2 <- unique(interactions[which(interactions[, 'consumer'] %in% inter_Cm), 'consumer']) # consumers in Cm[j] for which information is still available in catalog after deletion of Cm[j] from catalog

                                #Removing a percentage of consumers described in catalog
                                    # sample_iter <- sample(x = inter_Cm2, size = round((percent_rm / 100) * length(inter_Cm2)), replace = FALSE)
                                    sample_iter <- sample(x = S1, size = round((percent_rm / 100) * length(S1)), replace = FALSE) # To use if removing a percent of all taxa in original web

                                    if(length(sample_iter) == 0) {
                                        S1_no_mod <- seq(1,length(S1))
                                    } else {
                                        for(k in 1:length(sample_iter)) {
                                          S0[sample_iter[k], 'resource'] <- ""
                                          S0[sample_iter[k], 'non-resource'] <- ""
                                          S0[sample_iter[k], 'consumer'] <- ""
                                          S0[sample_iter[k], 'non-consumer'] <- ""
                                        }
                                        S1_no_mod <- which(!S1 %in% sample_iter)
                                    }

                                # 2. Preexisting information kept to inform algorithm
                                    if(length(S1_no_mod) == 0) {
                                        NULL
                                    } else {
                                    # Only modifying those that are loosing data from the catalogue, less time
                                        to.change <- numeric()
                                        for(k in 1:length(S1_no_mod)) {
                                            to.change <- c(to.change, which(interactions[, 'consumer'] == S1[S1_no_mod[k]]), which(interactions[, 'resource'] == S1[S1_no_mod[k]]))
                                        }
                                        to.change <- unique(to.change)

                                    # Modifying sets of resources and non-resources for taxa in S1_no_mod
                                        interactions <- interactions[to.change, ]
                                        rownames(interactions) <- seq(1,nrow(interactions))
                                        resource_set <- consumer_set_of_resource(consumer = interactions[, 'consumer'],
                                                                                  resource = interactions[, 'resource'],
                                                                                  inter_type = interactions[, 'inter'])

                                        consumer_set <- resource_set_of_consumer(consumer = interactions[, 'consumer'],
                                                                                resource = interactions[, 'resource'],
                                                                                inter_type = interactions[, 'inter'])


                                    # From binary interactions catalogue with consumer, resources, interaction or non-interaction (rownames need to == taxa name)
                                        for(k in 1:nrow(resource_set)) {
                                          S0[resource_set[k, 'consumer'], 3] <- resource_set[k, 'resource']
                                          S0[resource_set[k, 'consumer'], 4] <- resource_set[k, 'non-resource']
                                        }
                                        for(k in 1:nrow(consumer_set)) {
                                          S0[consumer_set[k, 'resource'], 5] <- consumer_set[k, 'consumer']
                                          S0[consumer_set[k, 'resource'], 6] <- consumer_set[k, 'non-consumer']
                                        }
                                    remove(interactions, resource_set, to.change)
                                    }#if

                                # Recalculate similarity
                                    similarity.consumer <- similarity_taxon_predict(S0 = S0,
                                                                                        S1 = S1,
                                                                                        wt = wt,
                                                                                        similarity.matrix = similarity.consumer,
                                                                                        taxa = 'consumer')

                                    similarity.resource <- similarity_taxon_predict(S0 = S0,
                                                                                        S1 = S1,
                                                                                        wt = wt,
                                                                                        similarity.matrix = similarity.resource,
                                                                                        taxa = 'resource')

                                # Predicting interactions
                                    Tanimoto_analysis[[p]][[o]][[n]][[m]][[i]][[j]] <- two_way_tanimoto_predict(Kc = Kc,
                                                                                            Kr = Kr,
                                                                                            S0 = S0,
                                                                                            S1 = S1,
                                                                                            MW = mw,
                                                                                            similarity.consumer = similarity.consumer,
                                                                                            similarity.resource = similarity.resource,
                                                                                            minimum_threshold = minimum_threshold)

                                save(x = Tanimoto_analysis, file = file.to.save)
                                remove(S0, S1, similarity.consumer, similarity.resource)
                                iteration <- iteration + 1
                                setTxtProgressBar(pb, iteration)
                            }#2nd loop for all C[i]

                            save(x = Tanimoto_analysis, file = file.to.save)
                            remove(wt)

                        }#1st loop for all types of wt values
                }#m
            }#n
        }#o
    }#p
    #Saving number of species in original web vs catalog once web removed
    # percent_original <- length(inter_Cm2) / length(inter_Cm)
    # x <- c(percent_original, length(inter_Cm), length(inter_Cm2))
    # file.to.save2 <- serialNext("./Analyses/Tanimoto_temp/Tanimoto_analysis_pc_tx.RData")
    # save(x = x, file = file.to.save2)

    close(pb)
    print(Sys.time() - init.time)

    save(x = Tanimoto_analysis, file = paste('./Analyses/',filename,'.RData',sep=''))

    return(Tanimoto_analysis)
}
