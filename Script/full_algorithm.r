full_algorithm <- function(Kc, Kr, S0, S1, MW, wt, minimum_threshold) {
    # Two-way Tanimoto Algorithm
    # ===========================

    # Parameters:
    #   Kc  Integer, how many neighbors to select for consumers
    #   Kr  Integer, how many neighbors to select for resources
    #   S0  A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource']
    #   S1  The subset of S0 where we want to predict new preys, string vector with taxa name
    #   MW  Mimimum weight to accept a candidate as a prey

    # Output
    #   A vector of sets (the preys for each species)

    predictions <- matrix(nrow = length(S1), ncol = 3, data = "", dimnames = list(c(S1), c('consumer','resource_empirical','resource_predictions'))) # empty object for resource predictions
    predictions[, 'consumer'] <- S1

    pb <- txtProgressBar(min = 0,max = length(S1), style = 3)
    for(i in 1:length(S1)) { # loop through each taxon in S1
        candidates <- matrix(nrow = 0, ncol = 2, dimnames = list(c(), c('resource', 'weight')), data = NA) # empty matrix for resource candidate list for S1[i], with taxon name and weight
        resources.S1 <- unlist(strsplit(S0[S1[i], 'resource'], " \\|\\ ")) # resources of S1[i]

        # Add resources that are already listed as resources for S1[i] in predictions[, 'resource_empirical'] or
        # Find similar resources to resources for S1[i] in S1
        if(length(resources.S1) > 0) {
            empirical <- character()
            for(j in 1:length(resources.S1)) { #loop through empirical resources for S1
                if(resources.S1[j] %in% S1) {
                    empirical <- c(empirical, resources.S1[j]) # observed resource found in S1 are automatically added to the column resource_empirical
                } else { # selecting Kr most similar resources in S1
                    # Let's assume for this part that we are not compiling a different similarity measure for predators and preys.

                    similarity.resources <- similarity_full_algorithm(S0 = unique(S0[which(S0[, 'taxon'] %in% S1 | S0[, 'taxon'] == resources.S1[j]), ]), # S1 in S0 + resource for which similarity has to be measured
                                                                    S1 = resources.S1[j], # resource for which similarity has to be measured
                                                                    wt = wt,
                                                                    taxa = 'resource')

                    similar.resource <- matrix(nrow = nrow(similarity.resources), ncol = 2, dimnames = list(c(), c('resource','similarity')), data = NA) # importing K nearest neighbors resources
                    similar.resource[, 'resource'] <- names(similarity.resources[order(similarity.resources, decreasing = TRUE), ])
                    similar.resource[, 'similarity'] <- similarity.resources[order(similarity.resources, decreasing = TRUE)]
                    to.remove <- which(similar.resource[,'resource'] == resources.S1[j])
                    if(length(to.remove) > 0){ #remove resoures.S1[j] in case it gets through (just keeping it consistant with other similarity evaluation further down in the catalogue, even though it is not necessary in this portion)
                        similar.resource <- similar.resource[-which(similar.resource[,'resource'] == resources.S1[j]), ]
                    }

                    # If multiple taxa with same similarity, randomly select those that will be used as similar resources.
                    if(similar.resource[Kr+1, 'similarity'] == similar.resource[Kr, 'similarity']) {
                        same.similarity <- which(similar.resource[, 'similarity'] == similar.resource[Kr, 'similarity'])
                        similar.resource[same.similarity, ] <- similar.resource[sample(same.similarity), ]
                        similar.resource <- similar.resource[1:Kr, ]
                    } else {
                        similar.resource <- similar.resource[1:Kr, ]
                    }# if for random draw

                    for(l in 1:Kr) { # extracting resource candidates
                        if(all.equal(similar.resource[, 'similarity'], rep('0',Kr)) == TRUE) { # if similarities all == 0, break
                            break
                        } else if(similar.resource[l, 'similarity'] == '0') { # if similarity l == 0, no candidates provided
                            NULL
                            # minimum threshold try.. adding it as a Parameters.. might not make sense, have to discuss it. If we keep it, previous else ifs can be removed
                        } else if(similar.resource[l, 'similarity'] < minimum_threshold) {
                            NULL
                        } else if((similar.resource[l, 'resource'] %in% candidates[, 'resource']) == TRUE) { # if candidate is already in candidate list, add resource' with wt to its weight
                          candidates[which(candidates[, 'resource'] == similar.resource[l]), 'weight'] <- as.numeric(candidates[which(candidates[, 'resource'] == similar.resource[l]), 'weight']) + as.numeric(similar.resource[l, 'similarity'])
                        } else {
                              candidates <- rbind(candidates, similar.resource[l, ]) # if candidate is not in the list, add it resource' with wt to its weight
                        }#if3
                    }#l
                }#if
            }#j
            predictions[S1[i], 'resource_empirical'] <- paste(empirical, collapse = ' | ')
        }#if1

        # Identify similar consumers to S1[i]

        similarity.consumers <- similarity_full_algorithm(S0 = S0,
                                                        S1 = S1[i],
                                                        wt = wt,
                                                        taxa = 'consumer')

        similar.consumer <- matrix(nrow = nrow(similarity.consumers), ncol = 2, dimnames = list(c(), c('consumer','similarity')), data = NA) # importing K nearest neighbors consumers
        similar.consumer[, 'consumer'] <- names(similarity.consumers[order(similarity.consumers, decreasing = TRUE), ])
        similar.consumer[, 'similarity'] <- similarity.consumers[order(similarity.consumers, decreasing = TRUE)]
        to.remove <- which(similar.consumer[,'consumer'] == S1[i])
        if(length(to.remove) > 0){ #remove resoures.S1[j] in case it gets through (just keeping it consistant with other similarity evaluation further down in the catalogue, even though it is not necessary in this portion)
            similar.consumer <- similar.consumer[-which(similar.consumer[,'consumer'] == S1[i]), ]
        }

        # If multiple taxa with same similarity, randomly select those that will be used as similar resources.
        if(similar.consumer[Kc+1, 'similarity'] == similar.consumer[Kc, 'similarity']) {
            same.similarity <- which(similar.consumer[, 'similarity'] == similar.consumer[Kc, 'similarity'])
            similar.consumer[same.similarity, ] <- similar.consumer[sample(same.similarity), ]
            similar.consumer <- similar.consumer[1:Kc, ]
        } else {
            similar.consumer <- similar.consumer[1:Kc, ]
        }# if for random draw


        # Est-ce que la valeur de similarité a de l'importance pour l'attribution des proies?
        # If yes, we could add an argument call wt_predator.
          # if(wt_predator == FALSE) {
          #   resources <- unique of all prey species of all similar predators
          # } else {}

        for(j in 1:Kc) { #loop through consumers

            if(all.equal(similar.consumer[, 'similarity'], rep('0',Kc)) == TRUE) { # if similarities all == 0, break
                break
            } else if(similar.consumer[j, 'similarity'] == '0') { # if similarity l == 0, no candidates provided
                NULL
            } else {

                # It's possible that consumers in the list have high taxonomic similarity, but no recorded resource
                candidate.resource <- unlist(strsplit(S0[similar.consumer[j, 'consumer'], 'resource'], " \\|\\ ")) # list of resources for consumer j
                # candidate.resource <- candidate.resource[(candidate.resource %in% resources.S1) == FALSE] # substracting candidate resources that are already listed as resources for S1[i] and hence considered in the preceding code segment

                for(k in 1:length(candidate.resource)) { # loop through resources of consumer j
                    if(length(candidate.resource) == 0) { # if candidate resource list is empty, break
                        break
                    } else if(candidate.resource[1] == "") { # if candidate list is an empty vector "", break
                        break
                    } else if(candidate.resource[k] == S1[i]) {
                    #   #// FIXME: if candidate resource is taxon for which predictions are being made, break (unless we want to allow CANIBALISM). Add argument for cannibalism allowed or not
                         NULL
                    } else if((candidate.resource[k] %in% S1) == TRUE) {
                        if((candidate.resource[k] %in% candidates[, 'resource']) == TRUE) {# if candidate is already in candidate list, add 1 to its weight
                            candidates[which(candidates[, 'resource'] == candidate.resource[k]), 'weight'] <- as.numeric(candidates[which(candidates[, 'resource'] == candidate.resource[k]), 'weight']) + 1
                        } else {
                            candidates <- rbind(candidates, c(candidate.resource[k], 1)) # if candidate is not in the list, add it with 1 to its weight
                        }#if2

                    } else {
                        similarity.resources <- similarity_full_algorithm(S0 = unique(S0[which(S0[, 'taxon'] %in% S1 | S0[, 'taxon'] == candidate.resource[k]), ]), # S1 in S0 + resource for which similarity has to be measured
                                                                        S1 = candidate.resource[k], # resource for which similarity has to be measured
                                                                        wt = wt,
                                                                        taxa = 'resource')

                        similar.resource <- matrix(nrow = nrow(similarity.resources), ncol = 2, dimnames = list(c(), c('resource','similarity')), data = NA) # importing K nearest neighbors resources
                        similar.resource[, 'resource'] <- names(similarity.resources[order(similarity.resources, decreasing = TRUE), ])
                        similar.resource[, 'similarity'] <- similarity.resources[order(similarity.resources, decreasing = TRUE)]
                        to.remove <- which(similar.resource[,'resource'] == candidate.resource[k])
                        if(length(to.remove) > 0){ #remove resoures.S1[j] in case it gets through (just keeping it consistant with other similarity evaluation further down in the catalogue, even though it is not necessary in this portion)
                            similar.resource <- similar.resource[-which(similar.resource[,'resource'] == candidate.resource[k]), ]
                        }

                        # If multiple taxa with same similarity, randomly select those that will be used as similar resources.
                        if(similar.resource[Kr+1, 'similarity'] == similar.resource[Kr, 'similarity']) {
                            same.similarity <- which(similar.resource[, 'similarity'] == similar.resource[Kr, 'similarity'])
                            similar.resource[same.similarity, ] <- similar.resource[sample(same.similarity), ]
                            similar.resource <- similar.resource[1:Kr, ]
                        } else {
                            similar.resource <- similar.resource[1:Kr, ]
                        }# if for random draw

                        for(l in 1:Kr) { # extracting resource candidates
                            if(all.equal(similar.resource[, 'similarity'], rep('0',Kr)) == TRUE) { # if similarities all == 0, break
                                break
                            } else if(similar.resource[l, 'similarity'] == '0') { # if similarity l == 0, no candidates provided
                                NULL
                                # minimum threshold try.. adding it as a Parameters.. might not make sense, have to discuss it. If we keep it, previous else ifs can be removed
                            } else if(similar.resource[l, 'similarity'] < minimum_threshold) {
                                NULL
                            } else if((similar.resource[l, 'resource'] %in% candidates[, 'resource']) == TRUE) { # if candidate is already in candidate list, add 1 to its weight
                              candidates[which(candidates[, 'resource'] == similar.resource[l]), 'weight'] <- as.numeric(candidates[which(candidates[, 'resource'] == similar.resource[l]), 'weight']) + as.numeric(similar.resource[l, 'similarity'])
                            } else {
                                  candidates <- rbind(candidates, similar.resource[l, ]) # if candidate is not in the list, add it with its weight = similarity
                            }#if3
                        }#l
                    } #if1
                }#k
            }#if
        }#j

        candidates <- candidates[which(candidates[, 'weight'] >= MW), ] # remove candidates with a weight below MW
        if(is.matrix(candidates) == TRUE) { #if it's a vector, there's only one predicted resource, no need to order
            candidates[order(candidates[, 'weight']), ] # sorts candidates according to their weight
            predictions[S1[i], 'resource_predictions'] <- paste(candidates[, 'resource'], collapse = ' | ')
        } else {
          predictions[S1[i], 'resource_predictions'] <- paste(candidates['resource'], collapse = ' | ')
        }#if
    setTxtProgressBar(pb, i)
    }#i
    close(pb)
    return(predictions)
}#full algorithm function
