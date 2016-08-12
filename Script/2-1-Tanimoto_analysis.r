# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#   2. Tanimoto analysis for multiple parameter values
# -----------------------------------------------------------------------------

# Evaluating the effects of multiple parameters on the efficiency of the algorithm

# -----------------------------------------------------------------------------
# FILES:
#   RData <- file = "RData/Tanimoto_analysis.RData" XXX adjust name with type of analysis
#   Script  <- file = "Script/2-1-Tanomoto_analysis.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# PARAMETERS:
    filename <- 'Multiple_parameters'
    min.tx = 45
    K.values = c(2,4,6,8)
    MW = c(1,3,5)
    WT = c(0,0.3,0.6,1)
    blind = FALSE
    minimum_threshold = 0.3
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load("./RData/Tanimoto_data.RData")
load("./RData/interactions_source.RData")
suppressMessages(load("./RData/Similarity.matrices.RData")) # For similarity matrices already evaluated


# S0: A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource']
# Format interaction catalogue to fit this table format
    S0_catalog <- matrix(nrow = nrow(Tanimoto_data[[1]]), ncol = 4, data = "", dimnames = list(Tanimoto_data[[1]][, 'taxon'], c('taxon', 'taxonomy', 'resource', 'non-resource')))
    S0_catalog[, 1] <- Tanimoto_data[[1]][, 'taxon']
    S0_catalog[, 2] <- Tanimoto_data[[1]][, 'kingdom | phylum | class | order | family | genus | species']
    # From binary interactions catalogue with consumer, resources, interaction or non-interaction
    for(k in 1:nrow(Tanimoto_data[[3]])) {
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 3] <- Tanimoto_data[[3]][k, 'resource']
        S0_catalog[Tanimoto_data[[3]][k, 'consumer'], 4] <- Tanimoto_data[[3]][k, 'non-resource']
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

    to.delete <- c(which(Cm.lg < min.tx), which(Cm.lg > 1000)) # > 1000 is for GloBI

    Cm <- Cm[-to.delete]
    for(i in rev(to.delete)) {
        communities[[i]] <- NULL
    }
    names(communities) <- Cm

# Setting up lists to store the results
    wt.init <- seq(0,1,by=0.1)
    wt.remove <- which(!wt.init %in% WT)# selecting a subset of similarity matrices
    for(i in rev(wt.remove)) {
        similarity.matrices[[i]] <- NULL
    }

    sim.wt <- names(similarity.matrices)
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

min.wt <- vector("list", length(MW))
for(i in 1:length(MW)) {
    min.wt[[i]] <- Tanimoto_analysis
}
Tanimoto_analysis <- min.wt
names(Tanimoto_analysis) <- MW
remove(min.wt)

file.to.save <- serialNext("./Analyses/Tanimoto_temp/Tanimoto_analysis.RData")
save(x = Tanimoto_analysis, file = file.to.save)

init.time <- Sys.time()
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
                pb <- txtProgressBar(min = 0,max = length(Cm), style = 3)

                for(j in 1:length(Cm)) { #2nd loop for all C[i]
                    S1 <- communities[[j]]
                    S0 <- S0_catalog
                    similarity.matrix <- similarity.matrices[[i]]

                    # Two choices here:
                    #   1. The analysis is blind, which means we remove all the information available in the catalogue for all species in S1
                    #   2. The analysis takes into account preexisting information already contained in the catalogue

                    # 1. Blind analysis, removing all information on taxa in S1 from S0 (rownames need to == taxa name)
                    if(blind == TRUE) {
                        for(k in 1:length(S1)) {
                          S0[S1[k], 'resource'] <- ""
                          S0[S1[k], 'non-resource'] <- ""
                        }

                    # 2. Preexisting information kept to inform algorithm
                    } else { # blind == FALSE

                        interactions <- interactions_sources[-which(interactions_sources[, 'source'] == Cm[j]), 1:3]

                        # Only modifying those that are loosing data from the catalogue, less time
                            to.change <- numeric()
                            for(k in 1:length(S1)) {
                                to.change <- c(to.change, which(interactions[, 'consumer'] == S1[k]), which(interactions[, 'resource'] == S1[k]))
                            }
                            to.change <- unique(to.change)

                        # Modifying sets of resources and non-resources for taxa in S1
                            interactions <- interactions[to.change, ]
                            rownames(interactions) <- seq(1,nrow(interactions))
                            resource_set <- consumer_set_of_resource(consumer = interactions[, 'consumer'],
                                                                      resource = interactions[, 'resource'],
                                                                      inter_type = interactions[, 'inter'])

                        # From binary interactions catalogue with consumer, resources, interaction or non-interaction (rownames need to == taxa name)
                            for(k in 1:nrow(resource_set)) {
                              S0[resource_set[k, 'consumer'], 3] <- resource_set[k, 'resource']
                              S0[resource_set[k, 'consumer'], 4] <- resource_set[k, 'non-resource']
                            }
                        remove(interactions, resource_set, to.change)
                    } #if blind or not blind

                    # Recalculate similarity
                        similarity.matrix <- similarity_taxon_predict(S0 = S0,
                                                                            S1 = S1,
                                                                            wt = wt,
                                                                            similarity.matrix = similarity.matrix)

                    # Predicting interactions
                        Tanimoto_analysis[[n]][[m]][[i]][[j]] <- two_way_tanimoto_predict(Kc = Kc,
                                                                                Kr = Kr,
                                                                                S0 = S0,
                                                                                S1 = S1,
                                                                                MW = mw,
                                                                                similarity.matrix = similarity.matrix,
                                                                                minimum_threshold = minimum_threshold)

                    save(x = Tanimoto_analysis, file = file.to.save)
                    remove(S0, S1, similarity.matrix)
                    setTxtProgressBar(pb, j)
                }#2nd loop for all C[i]

                save(x = Tanimoto_analysis, file = file.to.save)
                remove(wt)

            }#1st loop for all types of wt values
            close(pb)
    }#m
}#n
print(Sys.time() - init.time)

# Catalog vs predictions
accuracy  <- vector('list', 3)
names(accuracy) <- c('Catalog', 'Predict', 'Algorithm')
accuracy[[1]] <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis, empirical.only = TRUE)
accuracy[[2]] <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis, predict.only = TRUE)
accuracy[[3]] <- tanimoto_accuracy(Tanimoto_analysis = Tanimoto_analysis)

#Figure
pdf("./Article/Catalog_vs_predictions.pdf",width=7,height=7)
# Plots
par(mfrow=c(2,2))
# Graph
for(j in 9:12) {
        eplot(xmin = -0.09, xmax = 1.09)
        par(pch = 21,  xaxs = "i", yaxs = "i", family = "serif")
        foodwebs <- names(Tanimoto_analysis[[1]][[1]][[1]])
        names <- c('TSS','Score y', 'Score -y', 'Accuracy score')
        col <- c("#FF8822","#449955","#2288FF")
        # col <- c("#FF000088","#00FF0088","#0000FF88")
        # col <- gray.colors(11, start = 0, end = 0.8, gamma = 2.2, alpha = NULL) # grey scale ramp
        # col <- sample(colours(), length(foodwebs))

        # Axes
            # rect(0, 0, 1, 1, col = "#eeeeee", border = NA)
            axis(side = 1, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02)
            axis(side = 2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02)
            axis(side = 3, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 1.02)
            axis(side = 4, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 1.02)
            # abline(v = seq(0,6,by = 2), col = "white", lty = 2)
            # abline(h = seq(1,2,by = 1), col = "white", lty = 2)

            mtext(text = names[j-8], side = 2, line = 2, at = 0.5, font = 2, cex = 1)
            mtext(text = "Similarity weight", side = 1, line = 2, at = 0.5, font = 2, cex = 1)

        for(i in 1:length(accuracy)) {
            accuracy_mean <- aggregate(as.numeric(accuracy[[i]][,j]) ~ as.numeric(accuracy[[i]][, 'wt']), data=accuracy[[i]], FUN=function(x) c(mean=mean(x), sd=sd(x)))
            # hack: we draw arrows but with very special "arrowheads" for error bars
            arrows(accuracy_mean[, 1], accuracy_mean[, 2][,1] - accuracy_mean[, 2][, 2], accuracy_mean[, 1], accuracy_mean[, 2][, 1] + accuracy_mean[, 2][, 2], length=0.05, angle=90, code=3, col = col[i])
            points(x = accuracy_mean[, 1], y = accuracy_mean[, 2][, 1], cex = 1.5, pch = 22, col = col[i])
        } #i

        ## Add legend
        if(j == 12) {
            legend(0.45, 0.3, lwd = 3, col = col, legend = names(accuracy), bty = 'n', y.intersp = 1)
        }
} #j
dev.off()

save(x = Tanimoto_analysis, file = paste('./Analyses/',filename,'.RData',sep=''))
