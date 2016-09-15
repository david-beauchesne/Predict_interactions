# Run init.r before other scripts
rm(list=ls())
 # for use in R console.
 # set own relevant directory if working in R console, otherwise ignore if in terminal
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/Predict_interactions")
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# REPOSITORY
#   Machine learning algorithm to predict biotic interactions. This repository
#   contains the scripts and the analyses to test the accuracy of the
#   algorithm.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# PROCESS STEPS:
#   0. Setting up dataset with proper format for analysis
#
#       0.1 Data set from RData in interactions_catalog repository
#           Script <- file = "./Script/0-1-Tanimoto_data.r
#           RData <- file = './RData/Tanimoto_data.RData'
#
#       0.2 Extracting sources for each binary interaction forming the catalogue
#           Script <- file = "./Script/0-1-Interactions_sources.r
#           RData <- file = "./RData/interactions_source.RData")
#
#   1. Calculating similarity matrices for resources and consumers
#       Script <- file = './Script/1-Similarity_matrix.r'
#       RData <- file = './RData/similarity_matrices.RData'
#
#   2. Tanimoto analysis for XXX
#
#       2.1 Tanimoto predictions for set of X parameters
#           Script <- file = './Script/2-1-Tanimoto_analysis.r'
#           RData <- file = './RData/Tanimoto_analysis.RData'
#
#       2.2 Evaluation of analysis accuracy + tables and figures
#           Script <- file = './Script/2-2-Tanimoto_accuracy.r'
#           RData <- file = './RData/Tanimoto_accuracy.RData'
#           Figures <- file = ''
#           Tables <- file = ''
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FUNCTIONS (add a description of the functions eventually)
source("./Script/tanimoto.r") # basic tanimoto similarity
source("./Script/tanimoto_traits.r") # extended tanimoto included trait/taxonomy vector
source("./Script/similarity_taxon.r") # similarity matrix for set of taxa
source("./Script/similarity_taxon_predict.r") #similarity of additional taxa in S1 not found in S0
source("./Script/two_way_tanimoto_predict.r") # interaction predictions from two-way Tanimoto algorithm
source("./Script/prediction_matrix.r") # predictions formatted to food web matrix format (S x S)
source("./Script/empirical_matrix.r") # predictions formatted to food web matrix format (S x S)
source("./Script/consumer_set_of_resource.r")
source("./Script/resource_set_of_consumer.r")
source("./Script/prediction_accuracy.r") #
source("./Script/prediction_accuracy_id.r") #
source("./Script/tanimoto_accuracy.r") # calculating the accuracy of predictions from Tanimoto_predictions
source("./Script/serialNext.r") # function to avoid overwriting existing files in temporary analyses folder
source("./Script/eplot.r") # empty plot for figure generation
source("./Script/tanimoto_analysis.r")
source("./Script/catalog_predictions.r") # computing prediction accuracy ~ # taxa in catalog
source("./Script/catalog_predictions_accuracy.r") # accuracy of predictions for accuracy ~ # taxa in catalog
source("./Script/full_algorithm.r") # full algorithm with similarity measurements included
source("./Script/similarity_full_algorithm.r") # similarity measurements for full algorithm
source("./Script/duplicate_row_col.r") # function to combine duplicated row and column names
source("./Script/bin_inter.r") # function to extract binary interaction from diet matrix


# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   In this version of the algorithm, we use similarity matrices rather than graphs, which greatly slows down the analysis speed.
#   We therefore divide the algorightm between :
#     Similarity evaluation (functions: similarity_taxon & similarity_taxon_to_predict, 'wt' argument has to be the same for both functions)
#     Interaction predictions (function: two_way_tanimoto_predict)

# Process steps for analyses:
#   1. Similarity between taxa combinations
#     1.1 Evaluate the similarity matrix of S0 (i.e. all species in catalogue) for a number of wt values seq(0, 1, by = 0.1)
#     1.2 Define S1, set of species forming a community C[i] and for which we wish to predict interactions
#     1.3 Remove all species in S1 from similarity matrix alreay measured and interactions stemming from C[i]
#     1.4 Extend similarity matrix to include S1 taxa (Evaluate similarity for all additionnal combinations added to the matrix)
#
#   For each species in S1:
#   2. Identify resources already known in interaction catalogue (S0) for S1 species
#     2.1 If resoures are in S1, automatically add them to the predictions as empirically valid interactions
#     2.2 If resources are not in S1, find Kr similar resources in S1 and add them to candidate list with weight equal to their similarity

#   3. Identify Kc similar consumers to S1 in S0
#     3.1 Extract set of candidate resources from each similar consumer, if any
#     3.2 If candidate resource is in S1, add it to candidate list with weight 1
#     3.3 If candidate resource not in S1, find Kr similar resources in S1 and add them to candidate list with weight equal to their similarity

#   4. Make predictions:
#     4.1 Remove taxa with weight < to minimum weight (MW) from prediction list
#     4.2 Sort prediction list according to weight. Higher weights mean higher likelihood for resource being consumed

#   Subset of communities based on the number of taxa available? Most of them end up having very few taxa represented in here. Less than I expected...
# -----------------------------------------------------------------------------
