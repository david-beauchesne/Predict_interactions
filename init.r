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
# PSEUDOCODE:
# Parameters:
#   Kc                    Integer, how many consumer neighbors to select
#   Kr                    Integer, how many resource neighbors to select
#   S0                    Interction catalogue w/ 'taxa', 'taxonomy', 'resource set', 'non-resource set', 'consumer set', 'non-consumer set'
#   S1                    Set of taxa for which we wish to predict pairwise interactions
#   MW                    Mimimum weight to accept a candidate as a prey
#   Minimum_threshold     Minimum similarity threshold used to accept candidate species. Arbitrary at this point.
#
# Output
#   A matrix 'predictions' with columns
#     1. S1 taxa
#     2. empirical resource of S1 taxa
#     3. predicted resources of S1 taxa
#
# ------------
#
# predictions <- empty vector
#
#
# for consumers in S1
#     candidate_list <- empty vector
#
#     # 1. Empirical information in catalogue
#     resources_S1 = set of resources found in S0
#     empirical_resource <- empty vector
#
#     if length(resources_S1) > 0:
#         for resources in resources_S1
#             if resources in S1:
#                 add resources to empirical_resource
#             else:
#                 similar_resource <- pick K most similar resources in S1 based on taxonomy and set of consumers
#                     if similarity K + 1 = similarity K:
#                         random sample of similar resources with similarity K
#
#                 for resources' in similar_resource
#                     if all K similarity = 0:
#                         break out of loop
#                     else if resources' similarity = 0:
#                         NULL
#                     else if resources' similarity < minimum similarity threshold:
#                         NULL
#                     else if resources' in candidate_list:
#                         add weight = similarity between resources and resources' to resources' in candidate_list
#                     else: (not in candidate_list)
#                         add resources' to candidate_list w/ weight = similarity between resources and resources'
#
#         add empirical_resource to predictions matrix
#
#     # 2. Similar consumers information
#     similar_consumers <- pick K most similar consumers in S0 based on taxonomy and set of resources
#         if similarity K + 1 = similarity K:
#             random sample of similar consumers with similarity K
#
#     for consumers' in similar_consumers
#         if all K similarity = 0:
#             break out of loop
#         else if consumers' similarity = 0:
#             NULL
#
#         candidate_resources = resources' of consumers' in S0
#
#         for resources' in candidate_resources
#             if length(candidate_resources) == 0:
#                 break out of loop
#             else if candidate_resources == "":
#                 break out of loop
#             else if resources' = consumers: (does not allow for cannibalism. Should verify this at some point and allow for it, there are multiple instances of cannibalism in food webs)
#                 break out of loop
#             else if resources' in S1:
#                 if resources' in candidate_list:
#                     add weight = 1 to resources' in candidate_list
#                 else:
#                     add resources' w/ weight = 1 to candidate_list
#
#             else: (resources' not in S1)
#                 similar_resource <- pick K most similar resources in S1 based on taxonomy and set of consumers
#                     if similarity K + 1 = similarity K:
#                         random sample of similar resources with similarity K
#
#                 for resources' in similar_resource
#                     if all K similarity = 0:
#                         break out of loop
#                     else if resources' similarity = 0:
#                         NULL
#                     else if resources' similarity < minimum similarity threshold:
#                         NULL
#                     else if resources' in candidate_list:
#                         add weight = similarity between resources' and candidate_resources to resources' in candidate_list
#                     else: (not in candidate_list)
#                         add resources' to candidate_list w/ weight = similarity between resources' and candidate_resources
#
#     candidate_list <- choose candidate resources with weight >= MW
#     predictions <- add candidate_list to predictions
#
# return(predictions matrix)
