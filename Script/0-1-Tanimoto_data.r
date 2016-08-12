# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    0.1   Formatting interaction catalog
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
library(stringr)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load("../Interaction_catalog/RData/class_tx_tot.RData")
load("../Interaction_catalog/RData/GloBI_classification.RData")
load("../Interaction_catalog/RData/interactions.RData")
load("../Interaction_catalog/RData/GloBI_interactions.RData")
load("../Interaction_catalog/RData/sp_egsl.RData")
# ---------------------------------
# Unique binary interactions
# ---------------------------------

# Select binary interactions with species that have a fully resolved taxonomy
Biotic_inter <- vector('list',4)
names(Biotic_inter) <- c('Binary_interaction','Taxon_list','Inter_taxonomy','EGSL')

  # For Empirical Webs
    consumer <- numeric()
    resource <- numeric()
    pb <- txtProgressBar(min = 0,max = nrow(inter.tot), style = 3)
    for(i in 1:nrow(inter.tot)) {
      if(!is.na(class.tx.tot[inter.tot[i, 'Predator'], 1])) {
        consumer <- c(consumer, 1)
      } else {
        consumer <- c(consumer, 0)
      } #if

      if(!is.na(class.tx.tot[inter.tot[i, 'Prey'], 1])) {
        resource <- c(resource, 1)
      } else {
        resource <- c(resource, 0)
      } #if
      setTxtProgressBar(pb, i)
    } #i
    close(pb)

    # If all = 1, no need to adjust
    unique(consumer)
    unique(resource)

  # For GloBI interactions
    consumer <- numeric()
    resource <- numeric()
    pb <- txtProgressBar(min = 0,max = nrow(GloBI_interactions), style = 3)
    for(i in 1:nrow(GloBI_interactions)) {
      if(!is.na(GloBI_classification[GloBI_interactions[i, 'Predator'], 1])) {
        consumer <- c(consumer, 1)
      } else {
        consumer <- c(consumer, 0)
      } #if

      if(!is.na(GloBI_classification[GloBI_interactions[i, 'Prey'], 1])) {
        resource <- c(resource, 1)
      } else {
        resource <- c(resource, 0)
      } #if
      setTxtProgressBar(pb, i)
    } #i
    close(pb)

    # If all = 1, no need to adjust
    unique(consumer)
    unique(resource)

    cons_res <- cbind(resource,consumer)
    cons_res <- rowSums(cons_res)

    GloBI_interactions <- GloBI_interactions[-which(cons_res != 2), ]

    # Combining biotic interactions in complete dataset
    Biotic_inter[[1]] <- rbind(inter.tot[, 1:3], GloBI_interactions[, 1:3])
    colnames(Biotic_inter[[1]]) <- c('consumer','inter','resource')
    Biotic_inter[[1]] <- unique(Biotic_inter[[1]])

# ---------------------------------
# Unique taxon list
# ---------------------------------
  Biotic_inter[[2]] <- rbind(class.tx.tot, GloBI_classification)
  Biotic_inter[[2]] <- unique(Biotic_inter[[2]])
  rownames(Biotic_inter[[2]]) <- Biotic_inter[[2]][, 'taxon']

  # Adjust taxon names with taxonomy
  for(i in 1:nrow(Biotic_inter[[2]])) {
    Biotic_inter[[2]][i, 'taxon'] <- paste(Biotic_inter[[2]][i,paste(Biotic_inter[[2]][i, 'rank'])])
  }#i

  # Adjust taxon rank not kept
  Biotic_inter[[2]][which(Biotic_inter[[2]][, 'rank'] == 'tribe'), 'taxon'] <- paste(Biotic_inter[[2]][which(Biotic_inter[[2]][, 'rank'] == 'tribe'), 'family'])
  Biotic_inter[[2]][which(Biotic_inter[[2]][, 'rank'] == 'superfamily'), 'taxon'] <- paste(Biotic_inter[[2]][which(Biotic_inter[[2]][, 'rank'] == 'superfamily'), 'order'])
  Biotic_inter[[2]][which(Biotic_inter[[2]][, 'rank'] == 'subfamily'), 'taxon'] <- paste(Biotic_inter[[2]][which(Biotic_inter[[2]][, 'rank'] == 'subfamily'), 'family'])

  # First letter only as capital
  Names_change <- function(x){
    # Name resolve #1
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    return(x)
  }

  Biotic_inter[[2]][, -13] <- apply(Biotic_inter[[2]][, -13], 2, Names_change)
  Biotic_inter[[1]][, -2] <- apply(Biotic_inter[[1]][, -2], 2, Names_change)
  Biotic_inter[[2]] <- apply(Biotic_inter[[2]], 2, gsub, pattern = 'NANA', replacement = 'NA')

  # Also adjust in interactions list
  for(i in 1:nrow(Biotic_inter[[1]])) {
    Biotic_inter[[1]][i, 'consumer'] <- Biotic_inter[[2]][paste(Biotic_inter[[1]][i, 'consumer']), 'taxon']
    Biotic_inter[[1]][i, 'resource'] <- Biotic_inter[[2]][paste(Biotic_inter[[1]][i, 'resource']), 'taxon']
  }

# Also adjust egsl species list
 Biotic_inter[[4]] <- matrix(nrow = nrow(sp.egsl), ncol = ncol(Biotic_inter[[2]]), data = NA, dimnames = list(c(), colnames(Biotic_inter[[2]])))
 rownames(Biotic_inter[[4]]) <- sp.egsl[,1]
 for(i in 1:nrow(sp.egsl)) {
    Biotic_inter[[4]][i, ] <- Biotic_inter[[2]][sp.egsl[i,1], ]
 }

# Also adjust for Empirical Webs interactions
for(i in 1:nrow(inter.tot)) {
  inter.tot[i, 'Predator'] <- Biotic_inter[[2]][paste(inter.tot[i, 'Predator']), 'taxon']
  inter.tot[i, 'Prey'] <- Biotic_inter[[2]][paste(inter.tot[i, 'Prey']), 'taxon']
}

# Also adjust GloBI_interactions
for(i in 1:nrow(GloBI_interactions)) {
  GloBI_interactions[i, 'Predator'] <- Biotic_inter[[2]][paste(GloBI_interactions[i, 'Predator']), 'taxon']
  GloBI_interactions[i, 'Prey'] <- Biotic_inter[[2]][paste(GloBI_interactions[i, 'Prey']), 'taxon']
}

Biotic_inter[[1]] <- Biotic_inter[[1]][-which(Biotic_inter[[1]][,'resource'] == "Copepod"), ]


  # Unique values and adjusting rownames
  Biotic_inter[[1]] <- unique(Biotic_inter[[1]])
  Biotic_inter[[2]] <- unique(Biotic_inter[[2]])
  rownames(Biotic_inter[[2]]) <- Biotic_inter[[2]][, 'taxon']
  rownames(Biotic_inter[[4]]) <- Biotic_inter[[4]][, 'taxon']
  Biotic_inter[[4]] <- Biotic_inter[[4]][,-c(7,9,10,13)]
  GloBI_interactions <- unique(GloBI_interactions[, 1:3])
  inter.tot <- unique(inter.tot[, 1:3])

# --------------------------------------------
# First dataset: taxon with taxonomy as vector
# --------------------------------------------

colnames(GloBI_interactions) <- c('consumer','inter','resource')
taxon.list <- unique(c(unique(Biotic_inter[[1]][, 'consumer']), unique(Biotic_inter[[1]][, 'resource'])))

taxon <- matrix(nrow = length(taxon.list), ncol = 2, data = NA, dimnames = list(c(), c("taxon", "kingdom | phylum | class | order | family | genus | species")))
taxon[, 1] <- taxon.list

# Extracting taxonomy for each binary interaction
rank <- c("kingdom","phylum","class","order","family","genus","species")
taxonomy <- matrix(nrow = nrow(taxon), ncol = length(rank), dimnames = list(c(), rank))

pb <- txtProgressBar(min = 0,max = nrow(taxon), style = 3)
for(i in 1:nrow(taxon)) {
  taxonomy[i, ] <- Biotic_inter[[2]][taxon[i, 'taxon'], rank]
  setTxtProgressBar(pb, i)
} #i
close(pb)

# Combining taxonomy into single element
# Ranks are  "kingdom | phylum | class | order | family | genus | species"
taxon[, 2] <- apply(taxonomy, 1, paste, collapse = ' | ')

# write.table(taxon, "Phil_data/0-taxon_list.txt", sep="\t")

# ---------------------------------------------------------------------------
# Second dataset: Predators with sets of prey and non-prey for Empirical Webs
# ---------------------------------------------------------------------------
rownames(inter.tot) <- seq(1,nrow(inter.tot))
emp.web.inter <- consumer_set_of_resource(consumer = inter.tot[, 'Predator'],
                                          resource = inter.tot[, 'Prey'],
                                          inter_type = inter.tot[, 'FeedInter']
                                          )

# write.table(emp.web.inter, "Phil_data/1-emp_webs_interactions.txt", sep="\t")

# ----------------------------------------------------------------------------------
# Third dataset: Predators with sets of prey and non-prey for Empirical Webs + GloBI
# ----------------------------------------------------------------------------------
rownames(Biotic_inter[[1]]) <- seq(1,nrow(Biotic_inter[[1]]))
total.inter <- consumer_set_of_resource(consumer = Biotic_inter[[1]][, 'consumer'],
                                        resource = Biotic_inter[[1]][, 'resource'],
                                        inter_type = Biotic_inter[[1]][, 'inter']
                                        )

# write.table(total.inter, "Phil_data/2-total_interactions.txt", sep="\t")

# ----------------------------
# Fourth dataset: EGSL species
# ----------------------------
egsl <- matrix(nrow = nrow(Biotic_inter[[4]]), ncol = 2, data = NA, dimnames = list(c(Biotic_inter[[4]][, 'taxon']), c("taxon", "kingdom | phylum | class | order | family | genus | species")))
egsl[, 'taxon'] <- Biotic_inter[[4]][, 'taxon']
egsl[, 2] <- apply(Biotic_inter[[4]][,3:9], 1, paste, collapse = ' | ')
egsl <- unique(egsl)

# remove duplicated taxon
which(duplicated(egsl[,'taxon']))
egsl <- egsl[-1209, ]

# write.table(egsl, "Phil_data/3-EGSL_species.txt", sep="\t", row.names = FALSE, col.names = c('EGSL_species'))

# ----------------------------
# RData
# ----------------------------
Tanimoto_data <- vector("list", 4)
Tanimoto_data[[1]] <- taxon
Tanimoto_data[[2]] <- emp.web.inter
Tanimoto_data[[3]] <- total.inter
Tanimoto_data[[4]] <- egsl

save(x = Tanimoto_data, file = "./RData/Tanimoto_data.RData")
