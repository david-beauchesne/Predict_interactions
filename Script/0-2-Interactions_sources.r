# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    0.2   Extracting which taxa is found in which community from empirical data
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

#Loading all datasets with interactions
load("../Interaction_catalog/RData/barnes2008.RData")
load("../Interaction_catalog/RData/Kortsch2015.RData")
load("../Interaction_catalog/RData/GlobalWeb.RData")
load("../Interaction_catalog/RData/brose2005.RData")
load("../Interaction_catalog/RData/class_tx_tot.RData")
load("../Interaction_catalog/RData/GloBI_classification.RData")
load("../Interaction_catalog/RData/GloBI_interactions.RData")

# load("RData/EwE.RData") # Not yet finished
# load("RData/GloBI.RData") # Not yet finished

# List of webs
InterDataTot <- c(Barnes2008, Brose2005, GlobalWeb, Kortsch2015) # EwE to add

# Complete taxon list and interaction list from all webs
tx.list <- matrix(nrow=0, ncol=2, data=NA, dimnames = list(c(), c("taxon","rank")))
inter.list <- matrix(nrow=0, ncol=4, data=NA, dimnames = list(c(), c("Predator","FeedInter","Prey","Source")))


for(i in 1:length(InterDataTot)) {
  tx.list <- rbind(tx.list, as.matrix(InterDataTot[[i]][[5]]))
  inter.list <- rbind(inter.list, cbind(as.matrix(InterDataTot[[i]][[4]]), rep(names(InterDataTot[i]), nrow(InterDataTot[[i]][[4]]))))
}

tx.list <- unique(tx.list)
inter.list <- unique(inter.list)

# Taxonomic resolutions and those selected to move further in the analysis
# Decision is to use all taxonomic resolutions greater or equal to families
taxo.resol <- unique(inter.list[, 2])
taxo.resolution.accepted <- c("species", "genus", "family", "tribe", "subfamily", "superfamily")

# Extracting interactions for analysis
time_init <- Sys.time()
inter.tot <- inter_taxo_resolution(tx.list, inter.list, taxo.resolution.accepted)
Sys.time() - time_init

# Changing to binary interactions
for(i in 1:nrow(inter.tot)) {
  if(inter.tot[i, 2] != "0" & inter.tot[i, 2] != "1") {inter.tot[i, 2] <- "1"}
}

# Extracting taxon list for analysis
row.tx.accepted <- numeric()
for(i in 1:length(taxo.resolution.accepted)) {
  row.tx.accepted <- c(row.tx.accepted,which(tx.list[,2] == taxo.resolution.accepted[i]))
}

tx.list.tot <- tx.list[row.tx.accepted, ]

#inter.tot
#tx.list.tot

# ---------------- INTERACTIONS ---------------
GloBI_interactions <- cbind(GloBI_interactions[, c('Predator','FeedInter','Prey')], rep('GloBI', nrow(GloBI_interactions)), GloBI_interactions[, 'inter.resolution'])

Biotic_inter <- vector('list',4)
names(Biotic_inter) <- c('Binary_interaction','Taxon_list','Inter_taxonomy','EGSL')

# Combining biotic interactions in complete dataset
Biotic_inter[[1]] <- rbind(inter.tot[, 1:4], GloBI_interactions[, 1:4])
colnames(Biotic_inter[[1]]) <- c('consumer','inter','resource','source')
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
Biotic_inter[[1]]<- Biotic_inter[[1]][-which(Biotic_inter[[1]][, 'resource'] == 'Unidentified'), ] # Removing unidentified

no.result.to.delete <- c( "Baraeoptera roria",
                          "Cydorus latus",
                          "Hemiuris communis",
                          "Hyponigrus obsidianus",
                          "Sarortherdon macrochir",
                          "Scaphaloberis mucronata",
                          "Secernentia nematodes",
                          "Zealolessica cheira",
                          "Haploparaksis crassirostris",
                          'Spermophilus armatus',
                          "Spermophilus brunneus",
                          "Spermophilus franklinii",
                          "Spermophilus richardsonii",
                          "Spermophilus tridecemlineatus",
                          "Spermophilus washingtoni",
                          'Glossoma',
                          "Paracentropristes pomospilus",
                          "Delphacinae",
                          "Staphylininae",
                          "Ursinae",
                          "Zelandoperlinae",
                          "Euclymeninae",
                          "Pilumninae")

to.delete <- numeric() # À utiliser à la fin après avoir combiner les jeux de données
for(i in 1:length(no.result.to.delete)){
to.delete <- c(to.delete,which(Biotic_inter[[1]][, 'resource'] == no.result.to.delete[i]), which(Biotic_inter[[1]][, 'consumer'] == no.result.to.delete[i]))
}
to.delete <- unique(to.delete)
Biotic_inter[[1]] <- Biotic_inter[[1]][-to.delete, ]

# Also adjust in interactions list
for(i in 1:nrow(Biotic_inter[[1]])) {
Biotic_inter[[1]][i, 'consumer'] <- Biotic_inter[[2]][paste(Biotic_inter[[1]][i, 'consumer']), 'taxon']
Biotic_inter[[1]][i, 'resource'] <- Biotic_inter[[2]][paste(Biotic_inter[[1]][i, 'resource']), 'taxon']
}

# Unique values and adjusting rownames
Biotic_inter[[1]] <- unique(Biotic_inter[[1]])
Biotic_inter[[2]] <- unique(Biotic_inter[[2]])
rownames(Biotic_inter[[2]]) <- Biotic_inter[[2]][, 'taxon']

# Les informations de GloBI peuvent être une duplication des informations déjà relevées des food webs empiriques. Il faudrait songer à les retirer.
for(i in 1:nrow(Biotic_inter[[1]])) {
    if(Biotic_inter[[1]][i, 'source'] == "Globi") {
        Biotic_inter[[1]][i, 'source'] <- ""
    }
}
interactions_sources <- Biotic_inter[[1]]
interactions_sources <- interactions_sources[-which(interactions_sources[,'resource'] == "Copepod"), ]
rownames(interactions_sources) <- seq(1,nrow(interactions_sources))
save(x = interactions_sources, file = "./RData/interactions_source.RData")
#
# # Interactions unique avec sources
# multi_inter <- which(duplicated(Biotic_inter[[1]][,1:3]))
# # to.remove <- numeric() # If I wish to remove duplicated interactions at this stage
# for(i in 1:length(multi_inter)) {
#     duplicata <- which(Biotic_inter[[1]][, 1] == Biotic_inter[[1]][multi_inter[i], 1] & Biotic_inter[[1]][, 2] == Biotic_inter[[1]][multi_inter[i], 2] & Biotic_inter[[1]][, 3] == Biotic_inter[[1]][multi_inter[i], 3])
#
#     for(j in 2:length(duplicata)) {
#         if(Biotic_inter[[1]][duplicata[j], 'source'] == "") {
#             NULL
#         } else {
#             Biotic_inter[[1]][duplicata[1], 'source'] <- paste(c(Biotic_inter[[1]][duplicata[1], 'source'], Biotic_inter[[1]][duplicata[j], 'source']), collapse = " | ")
#         }
#         # to.remove <- c(to.remove, duplicata[j])
#     }#j
# }#i
# # Biotic_inter[[1]] <- Biotic_inter[[1]][-to.remove, ]
#
# # Set of sources
# #   Pour les analyses, tout sera retiré pour chaque S1, même si des informations alternatives sont disponibles. On sera donc nécessairement à blind = TRUE
# load("RData/Tanimoto_data.RData")
