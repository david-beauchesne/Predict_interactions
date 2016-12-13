# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#   2.8 Example with southern St. Lawrence EwE model for mid-1980s
# -----------------------------------------------------------------------------

# Evaluating algorithm accuracy ~ # of taxa in the catalog
# -----------------------------------------------------------------------------
# FILES:
#   RData <- file = "./RawData/South_St_Lawrence_EwE.RData"
#   Script  <- file = "Script/2-8_St_Lawrence_ex.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# REFERENCE:
#   Savenkoff, to add
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
library(stringr)
load("./RawData/South_St_Lawrence_EwE.RData")
SSL <- South_St_Lawrence

SSL[[2]][which(SSL[[2]] > 0)] <- 1
rownames(SSL[[2]]) <- colnames(SSL[[2]]) <- SSL[[3]]

sp_SSL <- matrix(ncol = 3, nrow = 29, data = NA, dimnames = list(c(), c('ACCR','SP','FG')))

sp_SSL[,1] <- c('WHA','HAS','HOS','GRS','HSE','SEA','LCO','SCO','LGH','SAP','LAP','FLO','SKA','RED','LDF','SDF','CAP','LPF','PISF','PLSF','SHR','LCRU','ECH','MOL','POL','OBI','LZOO','SZOO','PHY')

sp_SSL[,3] <- c('Cetaceans',
'Harp seals',
'Hooded seals',
'Grey seals',
'Harbour seals',
'Seabirds',
'Atlantic cod',
'Atlantic cod',
'grayland halibut',
'American plaice',
'American plaice',
'Flounders',
'Skates',
'Redfish',
'Large demersal feeders',
'Small demersal feeders',
'Capelin',
'Large pelagic feeders',
'Piscivorous small pelagic feeders',
'Planktivorous small pelagic feeders',
'Shrimp',
'Large crustaceans',
'Echinoderms',
'Molluscs',
'Polychates',
'Other benthic invertebrates',
'Large zooplankton',
'Small zooplankton',
'Phytoplankton')



sp_SSL[,2] <- c('Balaenoptera physalus - Balaenoptera acutorostrata - Megaptera novaeangliae - Phocoena phocoena - Lagenorhynchus acutus - Lagenorhynchus albirostris',
'Pagophilus groenlandicus',
'Cystophora cristata',
'Halichoerus grypus',
'Phoca vitulina',
'Phalacrocorax carbo - Phalacrocorax auritus - Larus delawarensis - Larus argentatus - Larus marinus - Sterna hirundo - Sterna paradisaea - Cepphus grylle - Oceanodroma leucorhoa - Morus bassanus - Rissa tridactyla - Uria aalge - Alca torda - Fratercula arctica',
'Gadus morhua',
'Gadus morhua',
'Reinhardtius hippoglossoides',
'Hippoglossoides platessoides',
'Hippoglossoides platessoides',
'Limanda ferruginea - Glyptocephalus cynoglossus - Pseudopleuronectes americanus',
'Amblyraja radiata - Malacoraja senta - Leucoraja ocellata',
'Sebastes mentella - Sebastes fasciatus',
'Urophycis tenuis - Melanogrammus aeglefinus - Centroscyllium fabricii - Anarhichas - Cyclopterus lumpus - Lycodes - Macrouridae - Zoarcidae - Lophius americanus - Hippoglossus hippoglossus',
'Myoxocephalus - Tautogolabrus adspersus - Zoarces americanus',
'Mallotus villosus',
'Squalus acanthias - Pollachius virens - Merluccius bilinearis - Cetorhinus maximus',
'Scomber scombrus - Illex illecebrosus',
'Clupea harengus - Scomberesox saurus - Gonatus',
'Argis dentata - Eualus macilentus - Eualus gaimardi - Pandalus montagui',
'Chionoecetes opilio - Hyas',
'Echinarachnius parma - Stronglyocentrotus pallidus - Ophiura robusta',
'Mesodesma deauratum - Cyrtodaria siliqua',
'Parexogone hebes',
'Miscellaneous crustaceans', #to remove OBI
'Euphausiids - chaetognaths', # to remove LZOO
'Oithona similis - Temora longicornis - Pseudocalanus - Calanus finmarchicus',
'Chaetoceros affinis - Chaetoceros - Leptocylindrus minimus - Thalassiiosira nordenskioldii - Thalassiiosira - Fragilariopsis')

# Removing portions of the web for which there is no taxa usable for the analysis, 'OBI' & 'LZOO'
SSL[[2]] <- SSL[[2]][-which(colnames(SSL[[2]]) == 'OBI' | colnames(SSL[[2]]) == 'LZOO'), -which(colnames(SSL[[2]]) == 'OBI' | colnames(SSL[[2]]) == 'LZOO')]
sp_SSL <- sp_SSL[-which(sp_SSL[,1] == 'OBI' | sp_SSL[,1] == 'LZOO'), ]

S1 <- unique(unlist(str_split(sp_SSL[,2], ' - ')))

load("./RData/S0_catalog.RData")
S0 <- S0_catalog


# Have to extract taxonomy for speies that are not found in S0
S1_missing <- which(!S1 %in% S0[,1])

S1_add_S0 <- matrix(ncol = 6, nrow = length(S1_missing), data = "", dimnames = list(c(), c("taxon", "taxonomy", "resource", "non-resource", "consumer", "non-consumer")))

S1_add_S0[, 'taxon'] <- S1[S1_missing]

S1_add_S0[, 'taxonomy'] <- c('Animalia | Chordata | Mammalia | Cetartiodactyla | Delphinidae | Lagenorhynchus | Lagenorhynchus acutus',
'Animalia | Chordata | Mammalia | Carnivora | Phocidae | Halichoerus | Halichoerus grypus',
'Animalia | Chordata | Procellariiformes | Hydrobatidae | Oceanodroma | Oceanodroma leucorhoa',
'Animalia | Chordata | Aves | Pelecaniformes | Sulidae | Morus | Morus bassanus',
'Animalia | Chordata | Aves | Charadriiformes | Alcidae | Alca | Alca torda',
'Animalia | Chordata | Elasmobranchii | Rajiformes | Rajidae | Malacoraja | Malacoraja senta',
'Animalia | Chordata | Elasmobranchii | Squaliformes | Etmopteridae | Centroscyllium | Centroscyllium fabricii',
'Animalia | Arthropoda | Malacostraca | Decapoda | Crangonidae | Argis | Argis dentata',
'Animalia | Arthropoda | Malacostraca | Decapoda | Thoridae | Eualus | Eualus macilentus',
'Animalia | Arthropoda | Malacostraca | Decapoda | Thoridae | Eualus | Eualus gaimardii',
'Animalia | Echinodermata | Echinoidea | Camarodonta | Strongylocentrotidae | Strongylocentrotus | Strongylocentrotus pallidus',
'Animalia | Mollusca | Bivalvia | Imparidentia | Mesodesmatidae | Mesodesma | Mesodesma deauratum',
'Animalia | Mollusca | Bivalvia | Adapedonta | Hiatellidae | Cyrtodaria | Cyrtodaria siliqua',
'Animalia | Annelida | Polychaeta | Phyllodocida | Syllidae | Parexogone | Parexogone hebes',
'Chromista | Ochrophyta | Bacillariophyceae | Chaetocerotanae | Chaetocerotaceae | Chaetoceros | Chaetoceros affinis',
'Chromista | Ochrophyta | Bacillariophyceae | Leptocylindrales | Leptocylindraceae | Leptocylindrus | Leptocylindrus minimus',
'Chromista | Ochrophyta | Bacillariophyceae | Thalassiosirales | Thalassiosiraceae | Thalassiosira | Thalassiosira nordenskioeldii',
'Chromista | Ochrophyta | Bacillariophyceae | Thalassiosirales | Thalassiosiraceae | Thalassiosira | NA')

 S0 <- rbind(S0, S1_add_S0) #binding missing taxonomies
 rownames(S0) <- S0[, 'taxon']


# #Thinning down catalogue
# S02 <- S0[unique(c(which(S0[, 'resource'] != ""), which(S0[, 'consumer'] != ""))), ]
#
# S1_missing2 <- which(!S1 %in% S02[,1]) #after culling
# S1_missing3 <- S1_missing2[which(!S1_missing2 %in% S1_missing)] #taxo to keep
# S1_add_S0 <- rbind(S1_add_S0, S0[which(S0[, 'taxon'] %in% S1[S1_missing3]), ])
# rownames(S1_add_S0) <- NULL
# S0 <- rbind(S02, S1_add_S0)
# remove(S02,S1_missing3,S1_missing2,S1_missing)

# Predicting interactions
SSL_predict <- full_algorithm(Kc = 4,
                            Kr = 4,
                            S0 = S0,
                            S1 = S1,
                            MW = 1,
                            wt = 0.5,
                            minimum_threshold = 0.3)

# SSL_predict2 <- full_algorithm(Kc = 4,
#                             Kr = 4,
#                             S0 = S0,
#                             S1 = S1,
#                             MW = 1,
#                             wt = 0.5,
#                             minimum_threshold = 0.2)

SSL_predict_mat <- prediction_matrix(S1 = S1, predictions = SSL_predict)
# SSL_predict_mat2 <- prediction_matrix(S1 = S1, predictions = SSL_predict2)
x <- SSL_predict_mat

for(i in 1:nrow(sp_SSL)) {
    Sx <- unique(unlist(str_split(sp_SSL[i,2], ' - ')))
    for(j in 1:length(Sx)){
        for(k in 1:length(S1))
        if(S1[k] %in% Sx == TRUE) {
            colnames(SSL_predict_mat)[k] <- rownames(SSL_predict_mat)[k] <- sp_SSL[i, 2]
            # colnames(SSL_predict_mat2)[k] <- rownames(SSL_predict_mat2)[k] <- sp_SSL[i, 2]
        }
    }
}

SSL_predict_mat_combine <- dupl_sp(SSL_predict_mat)
# SSL_predict_mat_combine2 <- dupl_sp(SSL_predict_mat2)

SSL_emp <- SSL[[2]]
colnames(SSL_emp) <- rownames(SSL_emp) <- sp_SSL[,2]
SSL_emp <-  dupl_sp(SSL_emp)

accuracy_SSL <- prediction_accuracy_id(predicted = SSL_predict_mat_combine, empirical = SSL_emp)
# accuracy_SSL2 <- prediction_accuracy_id(predicted = SSL_predict_mat_combine2, empirical = SSL_emp)
accuracy_SSL
# accuracy_SSL2

# for(i in 2:nrow(accuracy_SSL[[4]])) {
#     print(paste(rownames(SSL_predict_mat_combine)[accuracy_SSL[[4]][i, 2]], "EATS", rownames(SSL_predict_mat_combine)[accuracy_SSL[[4]][i, 1]]))
# }
#
# for(i in 2:nrow(accuracy_SSL[[3]])) {
#     print(paste(rownames(SSL_predict_mat_combine)[accuracy_SSL[[3]][i, 2]], "EATS", rownames(SSL_predict_mat_combine)[accuracy_SSL[[3]][i, 1]]))
# }


SSL_bin_inter <- bin_inter(SSL_predict_mat_combine)
# SSL_bin_inter2 <- bin_inter(SSL_predict_mat_combine2)
SSL_emp_bin <- bin_inter(SSL_emp)
SSL_bin_inter <- SSL_bin_inter[which(SSL_bin_inter[, 'FeedInter'] == '1'), ]
# SSL_bin_inter2 <- SSL_bin_inter2[which(SSL_bin_inter2[, 'FeedInter'] == '1'), ]
SSL_emp_bin <- SSL_emp_bin[which(SSL_emp_bin[, 'FeedInter'] == '1'), ]

# SSL species with interactions noted in catalogue
x <- which(S0[, 'taxon'] %in% S1)
length(which(S0[x,'resource'] != "" | S0[x,'consumer'] != ""))

id_c <- matrix(nrow = nrow(accuracy_SSL[[4]]), ncol = 2, data = NA, dimnames = list(c(), c('consumer','resource')))
for(i in 2:nrow(accuracy_SSL[[4]])) {
    id_c[i,1] <- rownames(SSL_predict_mat_combine)[accuracy_SSL[[4]][i, 1]]
    id_c[i,2] <- rownames(SSL_predict_mat_combine)[accuracy_SSL[[4]][i, 2]]
}

id_b <- matrix(nrow = nrow(accuracy_SSL[[3]]), ncol = 2, data = NA, dimnames = list(c(), c('consumer','resource')))
for(i in 2:nrow(accuracy_SSL[[3]])) {
    id_b[i,1] <- rownames(SSL_predict_mat_combine)[accuracy_SSL[[3]][i, 1]]
    id_b[i,2] <- rownames(SSL_predict_mat_combine)[accuracy_SSL[[3]][i, 2]]
}
for(i in 1:nrow(sp_SSL)){
    id_c[which(id_c[, 'consumer'] == sp_SSL[i,2]), 'consumer'] <- sp_SSL[i,3]
    id_c[which(id_c[, 'resource'] == sp_SSL[i,2]), 'resource'] <- sp_SSL[i,3]
    id_b[which(id_b[, 'consumer'] == sp_SSL[i,2]), 'consumer'] <- sp_SSL[i,3]
    id_b[which(id_b[, 'resource'] == sp_SSL[i,2]), 'resource'] <- sp_SSL[i,3]
}



pp <- which(SSL_emp_bin[,'Predator'] == "Scomber scombrus - Illex illecebrosus" | SSL_emp_bin[,'Prey'] == "Scomber scombrus - Illex illecebrosus")
cap <- which(SSL_emp_bin[,'Predator'] == "Mallotus villosus" | SSL_emp_bin[,'Prey'] == "Mallotus villosus")
SSL_emp_part <- SSL_emp_bin[unique(c(pp,cap)), ]


pp <- which(SSL_bin_inter[,'Predator'] == "Scomber scombrus - Illex illecebrosus" | SSL_bin_inter[,'Prey'] == "Scomber scombrus - Illex illecebrosus")

cap <- which(SSL_bin_inter[,'Predator'] == "Mallotus villosus" | SSL_bin_inter[,'Prey'] == "Mallotus villosus")
SSL_pred_part <- SSL_bin_inter[unique(c(pp,cap)), ]

for(i in 1:nrow(sp_SSL)){
    SSL_emp_part[which(SSL_emp_part[, 'Predator'] == sp_SSL[i,2]), 'Predator'] <- sp_SSL[i,3]
    SSL_emp_part[which(SSL_emp_part[, 'Prey'] == sp_SSL[i,2]), 'Prey'] <- sp_SSL[i,3]
    SSL_pred_part[which(SSL_pred_part[, 'Predator'] == sp_SSL[i,2]), 'Predator'] <- sp_SSL[i,3]
    SSL_pred_part[which(SSL_pred_part[, 'Prey'] == sp_SSL[i,2]), 'Prey'] <- sp_SSL[i,3]
}

unique(c(SSL_emp_part,SSL_pred_part))

SSL_pred_part <- gsub("1", "->",SSL_pred_part)
SSL_pred_part <- gsub("Skates", "1",SSL_pred_part)
SSL_pred_part <- gsub("Cetaceans", "2",SSL_pred_part)
SSL_pred_part <- gsub("Hooded seals", "3",SSL_pred_part)
SSL_pred_part <- gsub("Atlantic cod", "4",SSL_pred_part)
SSL_pred_part <- gsub("Grey seals", "5",SSL_pred_part)
SSL_pred_part <- gsub("Harp seals", "6",SSL_pred_part)
SSL_pred_part <- gsub("Seabirds", "7",SSL_pred_part)
SSL_pred_part <- gsub("Harbour seals", "8",SSL_pred_part)
SSL_pred_part <- gsub("grayland halibut", "9",SSL_pred_part)
SSL_pred_part <- gsub("Piscivorous small pelagic feeders", "10",SSL_pred_part)
SSL_pred_part <- gsub("Redfish", "11",SSL_pred_part)
SSL_pred_part <- gsub("Large pelagic feeders", "12",SSL_pred_part)
SSL_pred_part <- gsub("Large demersal feeders", "13",SSL_pred_part)
SSL_pred_part <- gsub("Capelin", "14",SSL_pred_part)
SSL_pred_part <- gsub("Small demersal feeders","15",SSL_pred_part)
SSL_pred_part <- gsub("Planktivorous small pelagic feeders", "16",SSL_pred_part)
SSL_pred_part <- gsub("Small zooplankton", "17",SSL_pred_part)
SSL_pred_part <- gsub("Flounders", "18",SSL_pred_part)
SSL_pred_part <- gsub("Large crustaceans", "19",SSL_pred_part)
SSL_pred_part <- gsub("American plaice", "20",SSL_pred_part)
SSL_pred_part <- gsub("Shrimp", "21",SSL_pred_part)

SSL_emp_part <- gsub("1", "->",SSL_emp_part)
SSL_emp_part <- gsub("Skates", "1",SSL_emp_part)
SSL_emp_part <- gsub("Cetaceans", "2",SSL_emp_part)
SSL_emp_part <- gsub("Hooded seals", "3",SSL_emp_part)
SSL_emp_part <- gsub("Atlantic cod", "4",SSL_emp_part)
SSL_emp_part <- gsub("Grey seals", "5",SSL_emp_part)
SSL_emp_part <- gsub("Harp seals", "6",SSL_emp_part)
SSL_emp_part <- gsub("Seabirds", "7",SSL_emp_part)
SSL_emp_part <- gsub("Harbour seals", "8",SSL_emp_part)
SSL_emp_part <- gsub("grayland halibut", "9",SSL_emp_part)
SSL_emp_part <- gsub("Piscivorous small pelagic feeders", "10",SSL_emp_part)
SSL_emp_part <- gsub("Redfish", "11",SSL_emp_part)
SSL_emp_part <- gsub("Large pelagic feeders", "12",SSL_emp_part)
SSL_emp_part <- gsub("Large demersal feeders", "13",SSL_emp_part)
SSL_emp_part <- gsub("Capelin", "14",SSL_emp_part)
SSL_emp_part <- gsub("Small demersal feeders", "15",SSL_emp_part)
SSL_emp_part <- gsub("Planktivorous small pelagic feeders", "16",SSL_emp_part)
SSL_emp_part <- gsub("Small zooplankton", "17",SSL_emp_part)
SSL_emp_part <- gsub("Flounders", "18",SSL_emp_part)
SSL_emp_part <- gsub("Large crustaceans", "19",SSL_emp_part)
SSL_emp_part <- gsub("American plaice", "20",SSL_emp_part)
SSL_emp_part <- gsub("Shrimp", "21",SSL_emp_part)

SSL_emp_part
SSL_pred_part


# Figure 4 - article couleur
library(DiagrammeR)
grViz("

digraph boxes_and_circles{

    node [shape = box
            # fixedsize = TRUE
            # width = 2.5
            ]
            1 [label =  <Skates>]
            2 [label =  <Cetaceans>]
            3 [label =  <Hooded seals>]
            4 [label =  <Atlantic cod>]
            5 [label =  <Grey seals>]
            6 [label =  <Harp seals>]
            7 [label =  <Seabirds>]
            8 [label =  <Harbour seals>]
            9 [label =  <grayland halibut>]
            10 [label =  <Piscivorous small<br/>pelagic feeders>]
            11 [label =  <Redfish>]
            12 [label =  <Large pelagic<br/>feeders>]
            13 [label =  <Large demersal<br/>feeders>]
            14 [label =  <Capelin>]
            15 [label =  <Small demersal<br/>feeders>]
            16 [label =  <Planktivorous small<br/>pelagic feeders>]
            17 [label =  <Small zooplankton>]
            18 [label =  <Flounders>]
            19 [label =  <Large crustaceans>]
            20 [label =  <American plaice>]
            21 [label =  <Shrimp>]

    edge [dir = back]
            12 -> 15 [color = 'transparent']
            12 -> 16 [color = 'transparent']
            13 -> 15 [color = 'transparent']
            13 -> 16 [color = 'transparent']
            20 -> 15 [color = 'transparent']
            20 -> 16 [color = 'transparent']
            18 -> 15 [color = 'transparent']
            18 -> 16 [color = 'transparent']
            4 -> 15 [color = 'transparent']
            4 -> 16 [color = 'transparent']
            15 -> 21 [color = 'transparent']
            15 -> 19 [color = 'transparent']
            16 -> 21 [color = 'transparent']
            16 -> 19 [color = 'transparent']

            2 -> 11 [color = 'transparent']
            2 -> 1 [color = 'transparent']
            2 -> 9 [color = 'transparent']
            3 -> 11 [color = 'transparent']
            3 -> 1 [color = 'transparent']
            3 -> 9 [color = 'transparent']
            5 -> 11 [color = 'transparent']
            5 -> 1 [color = 'transparent']
            5 -> 9 [color = 'transparent']
            6 -> 11 [color = 'transparent']
            6 -> 1 [color = 'transparent']
            6 -> 9 [color = 'transparent']

            #Empirical & predictions
            1 -> 10 [color = 'green', style = 'dashed']
            2 -> 10 [color = 'green', style = 'dashed']
            3 -> 10 [color = 'green', style = 'dashed']
            4 -> 10 [color = 'green', style = 'dashed']
            6 -> 10 [color = 'green', style = 'dashed']
            7 -> 10 [color = 'green', style = 'dashed']
            8 -> 10 [color = 'green', style = 'dashed']
            10 -> 16 [color = 'green', style = 'dashed']
            10 -> 14 [color = 'green', style = 'dashed']
            10 -> 17 [color = 'green', style = 'dashed']
            12 -> 10 [color = 'green', style = 'dashed']
            13 -> 10 [color = 'green', style = 'dashed']
            1 -> 14 [color = 'green', style = 'dashed']
            2 -> 14 [color = 'green', style = 'dashed']
            3 -> 14 [color = 'green', style = 'dashed']
            4 -> 14 [color = 'green', style = 'dashed']
            5 -> 14 [color = 'green', style = 'dashed']
            14 -> 17 [color = 'green', style = 'dashed']
            15 -> 14 [color = 'green', style = 'dashed']
            6 -> 14 [color = 'green', style = 'dashed']
            7 -> 14 [color = 'green', style = 'dashed']
            8 -> 14 [color = 'green', style = 'dashed']
            9 -> 14 [color = 'green', style = 'dashed']
            11 -> 14 [color = 'green', style = 'dashed']
            12 -> 14 [color = 'green', style = 'dashed']
            13 -> 14 [color = 'green', style = 'dashed']

            # Empirical only
            5 -> 10 [color = 'black']
            9 -> 10 [color = 'black']
            11 -> 10 [color = 'black']

            # Predictions only
            18 -> 10 [color = 'blue', style = 'dotted']
            15 -> 10 [color = 'blue', style = 'dotted']
            10 -> 21 [color = 'blue', style = 'dotted']
            10 -> 4 [color = 'blue', style = 'dotted']
            10 -> 18 [color = 'blue', style = 'dotted']
            10 -> 15 [color = 'blue', style = 'dotted']
            10 -> 10 [color = 'blue', style = 'dotted']
            10 -> 12 [color = 'blue', style = 'dotted']
            10 -> 13 [color = 'blue', style = 'dotted']
            19 -> 14 [color = 'blue', style = 'dotted']
            16 -> 14 [color = 'blue', style = 'dotted']
            20 -> 14 [color = 'blue', style = 'dotted']
            18 -> 14 [color = 'blue', style = 'dotted']
            14 -> 14 [color = 'blue', style = 'dotted']
            16 -> 10 [color = 'blue', style = 'dotted']
            20 -> 10 [color = 'blue', style = 'dotted']
            10 -> 19 [color = 'blue', style = 'dotted']
            10 -> 20 [color = 'blue', style = 'dotted']
            10 -> 9 [color = 'blue', style = 'dotted']
}
")


# Figure 4 - article B&W
library(DiagrammeR)
grViz("

digraph boxes_and_circles{

    node [shape = box
            # fixedsize = TRUE
            # width = 2.5
            ]
            1 [label =  <Skates>]
            2 [label =  <Cetaceans>]
            3 [label =  <Hooded seals>]
            4 [label =  <Atlantic cod>]
            5 [label =  <Grey seals>]
            6 [label =  <Harp seals>]
            7 [label =  <Seabirds>]
            8 [label =  <Harbour seals>]
            9 [label =  <grayland halibut>]
            10 [label =  <Piscivorous small<br/>pelagic feeders>]
            11 [label =  <Redfish>]
            12 [label =  <Large pelagic<br/>feeders>]
            13 [label =  <Large demersal<br/>feeders>]
            14 [label =  <Capelin>]
            15 [label =  <Small demersal<br/>feeders>]
            16 [label =  <Planktivorous small<br/>pelagic feeders>]
            17 [label =  <Small zooplankton>]
            18 [label =  <Flounders>]
            19 [label =  <Large crustaceans>]
            20 [label =  <American plaice>]
            21 [label =  <Shrimp>]

    edge [dir = back]
            12 -> 15 [color = 'transparent']
            12 -> 16 [color = 'transparent']
            13 -> 15 [color = 'transparent']
            13 -> 16 [color = 'transparent']
            20 -> 15 [color = 'transparent']
            20 -> 16 [color = 'transparent']
            18 -> 15 [color = 'transparent']
            18 -> 16 [color = 'transparent']
            4 -> 15 [color = 'transparent']
            4 -> 16 [color = 'transparent']
            15 -> 21 [color = 'transparent']
            15 -> 19 [color = 'transparent']
            16 -> 21 [color = 'transparent']
            16 -> 19 [color = 'transparent']

            2 -> 11 [color = 'transparent']
            2 -> 1 [color = 'transparent']
            2 -> 9 [color = 'transparent']
            3 -> 11 [color = 'transparent']
            3 -> 1 [color = 'transparent']
            3 -> 9 [color = 'transparent']
            5 -> 11 [color = 'transparent']
            5 -> 1 [color = 'transparent']
            5 -> 9 [color = 'transparent']
            6 -> 11 [color = 'transparent']
            6 -> 1 [color = 'transparent']
            6 -> 9 [color = 'transparent']

            #Empirical & predictions
            1 -> 10 [color = 'black', style = 'dashed']
            2 -> 10 [color = 'black', style = 'dashed']
            3 -> 10 [color = 'black', style = 'dashed']
            4 -> 10 [color = 'black', style = 'dashed']
            6 -> 10 [color = 'black', style = 'dashed']
            7 -> 10 [color = 'black', style = 'dashed']
            8 -> 10 [color = 'black', style = 'dashed']
            10 -> 16 [color = 'black', style = 'dashed']
            10 -> 14 [color = 'black', style = 'dashed']
            10 -> 17 [color = 'black', style = 'dashed']
            12 -> 10 [color = 'black', style = 'dashed']
            13 -> 10 [color = 'black', style = 'dashed']
            1 -> 14 [color = 'black', style = 'dashed']
            2 -> 14 [color = 'black', style = 'dashed']
            3 -> 14 [color = 'black', style = 'dashed']
            4 -> 14 [color = 'black', style = 'dashed']
            5 -> 14 [color = 'black', style = 'dashed']
            14 -> 17 [color = 'black', style = 'dashed']
            15 -> 14 [color = 'black', style = 'dashed']
            6 -> 14 [color = 'black', style = 'dashed']
            7 -> 14 [color = 'black', style = 'dashed']
            8 -> 14 [color = 'black', style = 'dashed']
            9 -> 14 [color = 'black', style = 'dashed']
            11 -> 14 [color = 'black', style = 'dashed']
            12 -> 14 [color = 'black', style = 'dashed']
            13 -> 14 [color = 'black', style = 'dashed']

            # Empirical only
            5 -> 10 [color = 'black']
            9 -> 10 [color = 'black']
            11 -> 10 [color = 'black']

            # Predictions only
            18 -> 10 [color = 'gray', style = 'dotted']
            15 -> 10 [color = 'gray', style = 'dotted']
            10 -> 21 [color = 'gray', style = 'dotted']
            10 -> 4 [color = 'gray', style = 'dotted']
            10 -> 18 [color = 'gray', style = 'dotted']
            10 -> 15 [color = 'gray', style = 'dotted']
            10 -> 10 [color = 'gray', style = 'dotted']
            10 -> 12 [color = 'gray', style = 'dotted']
            10 -> 13 [color = 'gray', style = 'dotted']
            19 -> 14 [color = 'gray', style = 'dotted']
            16 -> 14 [color = 'gray', style = 'dotted']
            20 -> 14 [color = 'gray', style = 'dotted']
            18 -> 14 [color = 'gray', style = 'dotted']
            14 -> 14 [color = 'gray', style = 'dotted']
            16 -> 10 [color = 'gray', style = 'dotted']
            20 -> 10 [color = 'gray', style = 'dotted']
            10 -> 19 [color = 'gray', style = 'dotted']
            10 -> 20 [color = 'gray', style = 'dotted']
            10 -> 9 [color = 'gray', style = 'dotted']
}
")
