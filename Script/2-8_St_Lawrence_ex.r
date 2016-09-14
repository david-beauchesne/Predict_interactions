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

sp_SSL <- matrix(ncol = 2, nrow = 29, data = NA, dimnames = list(c(), c('ACCR','SP')))

sp_SSL[,1] <- c('WHA','HAS','HOS','GRS','HSE','SEA','LCO','SCO','LGH','SAP','LAP','FLO','SKA','RED','LDF','SDF','CAP','LPF','PISF','PLSF','SHR','LCRU','ECH','MOL','POL','OBI','LZOO','SZOO','PHY')

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

load("./RData/Tanimoto_data.RData")
# S0: A large set of species and their preys, with column structure ['taxon', 'taxonomy', 'resource', 'non-resource', 'consumer', 'non-consumer']
# Format interaction catalogue to fit this table format
    S0 <- matrix(nrow = nrow(Tanimoto_data[[1]]), ncol = 6, data = "", dimnames = list(Tanimoto_data[[1]][, 'taxon'], c('taxon', 'taxonomy', 'resource', 'non-resource', 'consumer', 'non-consumer')))
    S0[, 1] <- Tanimoto_data[[1]][, 'taxon']
    S0[, 2] <- Tanimoto_data[[1]][, 'kingdom | phylum | class | order | family | genus | species']
    # From binary interactions catalogue with consumer, resources, interaction or non-interaction
    for(k in 1:nrow(Tanimoto_data[[3]])) {
        S0[Tanimoto_data[[3]][k, 'consumer'], 3] <- Tanimoto_data[[3]][k, 'resource']
        S0[Tanimoto_data[[3]][k, 'consumer'], 4] <- Tanimoto_data[[3]][k, 'non-resource']
        S0[Tanimoto_data[[3]][k, 'consumer'], 5] <- Tanimoto_data[[6]][k, 'consumer']
        S0[Tanimoto_data[[3]][k, 'consumer'], 6] <- Tanimoto_data[[6]][k, 'non-consumer']
    }


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

SSL_predict2 <- full_algorithm(Kc = 4,
                            Kr = 4,
                            S0 = S0,
                            S1 = S1,
                            MW = 1,
                            wt = 0.5,
                            minimum_threshold = 0.2)

SSL_predict_mat <- prediction_matrix(S1 = S1, predictions = SSL_predict)
SSL_predict_mat2 <- prediction_matrix(S1 = S1, predictions = SSL_predict2)
x <- SSL_predict_mat

for(i in 1:nrow(sp_SSL)) {
    Sx <- unique(unlist(str_split(sp_SSL[i,2], ' - ')))
    for(j in 1:length(Sx)){
        for(k in 1:length(S1))
        if(S1[k] %in% Sx == TRUE) {
            colnames(SSL_predict_mat)[k] <- rownames(SSL_predict_mat)[k] <- sp_SSL[i, 2]
            colnames(SSL_predict_mat2)[k] <- rownames(SSL_predict_mat2)[k] <- sp_SSL[i, 2]
        }
    }
}

SSL_predict_mat_combine <- dupl_sp(SSL_predict_mat)
SSL_predict_mat_combine2 <- dupl_sp(SSL_predict_mat2)

SSL_emp <- SSL[[2]]
colnames(SSL_emp) <- rownames(SSL_emp) <- sp_SSL[,2]
SSL_emp <-  dupl_sp(SSL_emp)

accuracy_SSL <- prediction_accuracy_id(predicted = SSL_predict_mat_combine, empirical = SSL_emp)
accuracy_SSL2 <- prediction_accuracy_id(predicted = SSL_predict_mat_combine2, empirical = SSL_emp)
accuracy_SSL
accuracy_SSL2

for(i in 2:nrow(accuracy_SSL[[4]])) {
    print(paste(rownames(SSL_predict_mat_combine)[accuracy_SSL[[4]][i, 2]], "EATS", rownames(SSL_predict_mat_combine)[accuracy_SSL[[4]][i, 1]]))
}

for(i in 2:nrow(accuracy_SSL[[3]])) {
    print(paste(rownames(SSL_predict_mat_combine)[accuracy_SSL[[3]][i, 2]], "EATS", rownames(SSL_predict_mat_combine)[accuracy_SSL[[3]][i, 1]]))
}


SSL_bin_inter <- bin_inter(SSL_predict_mat_combine)
SSL_bin_inter2 <- bin_inter(SSL_predict_mat_combine2)
SSL_emp_bin <- bin_inter(SSL_emp)
SSL_bin_inter <- SSL_bin_inter[which(SSL_bin_inter[, 'FeedInter'] == '1'), ]
SSL_bin_inter2 <- SSL_bin_inter2[which(SSL_bin_inter2[, 'FeedInter'] == '1'), ]
SSL_emp_bin <- SSL_emp_bin[which(SSL_emp_bin[, 'FeedInter'] == '1'), ]

# SSL species with interactions noted in catalogue
x <- which(S0[, 'taxon'] %in% S1)
length(which(S0[x,'resource'] != "" | S0[x,'consumer'] != ""))




# Load package
library(networkD3)
# Plot
simpleNetwork(as.data.frame(SSL_bin_inter[, c(1,3)]))
simpleNetwork(as.data.frame(SSL_emp_bin[, c(1,3)]))





# remplace , par ' - '
# remplacer les noms de colonnes et lignes
# combiner duplicatas
# rouler fonction du catalogue pour séparer les lignes et colonnes qui ont plusieurs entrées?
# faire l'analyse en séparant toutes les espèces listées, puis comparer l'analyse divisée, compartimenter les résultats (combiner les interactions des espèces qui sont dans un compartiment), et la réseau présenté dans l'article à partir de la matrice de diète.
