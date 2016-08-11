consumer_set_of_resource <- function(consumer, resource, inter_type) {
  # The arguments must represent binary interactions identifed by 1 (interaction) or 0 (non-interaction)
  consumer_list <- as.character(unique(consumer))
  cons_res <- matrix(nrow = length(consumer_list), ncol = 3, dimnames = list(c(), c('consumer', 'resource', 'non-resource')))
  cons_res[, 'consumer'] <- consumer_list

  for(i in 1:length(consumer_list)){
    yes.resource <- resource[names(which(inter_type[which(consumer == consumer_list[i])] == "1"))]
    non.resource <- resource[names(which(inter_type[which(consumer == consumer_list[i])] == "0"))]
    cons_res[i , 'resource'] <- paste(unique(yes.resource), collapse = ' | ')
    cons_res[i , 'non-resource'] <- paste(unique(non.resource), collapse = ' | ')
  } #i

return(cons_res)

} #function
