resource_set_of_consumer <- function(consumer, resource, inter_type) {
  # The arguments must represent binary interactions identifed by 1 (interaction) or 0 (non-interaction)
  resource_list <- as.character(unique(resource))
  res_cons <- matrix(nrow = length(resource_list), ncol = 3, dimnames = list(c(), c('resource', 'consumer', 'non-consumer')))
  res_cons[, 'resource'] <- resource_list

  for(i in 1:length(resource_list)){
    yes.consumer <- consumer[names(which(inter_type[which(resource == resource_list[i])] == "1"))]
    non.consumer <- consumer[names(which(inter_type[which(resource == resource_list[i])] == "0"))]
    res_cons[i , 'consumer'] <- paste(unique(yes.consumer), collapse = ' | ')
    res_cons[i , 'non-consumer'] <- paste(unique(non.consumer), collapse = ' | ')
  } #i

return(res_cons)

} #function
