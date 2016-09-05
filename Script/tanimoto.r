tanimoto <- function(resource_x, resource_y) {
  # The Tanimoto similarity computes the sum of shared elements in vectors resource_x and resource_y and divides this by the length of the longest vector
  # If either length of resource_x or resource_y == 0, similarity == 0
  # The order of vectors consumer_x or consumer_y has no importance, as long as elements in vectors are unique

  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # The same funcion is apparently used for trait similarity, including phylogeny
  # !!! There are NAs in the taxonomy that need to be taken into account, which is not the case at the moment !!!
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if(length(resource_x) == 0 || length(resource_y) == 0) {
        return(0.0)
    } else if(resource_x == "" || resource_y == "") {
        return(0.0)
    } else {
        inter <- sum(resource_x %in% resource_y)
        return(inter / ((length(resource_x) + length(resource_y)) - inter))
    }#if
}#end tanimoto function
