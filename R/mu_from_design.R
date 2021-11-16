#' Get means from Cohen's f 
#' 
#' A function for producing a pattern of means that will 
#' 
#' @return
#' @examples
#' # To be added
#' @export
#'

mu_from_design <- function(x, f) {
  model_matrix <- model_matrix_from_design(x)
  
  model_matrix_subset <- model_matrix[, names(f)]
  mu <- t(t(model_matrix_subset) * unlist(f))
  
  rowSums(mu)
}