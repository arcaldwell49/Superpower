#' Get means from Cohen's f 
#' 
#' A function for producing a pattern of means that will produce the Cohen's f values
#' 
#' @return
#' Vector of means
#' @examples
#' # To be added
#' @export
#'

mu_from_design <- function(design, f_list) {
  label_list = list()
  labelnames = vector()
  factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
  for (i1 in 1:length(factor_levels)){
    label_list1 = NULL
    labelnames <- append(labelnames,paste(paste(letters[i1]), sep = ""))
    
    for (i2 in 1:factor_levels[i1]){
      labelnames <- append(labelnames,paste(paste(letters[i1]), paste(i2), sep = ""))
      label_list1 = c(label_list1,paste(paste(letters[i1]), paste(i2), sep = ""))
    }
    
    label_list[[i1]] = as.vector(label_list1)
    names(label_list)[i1] = paste(paste(letters[i1]), sep = "")
    
  }
  
  x = label_list
  model_matrix <- model_matrix_from_design(x= label_list)
  
  model_matrix_subset <- model_matrix[, names(f_list)]
  mu <- t(t(model_matrix_subset) * unlist(f_list))
  
  rowSums(mu)
}

mu_from_design(design="3b", f_list = list(a=.1))
