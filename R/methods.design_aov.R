#' Methods for design_aov objects
#' 
#' Methods defined for objects returned from the ANOVA_design functions.
#' 
#' @param x object of class \code{design_aov} as returned from \code{ANOVA_design}
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{ANOVA_design}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the study design created from \code{ANOVA_design} function}
#'   \item{\code{plot}}{Returns \code{meansplot} from created from the \code{ANOVA_design} function}
#' }
#' 
#' @name design_aov-methods


### methods for design_aov (created by ANOVA_design)

#' @rdname design_aov-methods
#' @method print design_aov
#' @export

print.design_aov <- function(x,...){
  cat("The design is specified as:",x$design)
  cat("\n")
  cat("Summary of means (mu) and standard deviations (SD)")
  cat("\n")
  print(x$meansplot$data)
  cat("\n")
  cat("Correlation Matrix")
  cat("\n")
  print(x$cor_mat)
}

#' @rdname design_aov-methods
#' @method plot design_aov
#' @import ggplot2
#' @export

plot.design_aov <- function(x,...){

  return(x$meansplot)

}