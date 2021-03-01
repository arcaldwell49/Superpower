#' Methods for opt_alpha objects
#' 
#' Methods defined for objects returned from the optimal_alpha and ANOVA_compromise functions.
#' 
#' @param x object of class \code{opt_alpha} as returned from one of the optimal alpha functions in Superpower.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{ANOVA_compromise}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the optimal alpha results}
#'   \item{\code{plot}}{Returns a plot}
#' }
#' 
#' @name opt_alpha-methods


### methods for opt_alpha

#' @rdname opt_alpha-methods
#' @method print opt_alpha
#' @export

print.opt_alpha <- function(x,...){
  if (x$method == "ANOVA_compromise") {
    cat("Optimal Alpha & Beta for ANOVA tests")
    cat("\n")
    print(x$aov_comp, digits=4)
    cat("\n")
    if (!is.null(x$emmeans_comp)) {
      cat("\n")
      cat("Optimal Alpha & Beta for Estimated Marginal Means")
      cat("\n")
      print(x$emmeans_comp, digits = 4)
      cat("\n")
    }
  } else if(x$method == "optimal_alpha") {
    df_res = data.frame(Alpha = x$alpha,
                        Beta = x$beta,
                        'Weighted Combined Error Rate' = x$objective)
    cat("\n")
    cat("Optimal Alpha")
    cat("\n")
    print(df_res)
    cat("\n")
    cat("From power function:")
    cat("\n")
    print(x$power_function)
    cat("\n")
  }

}

#' @rdname opt_alpha-methods
#' @method plot opt_alpha
#' @import ggplot2
#' @export

plot.opt_alpha <- function(x,...){
  if (x$method == "ANOVA_compromise") {
    return(x$aov_plotlist)
  } else if (x$method == "optimal_alpha") {
    return(x$plot)
  }

}



