#' Methods for sim_result objects
#' 
#' Methods defined for objects returned from the ANOVA_exact, ANOVA_exact2, and ANOVA_power functions.
#' 
#' @param x object of class \code{sim_result} as returned from one of the simulation functions in Superpower
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{ANOVA_design}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the simulation result}
#'   \item{\code{plot}}{Returns \code{meansplot} or a plot of the distribution of p-values depending on whether an exact or Monte Carlo simulation was performed}
#' }
#' 
#' @name sim_result-methods


### methods for sim_result

#' @rdname sim_result-methods
#' @method print sim_result
#' @export

print.sim_result <- function(x,...){
  if (x$method == "ANOVA_exact" || x$method == "ANOVA_exact2") {
    cat("Power and Effect sizes for ANOVA tests")
    cat("\n")
    print(round(x$main_results, 4))
    cat("\n")
    cat("Power and Effect sizes for pairwise comparisons (t-tests)")
    cat("\n")
    print(round(x$pc_results, 2))
    if (!is.null(x$emm_results)) {
      cat("\n")
      cat("Power and Effect sizes for estimated marginal means")
      cat("\n")
      print(x$emm_results, digits = 4)
    }
  } else if (x$method == "ANOVA_power") {
    cat("Power and Effect sizes for ANOVA tests")
    cat("\n")
    print(x$main_results, digits = 4)
    cat("\n")
    cat("Power and Effect sizes for pairwise comparisons (t-tests)")
    cat("\n")
    print(x$pc_results, digits = 4)
    cat("\n")
    if (!is.null(x$emm_results)) {
      cat("Power and Cohen's f from estimated marginal means")
      cat("\n")
      print(x$emm_results, digits = 4)
    }

  }
}

#' @rdname sim_result-methods
#' @method plot sim_result
#' @import ggplot2
#' @export

plot.sim_result <- function(x,...){
  if (x$method == "ANOVA_exact" || x$method == "ANOVA_exact2") {
    return(x$plot)
  } else if (x$method == "ANOVA_power") {
    return(list(plot1 = x$plot1,
                plot2 = x$plot2))
  }

}