#' Methods for sim_result objects
#' 
#' Methods defined for objects returned from the ANOVA_exact, ANOVA_exact2, and ANOVA_power functions.
#' 
#' @param x object of class \code{sim_result} as returned from one of the simulation functions in Superpower.
#' @param object Result returned from ANOVA_power (only applicable argument for confint)
#' @param level Argument for confint. Confidence level for binomial proportion confidence intervals (Wilson, 1927). Default is .95.
#' @param parm Argument for confint. Select what results from the simulation to return with confidence intervals. Options currently include: main_results (default), pc_results, manova_results, and emm_results.
#' @param ... further arguments passed through, see description of return value
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the simulation result}
#'   \item{\code{plot}}{Returns \code{meansplot} or a plot of the distribution of p-values depending on whether an exact or Monte Carlo simulation was performed}
#'   \item{\code{confint}}{Returns confidence intervals for the selected result from ANOVA_power}
#' }
#' @section References:
#' Wilson, E. (1927). Probable Inference, the Law of Succession, and Statistical Inference. Journal of the American Statistical Association, 22(158), 209-212. doi:10.2307/2276774
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
      cat("\n")
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
    cat("\n")

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

#' @rdname sim_result-methods
#' @method confint sim_result
#' @export

confint.sim_result = function(object, 
                              parm="main_results",
                              level = .95, ...){
  x = object
  return = parm
  conf_level = level
  if(x$method != "ANOVA_power"){
    stop("x must be from ANOVA_power")
  }
  if (is.element(return, 
                 c("main_results",
                   "aov",
                   "anova",
                   "pc_results",
                   "pairwise",
                   "emm_results",
                   "emmeans",
                   "emm",
                   "manova_results",
                   "manova"
                 )) == FALSE ) {
    stop("object is not set to an exported table from ANOVA_power")
  }
  
  if(return == "main_results" || return == "aov" || return == "anova"){
    num_sims = x$nsims
    results = x$main_results
    results$lower.ci = mapply(function(x, y)
      ci_binom(x,y)[1]*100,
      results$power,
      num_sims)
    results$upper.ci = mapply(function(x, y)
      ci_binom(x,y)[2]*100,
      results$power,
      num_sims)
    results = results[c("power","lower.ci","upper.ci")]
  } else if(return == "pc_results" || return == "pairwise"){
    num_sims = x$nsims
    results = x$pc_results
    results$lower.ci = mapply(function(x, y)
      ci_binom(x,y)[1]*100,
      results$power,
      num_sims)
    results$upper.ci = mapply(function(x, y)
      ci_binom(x,y)[2]*100,
      results$power,
      num_sims)
    results= results[c("power","lower.ci","upper.ci")]
  } else if(return == "manova_results" || return == "manova"){
    num_sims = x$nsims
    results = x$manova_results
    if(is.null(results)){
      stop("MANOVA results not included")
    }
    results$lower.ci = mapply(function(x, y)
      ci_binom(x,y)[1]*100,
      results$power,
      num_sims)
    results$upper.ci = mapply(function(x, y)
      ci_binom(x,y)[2]*100,
      results$power,
      num_sims)
    results= results[c("power","lower.ci","upper.ci")]
  } else if(return == "emm_results" || return == "emm" || return == "emmeans"){
    num_sims = x$nsims
    results = x$emm_results
    if(is.null(results)){
      stop("emm results not included")
    }
    results$lower.ci = mapply(function(x, y)
      ci_binom(x,y)[1]*100,
      results$power,
      num_sims)
    results$upper.ci = mapply(function(x, y)
      ci_binom(x,y)[2]*100,
      results$power,
      num_sims)
    results$contrast = x$emm_results$contrast
    results= results[c("power","contrast","lower.ci","upper.ci")]
  } 

  return(results)
  
}



