#' Justify your alpha level by minimizing or balancing Type 1 and Type 2 error rates for ANOVAs.
#' @param design_result Output from the ANOVA_design function
#' @param correction Set a correction of violations of sphericity. This can be set to "none", "GG" Greenhouse-Geisser, and "HF" Huynh-Feldt
#' @param verbose Set to FALSE to not print results (default = TRUE)
#' @param emm Set to FALSE to not perform analysis of estimated marginal means
#' @param emm_model Set model type ("multivariate", or "univariate") for estimated marginal means
#' @param contrast_type Select the type of comparison for the estimated marginal means. Default is pairwise. See ?emmeans::`contrast-methods` for more details on acceptable methods.
#' @param emm_comp Set the comparisons for estimated marginal means comparisons. This is a factor name (a), combination of factor names (a+b), or for simple effects a | sign is needed (a|b)
#' @param costT1T2 Relative cost of Type 1 errors vs. Type 2 errors.
#' @param priorH1H0 How much more likely a-priori is H1 than H0?
#' @param error Either "minimal" to minimize error rates, or "balance" to balance error rates.
#' @param verbose Print each iteration of the optimization function if TRUE. Defaults to FALSE.
#' @return
#' alpha = alpha or Type 1 error that minimizes or balances combined error rates
#' beta = beta or Type 2 error that minimizes or balances combined error rates
#' objective = value that is the result of the minimization, either 0 (for balance) or the combined weighted error rates
#'
#' @examples
#' ## To be added
#' @section References:
#' too be added
#' @importFrom stats optimize
#' @export
#'


ANOVA_compromise <- function(design_result,
                             correction = Superpower_options("correction"), 
                             emm = Superpower_options("emm"),
                             emm_model = Superpower_options("emm_model"),
                             contrast_type = Superpower_options("contrast_type"),
                             emm_comp,
                             costT1T2 = 1, priorH1H0 = 1, error = "minimal", 
                             verbose = Superpower_options("verbose"), 
                             plot = Superpower_options("plot")){
  
  x1 = ANOVA_exact2(design_result,
                           correction = correction,
                           emm = emm,
                           emm_model = emm_model,
                           emm_comp = emm_comp)
  aov_comp = data.frame(effect = rownames(x1$main_results),
                        cohen_f = x1$main_results$cohen_f,
                        num_Df = x1$anova_table$num_Df,
                        den_Df = x1$anova_table$den_Df,
                        alpha = NA,
                        beta = NA,
                        objective = NA)
  
  #Calculate noncentrality
  aov_comp$f2 = aov_comp$cohen_f^2
  aov_comp$lambda <- aov_comp$f2*aov_comp$den_Df
  
  aov_plotlist = list()
  
  for(i in 1:nrow(x1$main_results)) {
    if(x1$main_results[i,]$partial_eta_squared >  1e-11){
      run_func = "(1 - pf(
  qf((1 - x),
     aov_comp$num_Df[subline],
     aov_comp$den_Df[subline]),
  aov_comp$num_Df[subline],
  aov_comp$den_Df[subline],
  aov_comp$lambda[subline]
))"
      run_func2 = gsub("subline",i,run_func)
      alpha_res = Superpower::optimal_alpha(power_function=run_func2,
                                            costT1T2 = costT1T2, 
                                            priorH1H0 = priorH1H0, 
                                            error = error)
      aov_comp[i,]$alpha = alpha_res$alpha
      aov_comp[i,]$beta = alpha_res$beta
      aov_comp[i,]$objective = alpha_res$objective
      aov_name = aov_comp[i,]$effect
      alpha_res$plot = alpha_res$plot + ggtitle(aov_name)
      aov_plotlist[[aov_name]] = alpha_res$plot
    } 
  }
  
  invisible(list(aov_comp = aov_comp,
                 aov_plotlist = aov_plotlist))
  
}
