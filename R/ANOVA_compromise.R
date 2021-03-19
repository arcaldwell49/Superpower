#' Justify your alpha level by minimizing or balancing Type 1 and Type 2 error rates for ANOVAs.
#' @param design_result Output from the ANOVA_design function
#' @param correction Set a correction of violations of sphericity. This can be set to "none", "GG" Greenhouse-Geisser, and "HF" Huynh-Feldt
#' @param emm Set to FALSE to not perform analysis of estimated marginal means
#' @param emm_model Set model type ("multivariate", or "univariate") for estimated marginal means
#' @param contrast_type Select the type of comparison for the estimated marginal means. Default is pairwise. See ?emmeans::`contrast-methods` for more details on acceptable methods.
#' @param emm_comp Set the comparisons for estimated marginal means comparisons. This is a factor name (a), combination of factor names (a+b), or for simple effects a | sign is needed (a|b)
#' @param costT1T2 Relative cost of Type 1 errors vs. Type 2 errors.
#' @param priorH1H0 How much more likely a-priori is H1 than H0? Default is 1: equally likely.
#' @param error Either "minimal" to minimize error rates, or "balance" to balance error rates.
#' @param liberal_lambda Logical indicator of whether to use the liberal (cohen_f^2\*(num_df+den_df)) or conservative (cohen_f^2\*den_df) calculation of the noncentrality (lambda) parameter estimate. Default is FALSE.
#' @return Returns dataframe with simulation data (power and effect sizes!), optimal alpha level, obtained beta error rate (1-power/100), and objective (see below for details). If NA is obtained in a alpha/beta/objective columns this indicates there is no effect for this particular comparison. Also returns alpha-beta compromise plots for all comparisons. Note: Cohen's f = sqrt(pes/1-pes) and the noncentrality parameter is = f^2*df(error)
#' 
#' \describe{
#'   \item{\code{"aov_comp"}}{A dataframe of ANOVA-level results.}
#'   \item{\code{"aov_plotlist"}}{List of plots for ANOVA-level effects}
#'   \item{\code{"manova_comp"}}{A dataframe of MANOVA-level results.}
#'   \item{\code{"manova_plotlist"}}{List of plots for MANOVA-level effects.}
#'   \item{\code{"emmeans_comp"}}{A dataframe of ANOVA-level results.}
#'   \item{\code{"emm_plotlist"}}{List of plots for estimated marginal means contrasts.}
#' 
#' }
#' alpha = alpha or Type 1 error that minimizes or balances combined error rates
#' beta = beta or Type 2 error that minimizes or balances combined error rates
#' objective = value that is the result of the minimization, either 0 (for balance) or the combined weighted error rates
#'
#' @examples
#' \dontrun{
#' design_result <- ANOVA_design(design = "3b*2w",
#' n = 6,
#' mu = c(1, 2, 2, 3, 3, 4),
#' sd = 3,
#' plot = FALSE)
#' example = ANOVA_compromise(design_result,emm = TRUE,emm_comp = "a")
#' }
#' @section References:
#' too be added
#' @importFrom stats optimize
#' @import emmeans
#' @import ggplot2
#' @export
#'


ANOVA_compromise <- function(design_result,
                             correction = Superpower_options("correction"), 
                             emm = Superpower_options("emm"),
                             emm_model = Superpower_options("emm_model"),
                             contrast_type = Superpower_options("contrast_type"),
                             emm_comp,
                             costT1T2 = 1, 
                             priorH1H0 = 1, 
                             error = "minimal",
                             liberal_lambda = Superpower_options("liberal_lambda")){
 

    if (missing(emm_comp)) {
      emm_comp = as.character(design_result$frml2)[2]
    }

  
  x1 = ANOVA_exact2(design_result,
                    correction = correction,
                    emm = emm,
                    emm_model = emm_model,
                    emm_comp = emm_comp,
                    verbose = FALSE)
  
  
  aov_comp = data.frame(effect = rownames(x1$main_results),
                        cohen_f = x1$main_results$cohen_f,
                        num_df = x1$anova_table$num_df,
                        den_df = x1$anova_table$den_df,
                        alpha = NA,
                        beta = NA,
                        objective = NA)

  if(any(is.na(aov_comp$cohen_f)) ||
     any(is.na(aov_comp$num_df)) ||
     any(is.na(aov_comp$den_df)) ||
     any(is.na(aov_comp$effect))) {
    stop("Missing aov_comp")
  }
  
  aov_plotlist = list()
  
  for (i in 1:nrow(x1$main_results)) {
    if(x1$main_results[i,]$partial_eta_squared >  1e-11){
      run_func = "power.ftest(num_df = x_num_df,den_df = x_den_df,cohen_f = x_cohen_f,alpha_level = x,liberal_lambda = x_lamb)$power/100"
      run_func2 = gsub("x_num_df",
                       aov_comp$num_df[i],
                       run_func)
      run_func2 = gsub("x_den_df",
                       aov_comp$den_df[i],
                       run_func2)
      run_func2 = gsub("x_cohen_f",
                       aov_comp$cohen_f[i],
                       run_func2)
      run_func2 = gsub("x_lamb",
                       liberal_lambda,
                       run_func2)


      alpha_res = optimal_alpha(power_function = run_func2,
                                costT1T2 = costT1T2,
                                priorH1H0 = priorH1H0,
                                error = error,
                                plot = FALSE)
      aov_comp[i,]$alpha = alpha_res$alpha
      aov_comp[i,]$beta = alpha_res$beta
      aov_comp[i,]$objective = alpha_res$objective
      aov_name = aov_comp[i,]$effect
      alpha_res$plot = alpha_res$plot + ggtitle(aov_name)
      aov_plotlist[[aov_name]] = alpha_res$plot
    } 
  }
  
  if (!is.null(x1$manova_results)) {
    manova_comp = data.frame(effect = rownames(x1$manova_results),
                             cohen_f = x1$manova_results$cohen_f,
                             num_df = x1$manova_table$num_df,
                             den_df = x1$manova_table$den_df,
                             alpha = NA,
                             beta = NA,
                             objective = NA)
    

    manova_plotlist = list()
    
    for (i in 1:nrow(manova_comp)) {
      
      if (x1$manova_table[i,]$p.value <  1) {
        
        run_func = "power.ftest(num_df = x_num_df,den_df = x_den_df,cohen_f = x_cohen_f,alpha_level = x,liberal_lambda=x_lamb)$power/100"
        run_func2 = gsub("x_num_df",
                         manova_comp$num_df[i],
                         run_func)
        run_func2 = gsub("x_den_df",
                         manova_comp$den_df[i],
                         run_func2)
        run_func2 = gsub("x_cohen_f",
                         manova_comp$cohen_f[i],
                         run_func2)
        run_func2 = gsub("x_lamb",
                         liberal_lambda,
                         run_func2)
        alpha_res = optimal_alpha(
          power_function = run_func2,
          costT1T2 = costT1T2,
          priorH1H0 = priorH1H0,
          error = error,
          plot = FALSE
        )
        manova_comp[i,]$alpha = alpha_res$alpha
        manova_comp[i,]$beta = alpha_res$beta
        manova_comp[i,]$objective = alpha_res$objective
        manova_name = manova_comp[i,]$effect
        alpha_res$plot = alpha_res$plot + ggtitle(manova_name)
        manova_plotlist[[manova_name]] = alpha_res$plot
      } 
    }
    manova_comp = data.frame(effect = manova_comp$effect,
                             cohen_f = manova_comp$cohen_f,
                             num_df = manova_comp$num_df,
                             den_df = manova_comp$den_df,
                             alpha = manova_comp$alpha,
                             beta = manova_comp$beta,
                             objective = manova_comp$objective)
    
  } else {
    manova_comp = NULL
    manova_plotlist = NULL
  }
  
  if (!is.null(x1$emm_results)) {
    emmeans_comp = data.frame(effect = x1$emm_results$contrast,
                          cohen_f = x1$emm_results$cohen_f,
                          num_df = 1,
                          den_df = x1$emmeans_table$df,
                          alpha = NA,
                          beta = NA,
                          objective = NA)

    
    emm_plotlist = list()
    
    for (i in 1:nrow(x1$emm_results)) {
      if (x1$emmeans_table[i,]$p.value <  1) {
        run_func = "power.ftest(num_df = x_num_df,den_df = x_den_df,cohen_f = x_cohen_f,alpha_level = x,liberal_lambda=x_lamb)$power/100"
        run_func2 = gsub("x_num_df",
                         emmeans_comp$num_df[i],
                         run_func)
        run_func2 = gsub("x_den_df",
                         emmeans_comp$den_df[i],
                         run_func2)
        run_func2 = gsub("x_cohen_f",
                         emmeans_comp$cohen_f[i],
                         run_func2)
        run_func2 = gsub("x_lamb",
                         liberal_lambda,
                         run_func2)
        alpha_res = optimal_alpha(
          power_function = run_func2,
          costT1T2 = costT1T2,
          priorH1H0 = priorH1H0,
          error = error,
          plot = FALSE
        )
        emmeans_comp[i,]$alpha = alpha_res$alpha
        emmeans_comp[i,]$beta = alpha_res$beta
        emmeans_comp[i,]$objective = alpha_res$objective
        emm_name = emmeans_comp[i,]$effect
        alpha_res$plot = alpha_res$plot + ggtitle(emm_name)
        emm_plotlist[[emm_name]] = alpha_res$plot
      } 
    }
    emmeans_comp = data.frame(effect = emmeans_comp$effect,
                             cohen_f = emmeans_comp$cohen_f,
                             num_df = emmeans_comp$num_df,
                             den_df = emmeans_comp$den_df,
                             alpha = emmeans_comp$alpha,
                             beta = emmeans_comp$beta,
                             objective = emmeans_comp$objective)
    
  } else {
    emmeans_comp = NULL
    emm_plotlist = NULL
  }
  
  # S3 method
  #class(compromise_ANOVA) <- "compromise_ANOVA"
  #attr(compromise_ANOVA, aov_comp)
  
  structure(list(aov_comp = aov_comp,
                 aov_plotlist = aov_plotlist,
                 manova_comp = manova_comp,
                 manova_plotlist = manova_plotlist,
                 emmeans_comp = emmeans_comp,
                 emm_plotlist = emm_plotlist,
                 method = "ANOVA_compromise"
                 ),
            class = "opt_alpha")
  
}
