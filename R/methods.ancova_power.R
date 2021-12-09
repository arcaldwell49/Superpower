#' Methods for ancova_power objects
#' 
#' Methods defined for objects returned from the ANCOVA_analytic function.
#' 
#' @param x object of class \code{ancova_power} as returned from one of the simulation functions in Superpower.
#' @param ... further arguments passed through, see description of return value
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the simulation result}
#'   \item{\code{plot}}{Returns a meansplot of from the defined design}
#' }
#' @name ancova_power-methods


### methods for ancova_power

#' @rdname ancova_power-methods
#' @method print ancova_power
#' @export

print.ancova_power <- function(x,...){
  
  if (!is.null(x$main_results)) {
    pow_tab1 = data.frame(`Total N`= x$main_results$N_tot,
                          `Covariates`= x$main_results$n_cov,
                          `r2`= x$main_results$r2,
                          `Alpha Level`= x$main_results$alpha_level,
                          `Beta Level`= x$main_results$beta_level,
                          `Power` = x$main_results$power,
                          check.names = FALSE)
    row.names(pow_tab1) = x$main_results$factor
    
    cat("Power Analysis Results for ANCOVA")
    cat("\n")
    print(pow_tab1, digits = 4)
    cat("\n")
  }
    if (!is.null(x$contrast_results)) {
      pow_tab2 = data.frame(`Total N`= x$contrast_results$N_tot,
                            `Covariates`= x$contrast_results$n_cov,
                            `r2`= x$contrast_results$r2,
                            `Alpha Level`= x$contrast_results$alpha_level,
                            `Beta Level`= x$contrast_results$beta_level,
                            `Power` = x$contrast_results$power,
                            check.names = FALSE)
      row.names(pow_tab2) = x$contrast_results$factor
      cat("Power Analysis Results for ANCOVA contrasts")
      cat("\n")
      print(pow_tab2, digits = 4)
    }
   
  
}

#' @rdname ancova_power-methods
#' @method plot ancova_power
#' @import ggplot2
#' @export

plot.ancova_power <- function(x,...){
  if(is.null(x$design_params$design)){
    stop("No plotting method when design is not provided.")
  }
  
  plot(ANOVA_design(design = x$design_params$design,
                    n = 40,
                    mu = x$design_params$mu,
                    sd = x$design_params$sd,
                    r = 0,
                    label_list = x$design_params$label_list,
                    plot = FALSE))
  
}

