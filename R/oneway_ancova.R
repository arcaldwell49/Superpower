#' Power Calculations for a one-way ANCOVA
#' 
#' Compute power of ANCOVA omnibus test (power_oneway_ancova) or contrast (power_oneway_ancova) for one-way (single factor), between subjects designs.
#' 
#' @param n Sample size in each condition.
#' @param mu Vector specifying mean for each condition.
#' @param n_cov Number of covariates.
#' @param r2 Coefficient of determination (r^2) of the combined covariates.
#' @param sd Standard deviation for all conditions (residual SD without covariate adjustment).
#' @param alpha_level Alpha level used to determine statistical significance. 
#' @param beta_level Type II error probability (power/100-1)
#' @param round_up Logical indicator for whether to round up the sample size(s) to a whole number. Default is TRUE.
#' @param type Sets the method for estimating power. "exact" will use the Shieh (2020) approach while "approx" will use the Keppel (1991) approach.
#' 
#' @return
#' dfs = degrees of freedom, 
#' N = Total sample size,
#' n = Sample size per group/condition,
#' n_cov = Number of covariates, 
#' mu = Mean for each condition,
#' sd = Standard deviation,
#' r2 = Coefficient of determination of combined covariates.
#' alpha_level = Type 1 error probability, 
#' beta_level = Type 2 error probability,
#' power = Power of test (1-beta_level\*100%), 
#' type = Method (Shieh or Keppel) for estimating power
#'
#' @examples
#' # Example from Table 1 Shieh 2020
#' power_oneway_ancova(mu = c(400, 450, 500), n = c(21,21,21),
#' r2 = .1^2, sd = 100)
#' @section References:
#' Keppel, G. (1991). Design and Analysis A Researcher's Handbook. 3rd Edition. Prentice Hall. Englewood Cliffs, New Jersey. See pages 323 - 324. 
#' Shieh, G. (2017). Power and sample size calculations for contrast analysis in ANCOVA. Multivariate behavioral research, 52(1), 1-11.
#' Shieh, G. (2020). Power analysis and sample size planning in ANCOVA designs. Psychometrika, 85(1), 101-120.
#' @importFrom stats uniroot pf df qf contr.sum dt dbeta qtukey optim
#' @export
#'


power_oneway_ancova <- function(n = NULL,
                                mu = NULL,
                                n_cov = 1,
                                r2 = NULL,
                                sd = 1,
                                alpha_level = Superpower_options('alpha_level'),
                                beta_level = NULL,
                                round_up = TRUE,
                                type = "exact"){
  
  if (!is.null(alpha_level) && !is.numeric(alpha_level) || any(0 > 
                                                               alpha_level | alpha_level > 1)) {
    stop(sQuote("alpha_level"), " must be numeric in [0, 1]")
  }
  
  if (!is.null(beta_level) && !is.numeric(beta_level) || any(0 > beta_level | 
                                                             beta_level > 1)) {
    stop(sQuote("beta_level"), " must be numeric in [0, 1].")
  } 
  
  if(is.null(mu) || length(mu) <= 1){
    stop("mu cannot be NULL or have length of 1. Please provide group means.")
  }
  
  if(!is.null(n) && length(n) != length(mu)){
    stop("length of n and mu must match.")
  }
  
  if(length(sd) > 1){
    stop("sd can only have length of 1.")
  }
  
  if (!is.null(beta_level)){
    pow = 1 - beta_level
  } 
  #var_e = sd^1*(1-r2)
  
  if(type != "exact" && type != "approx"){
    stop("type of power calculation can only be exact or approx")
  }
  
  
  if(is.null(n_cov) || !is.numeric(n_cov) || length(n_cov) > 1) {
    stop("Please provide number of covariates (n_cov).")
  }
  
  if(!is.null(r2) && (r2 >= 1 || r2 <= 0)){
    stop(sQuote("r2"), " must be numeric in [0, 1]")
  }
  
  if(!is.null(n)) {
    N_tot = sum(n)
  } else {
    N_tot = NULL
  }
  
  #internal error: exactly one of n, r2, beta_level, and alpha_level must be NULL
  if(sum(c(is.null(n),
           is.null(r2),
           is.null(beta_level),
           is.null(alpha_level))) != 1){
    stop("internal error: exactly one of n, r2, beta_level, and alpha_level must be NULL")
  }
  
  num_df = length(mu) - 1
  
  
  p.body <- quote({
    pwr_method(mu,
               n,
               n_cov,
               r2,
               sd,
               alpha_level,
               type)
  })
  
  if (!is.null(beta_level)){
    pow = 1 - beta_level
  } 
  
  if (is.null(beta_level)){
    pow <- eval(p.body)
  } else if(is.null(r2)) {
    r2 <- uniroot(function(r2) eval(p.body) - 
                    pow, c(1e-10, 1 - 1e-10))$root
  } else if (is.null(alpha_level)) {
    alpha_level <- uniroot(function(alpha_level) eval(p.body) - 
                             pow, c(1e-10, 1 - 1e-10))$root
  } else if (is.null(n)){
    
    p.body2 <- quote({
      pwr_method(mu,
                 n = rep(N_tot/length(mu), length(mu)),
                 n_cov,
                 r2,
                 sd,
                 alpha_level,
                 type)
    })
    
    
    N_tot <- optim(par=(2*length(mu)+n_cov),
                   fn=function(N_tot){ abs(eval(p.body2) - pow)}, 
                   c(2*length(mu)+n_cov, 1000000000), 
                     control=list(warn.1d.NelderMead = FALSE))$par
    n_1 = N_tot / length(mu)
    n = rep(n_1, length(mu))
  }else {
    stop("internal error: exactly one of n, r2, beta_level, and alpha_level must be NULL")
  }
  
  if(round_up == TRUE && (!(N_tot%%1==0) || !any(n%%1==0))) {
    N_tot = length(mu)*ceiling(N_tot / length(mu))
    n_1 = N_tot / length(mu)
    n = rep(n_1, length(mu))
    
    pow = pwr_method(mu = mu,
                     n = n,
                     n_cov = n_cov,
                     r2 = r2,
                     sd = sd,
                     alpha_level = alpha_level,
                     type = type)
    beta_level = 1-pow
    
  }

  
  den_df = N_tot - length(mu) - n_cov
  #sd_m = sqrt(anc_var_m(n,mu))
  #sd_e = sqrt((sd^2*(1-r2)))
  power_final = pow * 100
  beta_level = 1 - pow
  METHOD <- "Power Calculation for 1-way ANCOVA"
  TYPE = type
  structure(list(dfs = c(num_df, den_df), 
                 N = N_tot, 
                 n = n,
                 n_cov = n_cov,
                 mu = mu,
                 sd = sd,
                 r2 = r2,
                 alpha_level = alpha_level, 
                 beta_level = beta_level,
                 power = power_final,
                 type = TYPE,
                 method = METHOD), class = "power.htest")
}
