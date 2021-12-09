#' Power Calculations for ANCOVA Contrasts
#' 
#' Complete power analyses for specific ANCOVA contrasts. This function does not support within subjects factors.
#' 
#' @param cmat Matrix of the specific contrasts of interest
#' @param mu Vector specifying mean for each condition
#' @param n Sample size in each condition
#' @param sd Standard deviation for all conditions (or a vector specifying the sd for each condition)
#' @param r2 Coefficient of Determination of the model with only the covariates
#' @param n_cov Number of covariates
#' @param alpha_level Alpha level used to determine statistical significance
#' @param beta_level Type II error probability (power/100-1)
#' @param round_up Logical indicator (default = TRUE) for whether to round up sample size calculations to nearest whole number
#' 
#' @return
#' Object of class "power.htest", a list of the arguments (including the computed one) augmented with method and note elements.
#' @examples
#' ANCOVA_contrast(cmat = c(-1,1),
#' n = 15,
#' mu = c(0,1),
#' sd = 1,
#' r2 = .2,
#' n_cov = 1)
#' @section References:
#' Shieh, G. (2020). Power analysis and sample size planning in ANCOVA designs. Psychometrika, 85(1), 101-120.
#' @importFrom stats uniroot pf df qf contr.sum dt dbeta qtukey model.matrix terms optim
#' @export

ANCOVA_contrast <- function(cmat,
                            mu,
                            n = NULL,
                            sd,
                            r2 = NULL,
                            n_cov,
                            alpha_level = Superpower_options("alpha_level"),
                            beta_level = NULL,
                            round_up = TRUE) {
  
  METHOD <- "Power Calculation for ANCOVA contrast"
  TYPE = "Exact"
  
  if(is.vector(cmat)){
    cmat = matrix(cmat, nrow=1)
  }
  
  if(ncol(cmat) != length(mu)){
    stop("cmat must match the length of mu")
  }
  
  #Ensure, if single correlation is input, that it is between 0 and 1
  if(!is.null(r2)){
    if ((r2 <= 0) | r2 >= 1 ) {
      stop("Coefficient of Determination must be greater than -1 and less than 1")
    }
  }
  
  #Ensure sd is greater than 0
  if (any(sd <= 0) || length(sd) != 1) {
    stop("Standard deviation (sd) is less than or equal to zero; input a single value greater than zero")
  }
  
  if( missing(mu) ||  missing(sd) || missing(n_cov)){
    stop("mu, sd, and n_cov are missing and must be provided")
  }
  
  n_grp = length(mu)
  
  if(!is.null(n)){
    if(length(n==1)){
      nvec = rep(n,length(mu))
    } else {nvec = n}
    
    if(length(nvec) != length(mu)){
      stop("Length of N and mu do not match!")
    }
  } else {
    nvec = NULL
  }
  
  if (!is.null(beta_level)){
    pow = 1 - beta_level
  } 
  
  p.body = quote({
    pow_anc_meth(cmat,
                 mu,
                 nvec,
                 n_cov,
                 r2,
                 sd,
                 alpha_level)
  })
  
  if (is.null(beta_level)){
        res <- eval(p.body)
        beta_level <- 1-res$pow
        con_res = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )


    
  } else if(is.null(r2)) {
    
    r2 = optim(
      par = .5,
      fn = function(r2) {
        abs(eval(p.body)$pow - pow)
      },
      #c(.001, .999),
      upper = .999,
      lower = .001,
      method = "Brent"
    )$par

        #r2 <- uniroot(function(r2) eval(p.body)$pow - 
         #               pow, c(1e-10, 1 - 1e-10))$root
        res <- eval(p.body)
        beta_level <- 1-res$pow
        con_res = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )

    } else if (is.null(alpha_level)) {
  
        alpha_level <- uniroot(function(alpha_level) eval(p.body)$pow - 
                                 pow, c(1e-10, 1 - 1e-10))$root
        res <- eval(p.body)
        beta_level <- 1-res$pow
        con_res = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )

    
  } else if (is.null(n)){
    
    p.body2 <- quote({
      pow_anc_meth(cmat,
                   mu,
                   nvec = rep(N_tot / length(mu), length(mu)),
                   n_cov,
                   r2,
                   sd,
                   alpha_level)
    })

        N_tot <- optim(par=(2*length(mu)+n_cov),
                       fn=function(N_tot){ abs(eval(p.body2)$pow - pow)}, 
                       c(2*length(mu)+n_cov, 1000000000), 
                       control=list(warn.1d.NelderMead = FALSE))$par
        res <- eval(p.body2)
        beta_level <- 1-res$pow
        if(round_up == TRUE && (!(N_tot%%1==0) || !any(res$nvec%%1==0))) {
          N_tot = length(mu)*ceiling(N_tot / length(mu))
          
          res <- eval(p.body2)
          
        }
        con_res = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )

    
    
  }else {
    stop("internal error: exactly one of n, r2, beta_level, and alpha_level must be NULL")
  }
  
  structure(list(dfs = c(con_res$num_df,
                         con_res$den_df), 
                 N = con_res$N_tot, 
                 n = con_res$nvec,
                 n_cov = con_res$n_cov,
                 contrast = con_res$cmat,
                 mu = con_res$mu,
                 sd = con_res$sd,
                 r2 = con_res$r2,
                 alpha_level = con_res$alpha_level, 
                 beta_level = con_res$beta_level,
                 power = con_res$pow*100,
                 type = TYPE,
                 method = METHOD), 
            class = "power.htest")
}
