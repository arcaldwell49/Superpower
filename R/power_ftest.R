#' Power Calculations for an F-test
#' 
#' Compute power of test or determine parameters to obtain target power. Inspired by the pwr.f2.test function in the pwr package, but allows for varying noncentrality parameter estimates for a more liberal (default in pwr.f2.test) or conservative (default in this function) estimates (see Aberson, Chapter 5, pg 72).
#' 
#' @param num_df degrees of freedom for numerator
#' @param den_df degrees of freedom for denominator
#' @param cohen_f Cohen's f effect size. Note: this is the sqrt(f2) if you are used to using pwr.f2.test
#' @param alpha_level Alpha level used to determine statistical significance. 
#' @param beta_level Type II error probability (power/100-1)
#' @param liberal_lambda Logical indicator of whether to use the liberal (cohen_f^2\*(num_df+den_df)) or conservative (cohen_f^2\*den_df) calculation of the noncentrality (lambda) parameter estimate. Default is FALSE.
#' 
#' @return
#' num_df = degrees of freedom for numerator, 
#' den_df = degrees of freedom for denominator, 
#' cohen_f = Cohen's f effect size, 
#' alpha_level = Type 1 error probability, 
#' beta_level = Type 2 error probability,
#' power = Power of test (1-beta_level\*100%), 
#' lambda = Noncentrality parameter estimate (default = cohen_f^2\*den_df, liberal = cohen_f^2\*(num_df+den_df))
#'
#' @examples
#' design_result <- ANOVA_design(design = "2b",
#' n = 65,
#' mu = c(0,.5),
#' sd = 1,
#' plot = FALSE)
#' x1 = ANOVA_exact2(design_result, verbose = FALSE)
#' ex = power.ftest(num_df = x1$anova_table$num_df, 
#' den_df = x1$anova_table$den_df, 
#' cohen_f = x1$main_result$cohen_f,
#' alpha_level = 0.05,
#' liberal_lambda = FALSE)
#' @section References:
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.
#' Aberson, C. (2019). Applied Power Analysis for the Behavioral Sciences (2nd ed.). New York,NY: Routledge.
#' @importFrom stats uniroot optimize
#' @export
#'

power.ftest <- function(num_df = NULL, den_df = NULL,
                        cohen_f = NULL,
                        alpha_level = Superpower_options("alpha_level"),
                        beta_level = NULL,
                        liberal_lambda = Superpower_options("liberal_lambda")) 
{
  
  #if (sum(sapply(list(num_df, den_df, cohen_f, beta_level, alpha_level), is.null)) != 
  #    1) {
  #  stop("exactly one of num_df, den_df, cohen_f, beta_level, and alpha_level must be NULL")
  #}
  if (!is.null(cohen_f)) {
    if (any(cohen_f < 0)) {
      stop("cohen_f must be positive")
    }
  }
  if (!is.null(num_df) && any(num_df < 1)) {
    stop("degree of freedom num_df for numerator must be at least 1")
  }
    
  if (!is.null(den_df) && any(den_df < 1)) {
    stop("degree of freedom den_df for denominator must be at least 1")
  }
  if (!is.null(alpha_level) && !is.numeric(alpha_level) || any(0 > 
                                                           alpha_level | alpha_level > 1)) {
    stop(sQuote("alpha_level"), " must be numeric in [0, 1]")
  }
    
  if (!is.null(beta_level) && !is.numeric(beta_level) || any(0 > beta_level | 
                                                   beta_level > 1)) {
    stop(sQuote("beta_level"), " must be numeric in [0, 1].")
  } 
    
  if (liberal_lambda == TRUE) {
  
  p.body <- quote({
    pf(qf(alpha_level, num_df, den_df, lower.tail =  FALSE), num_df, den_df, cohen_f^2 * (num_df+den_df+1), 
       lower.tail = FALSE)
  })
  } else {
    
    p.body <- quote({
      pf(qf(alpha_level, num_df, den_df, lower.tail =  FALSE), num_df, den_df, cohen_f^2 * (den_df), 
         lower.tail = FALSE)
    })
  }
  
  if (!is.null(beta_level)){
    pow = 1 - beta_level
  } 
  
  if (is.null(beta_level)){
    pow <- eval(p.body)
  } else if (is.null(num_df)) {
    p.body2 = p.body[2]
    p.body2 = gsub("alpha_level",
                   alpha_level,
                   p.body2)
    p.body2 = gsub("den_df",
                   den_df,
                   p.body2)
    p.body2 = gsub("cohen_f",
                   cohen_f,
                   p.body2)
    
    num_df = optimize(f = function(num_df) {
      abs(eval(parse(text=paste(p.body2)))-pow)
    }, c(0,1000))$min

    #num_df <- uniroot(function(num_df) eval(p.body) - pow, c(1, 100))$root
  }
  else if (is.null(den_df)) {
  
    den_df <- uniroot(function(den_df) eval(p.body) - pow, c(1 + 
                                                       1e-10, 1e+09))$root
  }
  else if (is.null(cohen_f)) {
    cohen_f <- uniroot(function(cohen_f) eval(p.body) - pow, c(1e-07, 
                                                       1e+07))$root
  }
  else if (is.null(alpha_level)) {
    alpha_level <- uniroot(function(alpha_level) eval(p.body) - 
                           pow, c(1e-10, 1 - 1e-10))$root
  }
  else {
    stop("internal error: exactly one of num_df, den_df, cohen_f, beta_level, and alpha_level must be NULL")
  }
  
  power_final = pow * 100
  beta_level = 1 - pow
  METHOD <- "Power Calculation for F-test"
  structure(list(num_df = num_df, 
                 den_df = den_df, 
                 cohen_f = cohen_f, 
                 alpha_level = alpha_level, 
                 beta_level = beta_level,
                 power = power_final, 
                 method = METHOD), class = "power.htest")
}
