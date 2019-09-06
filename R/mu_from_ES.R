#' Convenience function to calculate the means for between designs with one factor (One-Way ANOVA). Can be used to determine the means that should yield a specified effect sizes (expressed in Cohen's f).
#' @param K Number of groups (2, 3, or 4)
#' @param ES Effect size (eta-squared)
#' @return Returns vector of means
#' @examples
#' ## Medium effect size (eta-squared), 2 groups
#' ES <- 0.0588
#' K <- 2
#' mu_from_ES(K = K, ES = ES)
#' @section References:
#' Albers, C., & Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of Experimental Social Psychology, 74, 187–195. https://doi.org/10.1016/j.jesp.2017.09.004
#' @import ggplot2
#' @export

mu_from_ES <- function(K, ES){ # provides the vector of population means for a given population ES and nr of groups

  if (ES >= 1 | ES <= 0  ) {
    stop("the ES (partial eta squared) must be less than 1 and greater than zero")
  }

  if (K == 2 | K == 3 | K == 4 ){

  } else{stop("Number of levels (k) must be 2, 3, or 4")}

  f2 <- ES/(1-ES)
  if(K == 2){
    a <- sqrt(f2)
    muvec <- c(-a,a)
  }
  if(K == 3){
    a <- sqrt(3*f2/2)
    muvec <- c(-a, 0, a)
  }
  if(K == 4){
    a <- sqrt(f2)
    muvec <- c(-a, -a, a, a)
  } # note: function gives error when K not 2,3,4. But we don't need other K.
  return(muvec)
}
