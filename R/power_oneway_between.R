#' Analytic power calculation for one-way between designs.
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance
#' @return
#' mu = means
#'
#' sigma = standard deviation
#'
#' n = sample size
#'
#' alpha_level = alpha level
#'
#' Cohen_f = Cohen f
#'
#' f_2 = Cohen's f^2
#'
#' lambda = lambda
#'
#' F_critical = Critical F-value
#'
#' power = power
#'
#' df1 = degrees of freedom for the effect
#'
#' df2 = degrees of freedom of the error
#'
#' eta_p_2 = partial eta-squared
#'
#' mean_mat = matrix of the means
#'
#' @examples
#' ## Set up a within design with one factor with 2 levels,
#' ## 40 participants (woh do all conditions), and standard deviation of 2
#' ## with a mean pattern of 1, 0, 1, conditions labeled 'condition'
#' ## with names for levels of "cheerful", "neutral", "sad"
#' design_result <- ANOVA_design(design = "3b", n = 40, mu = c(1, 0, 1),
#'       sd = 2, labelnames = c("condition", "cheerful", "neutral", "sad"))
#' power_result <- power_oneway_between(design_result, alpha_level = 0.05)
#' @section References:
#' too be added
#' @importFrom stats pf qf
#' @export
#'
power_oneway_between <- function(design_result, alpha_level=0.05){

  #Error message if design other than 1-way between is input
  if (length(design_result$factors) != 1 || design_result$design_factors != 0 ) {
    stop("Only one-way between designs allowed for this function")
  }

  mean_mat <- t(matrix(design_result$mu,
                       nrow = length(design_result$mu)/design_result$factors,
                       ncol = design_result$factors)) #Create a mean matrix
  colnames(mean_mat) <- design_result$design_list
  rownames(mean_mat) <- design_result$factornames

  # Using the sweep function to remove rowmeans from the matrix
  mean_mat_res <- sweep(mean_mat,2, rowMeans(mean_mat))
  mean_mat_res
  MS_A <- design_result$n * (sum(mean_mat_res^2)/(length(design_result$mu)-1))
  SS_A <- design_result$n * sum(mean_mat_res^2)
  MS_error <- design_result$sd^2
  SS_error <- MS_error * (design_result$n*length(design_result$mu))
  df1 <- length(design_result$mu)-1
  df2 <- (design_result$n*length(design_result$mu) - length(design_result$mu))
  eta_p_2 <- SS_A/(SS_A+SS_error)
  f_2 <- eta_p_2/(1-eta_p_2)
  lambda <- f_2 * design_result$n * length(design_result$mu)
  # Cohen_f <- sqrt(sum(mean_mat_res^2)/length(design_result$mu))/sd #based on G*power manual page 28
  # We just take the sqrt(f_2) because formula above assumes maximum difference of means.
  Cohen_f <- sqrt(f_2)
  F_critical <- qf(alpha_level, df1, df2, lower.tail=FALSE) # Critical F-Value
  power <- (pf(F_critical, df1, df2, lambda, lower.tail = FALSE))*100 # power

  invisible(list(mu = design_result$mu,
                 sigma = design_result$sd,
                 n = design_result$n,
                 alpha_level = alpha_level,
                 Cohen_f = Cohen_f,
                 f_2 = f_2,
                 lambda = lambda,
                 F_critical = F_critical,
                 power = power,
                 df1 = df1,
                 df2 = df2,
                 eta_p_2 = eta_p_2,
                 mean_mat = mean_mat))
}
