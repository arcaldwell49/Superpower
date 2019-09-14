#' Analytic power calculation for one-way within designs.
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
#' Cohen_f = Cohen's f
#'
#' f_2 = Cohen's f squared
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
#' ## Set up a within design with 3 factors,
#' ## with correlation between observations of 0.8,
#' ## 40 participants (who do all conditions), and standard deviation of 2
#' ## with a mean pattern of 1, 0, 1, conditions labeled 'condition' and
#' ## 'voice', with names for levels of "cheerful", "neutral", "sad".
#' design_result <- ANOVA_design(design = "3w", n = 40, r = 0.8,
#'       mu = c(1, 0, 1), sd = 2,
#'       labelnames = c("condition", "cheerful", "neutral", "sad"))
#' power_result <- power_oneway_within(design_result, alpha_level = 0.05)
#' @section References:
#' too be added
#' @importFrom stats pf qf
#' @export
#'
power_oneway_within <- function(design_result, alpha_level=0.05){

  #Error message if design other than 1-way within is input
  if(length(design_result$design_factors) != 1  | design_result$design_factors != 1){
    stop("Only one-way within designs allowed for this function")
  }

  factor_levels <- as.numeric(strsplit(design_result$design, "\\D+")[[1]])
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
  df2 <- design_result$n*(length(design_result$mu)-1) - (length(design_result$mu)-1)
  eta_p_2 <- SS_A/(SS_A+SS_error)
  f_2 <- eta_p_2/(1-eta_p_2)
  lambda <- (design_result$n * sum(mean_mat_res^2))/(MS_error*(1-design_result$r)) # Formula 2
  # From : Park, I., & Schutz, R. W. (1999). “Quick and Easy” Formulae for Approximating Statistical Power in Repeated Measures ANOVA. Measurement in Physical Education and Exercise Science, 3(4), 249–270. https://doi.org/10.1207/s15327841mpee0304_5
  # The MS_error is reduced (and the lambda increased) by multiplying it by (1-r) (or 1/(1-r))
  Cohen_f <- sqrt(f_2)
  #G*power manual second way to calculate lambda from Table 3:
  u <- length(design_result$mu)/(1-design_result$r)
  lambda_2 <- f_2*u*design_result$n
  # Faul, F., Erdfelder, E., Lang, A.-G., & Buchner, A. (2007). G*Power 3: A flexible statistical power analysis program for the social, behavioral, and biomedical sciences. Behavior Research Methods, 39(2), 175–191. https://doi.org/10.3758/BF03193146

  # In G*power Cohen's f is identical in the within and between design.
  # In SPSS this is not true. In short, SPSS incorporates the correlation in f (and f^2, and partial eta-sqaured)
  # Therefore, from the code above (which equals G*Power) we can calculate the SPSS values.
  f_2_SPSS = f_2 * length(design_result$mu)/(length(design_result$mu)-1) * design_result$n/(design_result$n-1) * 1/(1-design_result$r)
  eta_p_2_SPSS <- f_2_SPSS/(1+f_2_SPSS)
  Cohen_f_SPSS <- sqrt(f_2_SPSS)

  F_critical <- qf(alpha_level, df1, df2, lower.tail=FALSE) # Critical F-Value
  power <- (pf(F_critical, df1, df2, lambda, lower.tail = FALSE))*100 # power

  invisible(list(mu = design_result$n,
                 sigma = design_result$sd,
                 n = design_result$n,
                 alpha_level = alpha_level,
                 Cohen_f = Cohen_f,
                 Cohen_f_SPSS = Cohen_f_SPSS,
                 f_2 = f_2,
                 f_2_SPSS = f_2_SPSS,
                 lambda = lambda,
                 lambda_2 = lambda_2,
                 F_critical = F_critical,
                 power = power,
                 df1 = df1,
                 df2 = df2,
                 eta_p_2 = eta_p_2,
                 eta_p_2_SPSS = eta_p_2_SPSS,
                 mean_mat = mean_mat))
}
