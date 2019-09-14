#' Analytic power calculation for two-way between designs.
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
#' Cohen_f_A = Cohen's f for main effect A
#'
#' Cohen_f_B = Cohen's f for main effect B
#'
#' Cohen_f_AB = Cohen's f for the A*B interaction
#'
#' f_2_A = Cohen's f squared for main effect A
#'
#' f_2_B = Cohen's f squared for main effect B
#'
#' f_2_AB = Cohen's f squared for A*B interaction
#'
#' lambda_A = lambda for main effect A
#'
#' lambda_B = lambda for main effect B
#'
#' lambda_AB = lambda for A*B interaction
#'
#' critical_F_A = critical F-value for main effect A
#'
#' critical_F_B = critical F-value for main effect B
#'
#' critical_F_AB = critical F-value for A*B interaction
#'
#' power_A = power for main effect A
#'
#' power_B = power for main effect B
#'
#' power_AB = power for A*B interaction
#'
#' df_A = degrees of freedom for main effect A
#'
#' df_B = degrees of freedom for main effect B
#'
#' df_AB = degrees of freedom for A*B interaction
#'
#' df_error = degrees of freedom for error term
#'
#' eta_p_2_A = partial eta-squared for main effect A
#'
#' eta_p_2_B = partial eta-squared for main effect B
#'
#' eta_p_2_AB = partial eta-squared for A*B interaction
#'
#' mean_mat = matrix of the means
#'
#' @examples
#' design_result <- ANOVA_design(design = "2b*2b", n = 40, mu = c(1, 0, 1, 0),
#'      sd = 2, labelnames = c("condition", "cheerful", "sad",
#'      "voice", "human", "robot"))
#' power_result <- power_twoway_between(design_result, alpha_level = 0.05)
#' @section References:
#' too be added
#' @importFrom stats pf qf
#' @export
#'
power_twoway_between <- function(design_result, alpha_level=0.05){

  #Error message if design other than 1-way between is input
  if(any(design_result$design_factors != 0) | length(design_result$design_factors) != 2 ){
    stop("Only two-way between designs allowed for this function")
  }

  mean_mat <- t(matrix(design_result$mu,
                       nrow = length(design_result$mu)/length(design_result$labelnameslist[[2]]),
                       ncol = length(design_result$labelnameslist[[1]]))) #Create a mean matrix
  colnames(mean_mat) <- design_result$labelnameslist[[1]]
  rownames(mean_mat) <- design_result$labelnameslist[[2]]

  mean_mat_AB <- mean_mat - (mean(mean_mat) + sweep(mean_mat,1, rowMeans(mean_mat)) + sweep(mean_mat,2, colMeans(mean_mat)))

  MS_A <- design_result$n * length(design_result$labelnameslist[[1]]) * (sum((colMeans(mean_mat) - mean(mean_mat))^2)/(length(design_result$labelnameslist[[1]])-1))
  SS_A <- design_result$n * length(design_result$labelnameslist[[1]]) * sum((colMeans(mean_mat) - mean(mean_mat))^2)

  MS_B <- design_result$n * length(design_result$labelnameslist[[2]]) * (sum((rowMeans(mean_mat) - mean(mean_mat))^2)/(length(design_result$labelnameslist[[2]])-1))
  SS_B <- design_result$n * length(design_result$labelnameslist[[2]]) * sum((rowMeans(mean_mat) - mean(mean_mat))^2)

  MS_AB <- design_result$n * sum(mean_mat_AB^2)/((length(design_result$labelnameslist[[1]])-1) * (length(design_result$labelnameslist[[2]])-1))
  SS_AB <- design_result$n * sum(mean_mat_AB^2)

  MS_error <- design_result$sd^2
  SS_error <- MS_error * (design_result$n*length(design_result$mu))

  # Power calculations
  df_A <- (length(design_result$labelnameslist[[1]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df_B <- (length(design_result$labelnameslist[[2]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df_AB <- (length(design_result$labelnameslist[[1]])-1) * (length(design_result$labelnameslist[[2]])-1)
  df_error <- (design_result$n*length(design_result$mu) - length(design_result$mu))

  eta_p_2_A <- SS_A/(SS_A+SS_error)
  eta_p_2_B <- SS_B/(SS_B+SS_error)
  eta_p_2_AB <- SS_AB/(SS_AB+SS_error)

  f_2_A <- eta_p_2_A/(1-eta_p_2_A)
  Cohen_f_A <- sqrt(f_2_A)
  f_2_B <- eta_p_2_B/(1-eta_p_2_B)
  Cohen_f_B <- sqrt(f_2_B)
  f_2_AB <- eta_p_2_AB/(1-eta_p_2_AB)
  Cohen_f_AB <- sqrt(f_2_AB)

  lambda_A <- design_result$n * length(design_result$labelnameslist[[1]]) * sum((rowMeans(mean_mat)-mean(rowMeans(mean_mat)))^2)/design_result$sd^2
  lambda_B <- design_result$n * length(design_result$labelnameslist[[2]]) * sum((colMeans(mean_mat)-mean(colMeans(mean_mat)))^2)/design_result$sd^2
  lambda_AB <- design_result$n * length(design_result$labelnameslist[[1]]) * length(design_result$labelnameslist[[2]]) * f_2_AB

  F_critical_A <- qf(alpha_level, df_A, df_error, lower.tail=FALSE)
  power_A <- (pf(F_critical_A, df_A, df_error, lambda_A, lower.tail = FALSE))*100
  F_critical_B <- qf(alpha_level, df_B, df_error, lower.tail=FALSE)
  power_B <- (pf(F_critical_B, df_B, df_error, lambda_B, lower.tail = FALSE))*100
  F_critical_AB <- qf(alpha_level, df_AB, df_error, lower.tail=FALSE)
  power_AB <- (pf(F_critical_AB, df_AB, df_error, lambda_AB, lower.tail = FALSE))*100

  invisible(list(mu = design_result$n,
                 sigma = design_result$sd,
                 n = design_result$n,
                 alpha_level = alpha_level,
                 Cohen_f_A = Cohen_f_A,
                 Cohen_f_B = Cohen_f_B,
                 Cohen_f_AB = Cohen_f_AB,
                 f_2_A = f_2_A,
                 f_2_B = f_2_B,
                 f_2_AB = f_2_AB,
                 lambda_A = lambda_A,
                 lambda_B = lambda_B,
                 lambda_AB = lambda_AB,
                 F_critical_A = F_critical_A,
                 F_critical_B = F_critical_B,
                 F_critical_AB = F_critical_AB,
                 power_A = power_A,
                 power_B = power_B,
                 power_AB = power_AB,
                 df_A = df_A,
                 df_B = df_B,
                 df_AB = df_AB,
                 df_error = df_error,
                 eta_p_2_A = eta_p_2_A,
                 eta_p_2_B = eta_p_2_B,
                 eta_p_2_AB = eta_p_2_AB,
                 mean_mat = mean_mat))
}
