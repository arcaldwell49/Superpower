#' Analytic power calculation for 2x2 within designs (needs updating).
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance
#' @return
#' mu = means
#'
#' sigma = standard deviation
#'
#' n = sample size
#'
#' rho_A = correlation between dependent factors for effect A
#'
#' rho_B = correlation between dependent factors for effect B
#'
#' rho_AB = correlation between dependent factors for A*B interaction
#'
#' alpha_level = alpha level
#'
#' Cohen_f_A = Cohen's f for main effect A
#'
#' Cohen_f_B = Cohen's f for main effect B
#'
#' Cohen_f_AB = Cohen's f for the A*B interaction
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
#' mean_mat = matrix of the means
#'
#' @examples
#' design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0),
#'      sd = 2, r = 0.8, labelnames = c("condition", "cheerful", "sad",
#'      "voice", "human", "robot"))
#' power_result <- power_2x2_within(design_result)
#' @section References:
#' to be added
#' @importFrom stats pf qf
#' @export
#'
power_2x2_within <- function(design_result, alpha_level = 0.05){

  #Error message if design other than 2x2 within is input
  if(length(design_result$design_factors) != 2 | any(design_result$design_factors != 1) ){
    stop("Only 2x2 within designs allowed for this function")
  }

  mu <- design_result$mu
  m_A <- length(design_result$labelnames[[1]])
  m_B <- length(design_result$labelnames[[2]])
  sigma <- design_result$sd
  n <- design_result$n
  rho_A <- design_result$cor_mat[1,3]
  rho_B  <- design_result$cor_mat[1,2]
  rho_AB  <- design_result$cor_mat[1,4]

  mean_mat <- t(matrix(mu,
                       nrow = 2,
                       ncol = 2)) #Create a mean matrix
  rownames(mean_mat) <- c("a1", "a2")
  colnames(mean_mat) <- c("b1", "b2")

  a1 <- mean_mat[1,1] - (mean(mean_mat) + (mean(mean_mat[1,]) - mean(mean_mat)) + (mean(mean_mat[,1]) - mean(mean_mat)))
  a2 <- mean_mat[1,2] - (mean(mean_mat) + (mean(mean_mat[1,]) - mean(mean_mat)) + (mean(mean_mat[,2]) - mean(mean_mat)))
  b1 <- mean_mat[2,1] - (mean(mean_mat) + (mean(mean_mat[2,]) - mean(mean_mat)) + (mean(mean_mat[,1]) - mean(mean_mat)))
  b2 <- mean_mat[2,2] - (mean(mean_mat) + (mean(mean_mat[2,]) - mean(mean_mat)) + (mean(mean_mat[,2]) - mean(mean_mat)))

  k <- 1 #one group (because all factors are within)

  variance_A <- sigma^2 * (1 - rho_A) + sigma^2 * (m_A - 1) * (rho_B - rho_AB) #Variance A

  variance_B <- sigma^2 * (1 - rho_B) + sigma^2 * (m_B - 1) * (rho_A - rho_AB) #Variance B

  variance_AB <- sigma^2 * (1 - max(rho_A, rho_B)) - sigma^2 * (min(rho_A, rho_B) - rho_AB) #Variance AB


  # For main effect A
  f_A <- sqrt(sum((rowMeans(mean_mat)-mean(rowMeans(mean_mat)))^2))/sigma
  lambda_A <- n * m_A * sum((rowMeans(mean_mat)-mean(rowMeans(mean_mat)))^2)/variance_A
  df1 <- (m_A - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df2 <- (n - k) * (m_A - 1) #calculate degrees of freedom 2
  F_critical_A <- qf(alpha_level, # critical F-vaue
                   df1,
                   df2,
                   lower.tail=FALSE)

  power_A <- pf(qf(alpha_level, #powerer
                 df1,
                 df2,
                 lower.tail = FALSE),
              df1,
              df2,
              lambda_A,
              lower.tail = FALSE)

  # For main effect B
  f_B <- sqrt(sum((colMeans(mean_mat)-mean(colMeans(mean_mat)))^2))/sigma
  lambda_B <- n * m_B * sum((colMeans(mean_mat)-mean(colMeans(mean_mat)))^2)/variance_B
  df1 <- (m_B - 1) #calculate degrees of freedom 1
  df2 <- (n - k) * (m_B - 1) #calculate degrees of freedom 2
  F_critical_B <- qf(alpha_level, # critical F-vaue
                   df1,
                   df2,
                   lower.tail=FALSE)

  power_B <- pf(qf(alpha_level, #powerer
                 df1,
                 df2,
                 lower.tail = FALSE),
              df1,
              df2,
              lambda_B,
              lower.tail = FALSE)

  # For the interaction
  f_AB <- sqrt(sum(c(a1, a2, b1, b2)^2)/length(mu))/sigma #based on G*powerer manual page 28
  lambda_AB <- n * sqrt(sum(c(a1, a2, b1, b2)^2)/length(mu))/ variance_AB
  df1 <- (m_A - 1)*(m_B - 1)  #calculate degrees of freedom 1
  df2 <- (n - k) * (m_A - 1) * (m_B - 1) #calculate degrees of freedom 2
  F_critical_AB <- qf(alpha_level, # critical F-vaue
                   df1,
                   df2,
                   lower.tail=FALSE)

  power_AB <- pf(qf(alpha_level, #powerer
                  df1,
                  df2,
                  lower.tail = FALSE),
               df1,
               df2,
               lambda_AB,
               lower.tail = FALSE)
  invisible(list(mu = mu,
                 sigma = sigma,
                 n = n,
                 rho_A = rho_A,
                 rho_B = rho_B,
                 rho_AB = rho_AB,
                 alpha_level = alpha_level,
                 Cohen_f_A = f_A,
                 Cohen_f_B = f_B,
                 Cohen_f_AB = f_AB,
                 lambda_A = lambda_A,
                 lambda_B = lambda_B,
                 lambda_AB = lambda_AB,
                 F_critical_A = F_critical_A,
                 F_critical_B = F_critical_B,
                 F_critical_AB = F_critical_AB,
                 power_A = power_A,
                 power_B = power_B,
                 power_AB = power_AB,
                 mean_mat = mean_mat))
}
