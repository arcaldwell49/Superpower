#' Analytic power calculation for three-way between designs.
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance (default to 0.05)
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
#' Cohen_f_C = Cohen's f for main effect C
#'
#' Cohen_f_AB = Cohen's f for the A*B interaction
#'
#' Cohen_f_AC = Cohen's f for the A*C interaction
#'
#' Cohen_f_BC = Cohen's f for the B*C interaction
#'
#' Cohen_f_ABC = Cohen's f for the A*B*C interaction
#'
#' f_2_A = Cohen's f squared for main effect A
#'
#' f_2_B = Cohen's f squared for main effect B
#'
#' f_2_C = Cohen's f squared for main effect C
#'
#' f_2_AB = Cohen's f squared for A*B interaction
#'
#' f_2_AC = Cohen's f squared for A*C interaction
#'
#' f_2_BC = Cohen's f squared for B*C interaction
#'
#' f_2_ABC = Cohen's f squared for A*B*C interaction
#'
#' lambda_A = lambda for main effect A
#'
#' lambda_B = lambda for main effect B
#'
#' lambda_C = lambda for main effect C
#'
#' lambda_AB = lambda for A*B interaction
#'
#' lambda_AC = lambda for A*C interaction
#'
#' lambda_BC = lambda for B*C interaction
#'
#' lambda_ABC = lambda for A*B*C interaction
#'
#' critical_F_A = critical F-value for main effect A
#'
#' critical_F_B = critical F-value for main effect B
#'
#' critical_F_C = critical F-value for main effect C
#'
#' critical_F_AB = critical F-value for A*B interaction
#'
#' critical_F_AC = critical F-value for A*C interaction
#'
#' critical_F_BC = critical F-value for B*C interaction
#'
#' critical_F_ABC = critical F-value for A*B*C interaction
#'
#' power_A = power for main effect A
#'
#' power_B = power for main effect B
#'
#' power_C = power for main effect C
#'
#' power_AB = power for A*B interaction
#'
#' power_AC = power for A*C interaction
#'
#' power_BC = power for B*C interaction
#'
#' power_ABC = power for A*B*C interaction
#'
#' df_A = degrees of freedom for main effect A
#'
#' df_B = degrees of freedom for main effect B
#'
#' df_C = degrees of freedom for main effect C
#'
#' df_AB = degrees of freedom for A*B interaction
#'
#' df_AC = degrees of freedom for A*C interaction
#'
#' df_BC = degrees of freedom for B*C interaction
#'
#' df_ABC = degrees of freedom for A*B*C interaction
#'
#' df_error = degrees of freedom for error term
#'
#' eta_p_2_A = partial eta-squared for main effect A
#'
#' eta_p_2_B = partial eta-squared for main effect B
#'
#' eta_p_2_C = partial eta-squared for main effect C
#'
#' eta_p_2_AB = partial eta-squared for A*B interaction
#'
#' eta_p_2_AC = partial eta-squared for A*C interaction
#'
#' eta_p_2_BC = partial eta-squared for B*C interaction
#'
#' eta_p_2_ABC = partial eta-squared for A*B*C interaction
#'
#' mean_mat = matrix of the means
#'
#' @examples
#' design_result <- ANOVA_design(design = "2b*2b*2b", n = 40,
#'       mu = c(1, 0, 1, 0, 0, 1, 1, 0), sd = 2,
#'       labelnames = c("condition", "cheerful", "sad",
#'       "voice", "human", "robot", "color", "green", "red"))
#' power_result <- power_threeway_between(design_result, alpha_level = 0.05)
#' @section References:
#' to be added
#' @importFrom stats pf qf
#' @export
#'
power_threeway_between <- function(design_result, alpha_level=0.05){

  #Error message if design other than 1-way between is input
  if(length(design_result$design_factors) != 3 | any(design_result$design_factors != 0)){
    stop("Only three-way between designs allowed for this function")
  }

  mu_array <- array(design_result$mu, dim = c(length(design_result$labelnameslist[[1]]),
                                              length(design_result$labelnameslist[[2]]),
                                              length(design_result$labelnameslist[[3]])))
  #A
  mu_A <- apply(mu_array,c(3),mean)
  mu_A
  #B
  mu_B <- apply(mu_array,c(2),mean)
  mu_B
  #C
  mu_C <- apply(mu_array,c(1),mean)
  mu_C

  #A*B
  mu_AB <- apply(mu_array,c(2,3),mean)
  mu_AB
  mu_AB <- mu_AB - (mean(design_result$mu) + sweep(mu_AB, 1, rowMeans(mu_AB)) + sweep(mu_AB, 2, colMeans(mu_AB)))
  mu_AB

  #A*C
  mu_AC <- apply(mu_array,c(1,3),mean)
  mu_AC
  mu_AC <- mu_AC - (mean(design_result$mu) + sweep(mu_AC, 2, colMeans(mu_AC)) + sweep(mu_AC, 1, rowMeans(mu_AC)))
  mu_AC

  #B*C
  mu_BC <- apply(mu_array,c(1,2),mean)
  mu_BC
  mu_BC <- mu_BC - (mean(design_result$mu) + sweep(mu_BC,1, rowMeans(mu_BC)) + sweep(mu_BC,2, colMeans(mu_BC)))
  mu_BC

  # Calculate degrees of freedom
  df_A <- (length(design_result$labelnameslist[[1]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df_B <- (length(design_result$labelnameslist[[2]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df_C <- (length(design_result$labelnameslist[[3]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction

  df_AB <- (length(design_result$labelnameslist[[1]]) - 1) * (length(design_result$labelnameslist[[2]]) - 1)
  df_AC <- (length(design_result$labelnameslist[[1]]) - 1) * (length(design_result$labelnameslist[[3]]) - 1)
  df_BC <- (length(design_result$labelnameslist[[2]]) - 1) * (length(design_result$labelnameslist[[3]]) - 1)

  df_ABC <- (length(design_result$labelnameslist[[1]]) - 1) * (length(design_result$labelnameslist[[2]]) - 1) * (length(design_result$labelnameslist[[3]]) - 1)

  df_error <- (design_result$n * length(design_result$mu)) - (length(design_result$labelnameslist[[1]])) * (length(design_result$labelnames[[2]])) * (length(design_result$labelnames[[3]]))
  df_total <- df_error + df_A + df_B + df_C + df_AB + df_AC + df_BC + df_ABC

  # Calculate sum of squares
  MS_A <- design_result$n * length(design_result$labelnameslist[[2]]) * length(design_result$labelnameslist[[3]]) * (sum((mu_A - mean(mu_A))^2)/(length(design_result$labelnameslist[[2]])-1))
  SS_A <- design_result$n * length(design_result$labelnameslist[[2]]) * length(design_result$labelnameslist[[3]]) * sum((mu_A - mean(mu_A))^2)

  MS_B <- design_result$n * length(design_result$labelnameslist[[1]]) * length(design_result$labelnameslist[[3]]) * (sum((mu_B - mean(mu_B))^2)/(length(design_result$labelnameslist[[2]])-1))
  SS_B <- design_result$n * length(design_result$labelnameslist[[1]]) * length(design_result$labelnameslist[[3]]) * sum((mu_B - mean(mu_B))^2)

  MS_C <- design_result$n * length(design_result$labelnameslist[[1]]) * length(design_result$labelnameslist[[2]]) * (sum((mu_C - mean(mu_C))^2)/(length(design_result$labelnameslist[[2]])-1))
  SS_C <- design_result$n * length(design_result$labelnameslist[[1]]) * length(design_result$labelnameslist[[2]]) * sum((mu_C - mean(mu_C))^2)

  MS_AB <- design_result$n * length(design_result$labelnameslist[[3]]) * sum(mu_AB^2)/((length(design_result$labelnameslist[[1]])-1) * (length(design_result$labelnameslist[[2]])-1))
  SS_AB <- design_result$n * length(design_result$labelnameslist[[3]]) * sum(mu_AB^2)

  SS_AB_between <- design_result$n * length(design_result$labelnameslist[[3]]) * sum((apply(mu_array,c(2,3),mean) - mean(apply(mu_array,c(2,3),mean)))^2)
  SS_AB_2 <- SS_AB_between - SS_A - SS_B

  MS_AC <- design_result$n * length(design_result$labelnameslist[[2]]) * sum(mu_AC^2)/((length(design_result$labelnameslist[[1]])-1) * (length(design_result$labelnameslist[[3]])-1))
  SS_AC <- design_result$n * length(design_result$labelnameslist[[2]]) * sum(mu_AC^2)

  SS_AC_between <- design_result$n * length(design_result$labelnameslist[[2]]) * sum((apply(mu_array,c(1,3),mean) - mean(apply(mu_array,c(1,3),mean)))^2)
  SS_AC_2 <- SS_AC_between - SS_A - SS_C

  MS_BC <- design_result$n * length(design_result$labelnameslist[[1]]) * sum(mu_BC^2)/((length(design_result$labelnameslist[[2]])-1) * (length(design_result$labelnameslist[[3]])-1))
  SS_BC <- design_result$n * length(design_result$labelnameslist[[1]]) * sum(mu_BC^2)

  SS_BC_between <- design_result$n * length(design_result$labelnameslist[[1]]) * sum((apply(mu_array,c(1,2),mean) - mean(apply(mu_array,c(1,2),mean)))^2)
  SS_BC_2 <- SS_BC_between - SS_B - SS_C

  MS_total <- design_result$sd^2
  SS_total <- MS_total * df_total

  SS_ABC_between <- design_result$n * sum((design_result$mu - mean(design_result$mu))^2)
  SS_ABC <- SS_ABC_between - SS_A - SS_B - SS_C - SS_AB - SS_AC - SS_BC

  SS_error <- SS_total - SS_A - SS_B - SS_C - SS_AB - SS_AC - SS_BC - SS_ABC
  MS_error <- SS_error/df_error

  # Calculate eta-squared
  # Note we are using df_total calculating SS_total. eta_p_2 is  SS_A/(SS_A + SS_total)
  # But ss_total is based on df_total, but we need the total sample size instead.
  eta_p_2_A <- SS_A/(SS_A + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_A
  eta_p_2_B <- SS_B/(SS_B + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_B
  eta_p_2_C <- SS_C/(SS_C + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_C
  eta_p_2_AB <- SS_AB/(SS_AB + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_AB
  eta_p_2_AC <- SS_AC/(SS_AC + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_AC
  eta_p_2_BC <- SS_BC/(SS_BC + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_BC
  eta_p_2_ABC <- SS_ABC/(SS_ABC + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_ABC

  # Cohen f and squared

  f_2_A <- eta_p_2_A/(1-eta_p_2_A)
  f_2_B <- eta_p_2_B/(1-eta_p_2_B)
  f_2_C <- eta_p_2_C/(1-eta_p_2_C)
  f_2_AB <- eta_p_2_AB/(1-eta_p_2_AB)
  f_2_AC <- eta_p_2_AC/(1-eta_p_2_AC)
  f_2_BC <- eta_p_2_BC/(1-eta_p_2_BC)
  f_2_ABC <- eta_p_2_ABC/(1-eta_p_2_ABC)

  Cohen_f_A <- sqrt(f_2_A)
  Cohen_f_B <- sqrt(f_2_B)
  Cohen_f_C <- sqrt(f_2_C)
  Cohen_f_AB <- sqrt(f_2_AB)
  Cohen_f_AC <- sqrt(f_2_AC)
  Cohen_f_BC <- sqrt(f_2_BC)
  Cohen_f_ABC <- sqrt(f_2_ABC)

  # Calculate Lambda
  lambda_A <- design_result$n * length(design_result$mu) * f_2_A
  lambda_B <- design_result$n * length(design_result$mu) * f_2_B
  lambda_C <- design_result$n * length(design_result$mu) * f_2_C

  lambda_AB <- design_result$n * length(design_result$mu) * f_2_AB
  lambda_AC <- design_result$n * length(design_result$mu) * f_2_AC
  lambda_BC <- design_result$n * length(design_result$mu) * f_2_BC

  lambda_ABC <- design_result$n * length(design_result$mu) * f_2_ABC

  # Calculate Critical F

  F_critical_A <- qf(alpha_level, df_A, df_error, lower.tail = FALSE)
  F_critical_B <- qf(alpha_level, df_B, df_error, lower.tail = FALSE)
  F_critical_C <- qf(alpha_level, df_C, df_error, lower.tail = FALSE)
  F_critical_AB <-
    qf(alpha_level, df_AB, df_error, lower.tail = FALSE)
  F_critical_AC <-
    qf(alpha_level, df_AC, df_error, lower.tail = FALSE)
  F_critical_BC <-
    qf(alpha_level, df_BC, df_error, lower.tail = FALSE)
  F_critical_ABC <-
    qf(alpha_level, df_ABC, df_error, lower.tail = FALSE)

  #Calculate Power

  power_A <-
    (pf(F_critical_A, df_A, df_error, lambda_A, lower.tail = FALSE)) * 100
  power_B <-
    (pf(F_critical_B, df_B, df_error, lambda_B, lower.tail = FALSE)) * 100
  power_C <-
    (pf(F_critical_C, df_C, df_error, lambda_C, lower.tail = FALSE)) * 100
  power_AB <-
    (pf(F_critical_AB, df_AB, df_error, lambda_AB, lower.tail = FALSE)) * 100
  power_AC <-
    (pf(F_critical_AC, df_AC, df_error, lambda_AC, lower.tail = FALSE)) * 100
  power_BC <-
    (pf(F_critical_BC, df_BC, df_error, lambda_BC, lower.tail = FALSE)) * 100
  power_ABC <-
    (pf(F_critical_ABC, df_ABC, df_error, lambda_ABC, lower.tail = FALSE)) *
    100

  # F-Value
  # F_A <- MS_A/MS_error
  # F_B <- MS_B/MS_error
  # F_C <- MS_C/MS_error
  # F_AB <- MS_AB/MS_error
  # F_AC <- MS_AC/MS_error
  # F_BC <- MS_BC/MS_error
  # F_ABC <- MS_ABC/MS_error
  #

  invisible(list(mu = design_result$mu,
                 sigma = design_result$sd,
                 n = design_result$n,
                 alpha_level = alpha_level,
                 Cohen_f_A = Cohen_f_A,
                 Cohen_f_B = Cohen_f_B,
                 Cohen_f_C = Cohen_f_C,
                 Cohen_f_AB = Cohen_f_AB,
                 Cohen_f_AC = Cohen_f_AC,
                 Cohen_f_BC = Cohen_f_BC,
                 Cohen_f_ABC = Cohen_f_ABC,
                 f_2_A = f_2_A,
                 f_2_B = f_2_B,
                 f_2_C = f_2_C,
                 f_2_AB = f_2_AB,
                 f_2_AC = f_2_AC,
                 f_2_BC = f_2_BC,
                 f_2_ABC = f_2_ABC,
                 lambda_A = lambda_A,
                 lambda_B = lambda_B,
                 lambda_C = lambda_C,
                 lambda_AB = lambda_AB,
                 lambda_AC = lambda_AC,
                 lambda_BC = lambda_BC,
                 lambda_ABC = lambda_ABC,
                 F_critical_A = F_critical_A,
                 F_critical_B = F_critical_B,
                 F_critical_C = F_critical_C,
                 F_critical_AB = F_critical_AB,
                 F_critical_AC = F_critical_AC,
                 F_critical_BC = F_critical_BC,
                 F_critical_ABC = F_critical_ABC,
                 power_A = power_A,
                 power_B = power_B,
                 power_C = power_C,
                 power_AB = power_AB,
                 power_AC = power_AC,
                 power_BC = power_BC,
                 power_ABC = power_ABC,
                 df_A = df_A,
                 df_B = df_B,
                 df_C = df_C,
                 df_AB = df_AB,
                 df_AC = df_AC,
                 df_BC = df_BC,
                 df_error = df_error,
                 eta_p_2_A = eta_p_2_A,
                 eta_p_2_B = eta_p_2_B,
                 eta_p_2_C = eta_p_2_C,
                 eta_p_2_AB = eta_p_2_AB,
                 eta_p_2_AC = eta_p_2_AC,
                 eta_p_2_BC = eta_p_2_BC,
                 eta_p_2_ABC = eta_p_2_ABC,
                 mean_mat = mu_array))
}

