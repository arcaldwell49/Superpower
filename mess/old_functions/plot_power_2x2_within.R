#' Convenience function to plot power across a range of sample sizes.
#' @param design_result Output from the ANOVA_design function
#' @param max_n Maximum sample size in power curve.
#' @return Returns plots with power curves for the effects, and a dataframe with the summary data.
#' @examples
#' design_result <- ANOVA_design(design = "2w*2w",
#'                              n = 20,
#'                              mu = c(0,0,0,0.3),
#'                              sd = 1,
#'                              r = c(
#'                                0.9, 0.4, 0.4,
#'                                0.4, 0.4,
#'                                0.9),
#'                              labelnames = c("condition", "cheerful", "sad",
#'      "voice", "human", "robot"))
#'
#' plot_power_2x2_within(design_result, max_n = 30)
#' @section References:
#' to be added
#' @import ggplot2
#' @export
#'
plot_power_2x2_within <- function(design_result, max_n){

  design = design_result$design
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r
  labelnames = c("A", "a1", "a2", "B", "b1", "b2")

  n_vec <- seq(from = 5, to = max_n)

  power_A <- numeric(length(n_vec))
  power_B <- numeric(length(n_vec))
  power_AB <- numeric(length(n_vec))

  for (i in 1:length(n_vec)){
    design_result <- ANOVA_design(design = design,
                                  n = n_vec[i],
                                  mu = mu,
                                  sd = sd,
                                  r = r,
                                  labelnames = labelnames)

    power_res <- power_2x2_within(design_result)

    power_A[i] <- power_res$power_A*100
    power_B[i] <- power_res$power_B*100
    power_AB[i] <- power_res$power_AB*100
  }

  res_df <- data.frame(n_vec, power_A, power_B, power_AB)

  p1 <- ggplot(data=res_df, aes(x = n_vec, y = power_A)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor A")

  p2 <- ggplot(data=res_df, aes(x = n_vec, y = power_AB)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor B")

  p3 <- ggplot(data=res_df, aes(x = n_vec, y = power_AB)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor AB")

  invisible(list(p1 = p1,
                 p2 = p2,
                 p3 = p3,
                 power_df = data.frame(paste("f = ",
                                             round(power_res$Cohen_f_A,2),
                                             " ",
                                             round(power_res$Cohen_f_B,2),
                                             " ",
                                             round(power_res$Cohen_f_AB,2),
                                             "\n",
                                             "r = ",
                                             power_res$rho_A,
                                             " ",
                                             power_res$rho_B,
                                             " ",
                                             power_res$rho_AB),
                                       n_vec,
                                       power_A,
                                       power_B,
                                       power_AB)))
}
