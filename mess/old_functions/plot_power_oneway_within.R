#' Convenience function to plot power across a range of sample sizes.
#' @param design_result Output from the ANOVA_design function
#' @param max_n Maximum sample size in power curve.
#' @return Returns plots with power curves for the effects, and a dataframe with the summary data.
#' @examples
#' design_result <- ANOVA_design(design = "3w",
#'                              n = 20,
#'                              mu = c(0,0,0.3),
#'                              sd = 1,
#'                              r = 0.7,
#'                              labelnames = c("condition",
#'                              "cheerful", "neutral", "sad"))
#'
#' plot_power_oneway_within(design_result, max_n = 30)
#' @section References:
#' too be added
#' @import ggplot2
#' @export

plot_power_oneway_within <- function(design_result, max_n){



  design = design_result$design
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r
  labelnames = c(design_result$factornames[[1]], design_result$labelnames[[1]])


  n_vec <- seq(from = 5, to = max_n)

  power_A <- numeric(length(n_vec))

  for (i in 1:length(n_vec)){
    design_result <- ANOVA_design(design = design,
                                  n = n_vec[i],
                                  mu = mu,
                                  sd = sd,
                                  r = r,
                                  labelnames = labelnames)

    power_res <- power_oneway_within(design_result)

    power_A[i] <- power_res$power*100
  }

  res_df <- data.frame(n_vec, power_res$power)

  p1 <- ggplot(data=res_df, aes(x = n_vec, y = power_A)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor A")

  invisible(list(p1 = p1,
                 power_df = data.frame(paste("f = ",
                                             round(power_res$Cohen_f,2)),
                                       n_vec,
                                       power_A)))
}
