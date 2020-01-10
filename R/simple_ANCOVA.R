#' Monte carlo simulation function for simple pre-post parellel group design
#' @param post_diff Difference between groups at the post time (defalt is zero)
#' @param sd standard deviation for all conditions (i.e., common standard deviation; default is 1)
#' @param r Correlation between pre and post measurement (default is .5)
#' @param MOE The margin of error. The desired length of one arm of a CI, or the maximum likely estimation error. If blank, it is set to the sd. (see Cummings and Calin-Jageman)
#' @param min_n Minimum sample size in for the simulations.
#' @param max_n Maximum sample size in for the simulations.
#' @param alpha_level Alpha level used to determine statistical significance
#' @param conf_level The confidence (or compatability) level. Default is 1-alpha_level.
#' @param nsims number of simulations to perform at each sample size. Default is 1000.
#' 
#' \describe{
#'   \item{\code{"sim_results"}}{Power and precision of both statistical tests at the various sample sizes}
#'   \item{\code{"plot_power_ANCOVA"}}{Plot of the power for ANCOVA results}
#'   \item{\code{"plot_power_ANOVA"}}{Plot of the power for ANOVA results}
#'   \item{\code{"plot_precision_ANCOVA"}}{Plot of assurance of the confidence interval for the ANCOVA estimate being within the margin of error}
#'   \item{\code{"plot_precision_ANOVA"}}{Plot of assurance of the confidence interval for the ANOVA estimate being within the margin of error}
#' 
#' }
#' @return Returns dataframe with simulation results at each sample size as well as plots of the confidence intervals widths and power for the ANOVA and ANCOVA result.
#' @examples
#' # Setup a simple 2x2 design with 
#' #.5 difference post with sd = 1.5 and r = .85
#' # Margin of error is at .5 per Cummings recommendations (since post_diff is .5)
#' # Want to check the sample sizes between n = 10 to 20
#' ANCOVA_result = simple_ANCOVA(post_diff = .5, sd = 1.5, r = .85, 
#' min_n = 10, max_n = 20, MOE = .5)
#' @section References:
#' Cumming, Geoff, and Robert Calin-Jageman. Introduction to the new statistics: Estimation, open science, and beyond. Routledge, 2016.
#' @importFrom stats pnorm pt qnorm qt as.formula median p.adjust pf sd power
#' @importFrom utils combn
#' @importFrom graphics pairs
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @import emmeans
#' @import ggplot2
#' @export
#'


simple_ANCOVA <- function(post_diff,sd=1, r = .5,
                          min_n=7, max_n=100, alpha_level=.05,
                          conf_level,
                          nsims = 1000, MOE) {
  
  #Need this to avoid "undefined" global error from occuring
  n_per_group <- power_ANCOVA <- power_ANOVA <- assurance_ANOVA <- assurance_ANCOVA <- NULL
  
  if(missing(conf_level)) {
    conf_level = (1-alpha_level)
  }
  
  if(missing(post_diff)) {
    post_diff = 0
  }
  
  if(missing(MOE)) {
    MOE = sd
  }
  
  if(r<=0 | r>=1){
    stop("r must be a single value between 0 and 1")
  }
  
  if(sd <= 0){
    stop("SD must be a positve number")
  }
  
  row_total = (max_n-min_n+1)
  sim_results = data.frame(n_per_group=(min_n:max_n), assurance_ANCOVA = rep(NA,row_total),
                           width_ANCOVA = rep(NA,row_total), sd_width_ANCOVA = rep(NA,row_total),
                           power_ANCOVA = rep(NA,row_total),
                           assurance_ANOVA = rep(NA,row_total), 
                           width_ANOVA = rep(NA,row_total), sd_width_ANOVA = rep(NA,row_total),
                           power_ANOVA = rep(NA,row_total))
  
  
  for (i in min_n:max_n) {
    #print(i) # Add print progress?
    grp_n = i
    tmp = NULL
    post_diff = post_diff
    sd = sd
    r = r
    alpha_level = alpha_level
    conf_level = conf_level
    tmp <- as.data.frame(t(as.matrix( replicate(nsims, ancova_gen(post_diff=post_diff,conf_level=conf_level,alpha_level = alpha_level, r = r, sd = sd, grp_n = grp_n), simplify = T)   )))
    #print(tmp)
    row = i - (min_n - 1)
    sim_results[row,]$width_ANCOVA = mean(unlist(tmp$width_ac))
    sim_results[row,]$assurance_ANCOVA = mean(unlist(tmp$width_ac) < MOE) * 100
    sim_results[row,]$sd_width_ANCOVA = sd(unlist(tmp$width_ac))
    sim_results[row,]$power_ANCOVA = mean(unlist(tmp$p_ac) < alpha_level)* 100
    
    sim_results[row,]$width_ANOVA = mean(unlist(tmp$width_aov))
    sim_results[row,]$assurance_ANOVA = mean(unlist(tmp$width_aov) < MOE) * 100
    sim_results[row,]$sd_width_ANOVA = sd(unlist(tmp$width_aov))
    sim_results[row,]$power_ANOVA = mean(unlist(tmp$p_aov) < alpha_level)*100
  }
  
  plot_power_ANCOVA = ggplot(sim_results,aes(x=n_per_group, y=power_ANCOVA)) +
    geom_line(size = 1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_bw() +
    labs(x = "Sample size per condition", y = "Power for ANCOVA (%)")
  
  plot_power_ANOVA = ggplot(sim_results, aes(x = n_per_group, y = power_ANOVA)) +
    geom_line(size = 1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_bw() +
    labs(x = "Sample size per condition", y = "Power for ANOVA (%)")
  
  plot_precision_ANCOVA = ggplot(sim_results,aes(x=n_per_group, y=assurance_ANCOVA)) +
    geom_line(size = 1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_bw() +
    labs(x = "Sample size per condition", y = "Assurance Level for ANCOVA C.I. Width (%)")
  
  plot_precision_ANOVA = ggplot(sim_results,aes(x=n_per_group, y=assurance_ANOVA)) +
    geom_line(size = 1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_bw() +
    labs(x = "Sample size per condition", y = "Assurance Level for ANOVA C.I. Width (%)")
  
  list(sim_results = sim_results,
       plot_power_ANCOVA = plot_power_ANCOVA,
       plot_power_ANOVA = plot_power_ANOVA,
       plot_precision_ANCOVA = plot_precision_ANCOVA,
       plot_precision_ANOVA = plot_precision_ANOVA)
  #tmp
  
}