#' Plot out power sensitivity plots for t or F tests
#' @param es Effect size range; either cohen's f or cohen's d depending on whether it is an f-test or t-test
#' @param n Sample size (t-test only) per group (two sample), total number of pairs (paired samples), or total observations (one-sample); only applies to t-test
#' @param alternative one- or two-sided test. Can be abbreviated.
#' @alpha_level A vector of alpha levels; default is 0.05
#' @param liberal_lambda Logical indictor of whether to use the liberal (cohen_f^2\*(num_df+den_df)) or conservative (cohen_f^2\*den_df) calculation of the noncentrality (lambda) parameter estimate. Default is FALSE.
#' @return Returns plots of effect size (x-axis) 
#'
#' @examples
#' \dontrun{
#' # To be added
#' }
#' @section References:
#' to be added
#' @importFrom stats optimize power.t.test
#' @import ggplot2
#' @import tidyr expand_grid
#' @import magrittr %>%
#' @import dplyr mutate
#' @export
#'

morey_plot.ttest = function(es = seq(0,1,.05),
                            n = NULL,
                            type = c("two.sample", "one.sample", "paired"),
                            alternative = c("two.sided", "one.sided"),
                            alpha_level = Superpower_options("alpha_level")){
  xlab = expression(paste("Standardized Effect Size (", delta,")"))
  legend_label = expression(paste(alpha,"-level"))
  if (!is.null(es)) {
    if (any(es < 0)) {
      stop("es must be positive")
    }
  }
  if (!is.null(n) && any(n < 3)) {
    stop("Sample size per group must be at least 3")
  }
  if (!is.null(alpha_level) && !is.numeric(alpha_level) || any(0 > 
                                                               alpha_level | alpha_level > 1)) {
    stop(sQuote("alpha_level"), " must be numeric in [0, 1]")
  }
  
  plot_title = power.t.test(n = min(n),
                            d = min(es),
                            type = type,
                            alternative = alternative)$method %>%
    gsub(replacement = "Curve",
         pattern = "calculation",
         fixed = TRUE) %>%
    gsub(replacement = "Power",
         pattern = "power",
         fixed = TRUE) %>%
    gsub(replacement = "t-test",
         pattern = "t test",
         fixed = TRUE)
  
  sub_title = power.t.test(
    n = min(n),
    d = min(es),
    type = type,
    alternative = alternative
  )$alternative %>%
    gsub(replacement = "-tailed",
         pattern = ".sided",
         fixed = TRUE)
  
  

  
  dats = expand.grid(es = es,
                     n = n,
                     alpha = alpha_level) %>%
    mutate(power = power.t.test(n = n,
                            delta = es,
                            type = type,
                            alternative = alternative,
                            sig.level = alpha)$power*100 )
  morey_plot = ggplot(dats,
                      aes(x=es,
                          y=power,
                          color = as.factor(alpha))) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = xlab,
         y = "Power (%)",
         title = plot_title,
         color = legend_label,
         subtitle = sub_title) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,10)) +
    theme_bw() +
    facet_wrap(~n,
               labeller = label_both)
  
  return(morey_plot)
}

morey_plot.ftest = function(es = seq(0,1,.05),
                            num_df = 1,
                            den_df = NULL,
                            alpha_level = Superpower_options("alpha_level"),
                            liberal_lambda = Superpower_options("alpha_level")){
  xlab = expression(paste("Standardized Effect Size (Cohen's ",italic(f),")")) 
  legend_label = expression(paste(alpha,"-level"))
  if (!is.null(es)) {
    if (any(es < 0)) {
      stop("es must be positive")
    }
  }
  if (is.null(num_df) || any(num_df < 1)) {
    stop("degree of freedom num_df for numerator must be at least 1")
  }
  
  if (is.null(den_df) || any(den_df < 1)) {
    stop("degree of freedom den_df for denominator must be at least 1")
  }
  if (!is.null(alpha_level) && !is.numeric(alpha_level) || any(0 > 
                                                               alpha_level | alpha_level > 1)) {
    stop(sQuote("alpha_level"), " must be numeric in [0, 1]")
  }

  dats = expand.grid(cohen_f = es,
                     num_df = num_df,
                     den_df = den_df,
                     alpha = alpha_level) %>%
    mutate(power = power.ftest(cohen_f = cohen_f,
                               num_df = num_df,
                               den_df = den_df,
                               alpha_level = alpha,
                               liberal_lambda = liberal_lambda)$power)
  morey_plot = ggplot(dats,
                      aes(x=cohen_f,
                          y=power,
                          color = as.factor(alpha))) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = xlab,
         y = "Power (%)",
         title = "F-test Power Curve",
         color = legend_label) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,10)) +
    theme_bw() +
    facet_grid(den_df~num_df,
               labeller = label_both)
  
  return(morey_plot)
}

test = morey_plot.ftest(den_df=15,
                        es = seq(0,1,.01),
                 alpha_level = c(.025,.088))
test
ggplot(test$data,
       aes(x=cohen_f,
           y=power,
           color = as.factor(alpha))) +
  geom_line() 

power.ftest(cohen_f = .1,
            num_df = 1,
            den_df = 15,
            alpha_level = .088,
            liberal_lambda = TRUE)$power
morey_plot.ttest(n=12)
