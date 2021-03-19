#' Compute power for \pkg{emmeans} contrasts
#' 
#' Computes power based on t value and degrees of freedom for contrasts.
#' \emph{Do not use to calculate "observed power" for empirical datasets
#' (Hoenig & Heisey, 2001).}
#'
#' @param x \code{emmGrid}. Grid of contrasts to estimate power from.
#' @param ... Other arguments passed to the function if object is not already a \code{emmGrid} object.
#' @inheritParams ANOVA_exact 
#' @details Note that calculation of power is based on the F- and t-ratio assuming
#'   two-sided testing. Thus, the function does not honor adjustments of the
#'   testing procedure due to either one-sided testing (or two-one sided tests)
#'   or corrections for multiple comparisons via the \code{p.adjust} option in 
#'   \code{emmeans}.
#'   
#'   Power for one-sided tests can be calculated, if the means of the simulated
#'   dataset are consistent with the directional hypothesis, by doubling
#'   \code{alpha_level}. Similarly, power for Bonferroni-corrected contrasts can be
#'   calculated by adjusting \code{alpha_level} accordingly (see examples).
#'   \code{...} Other arguments passed onto the function
#'
#' @inherit ANOVA_exact return
#' @importFrom magrittr '%>%'
#' @importFrom dplyr select mutate everything
#' @importFrom tidyselect matches
#' @author Frederik Aust
#' @references Hoenig, J. M., & Heisey, D. M. (2001). The Abuse of Power. The American Statistician, 55(1), 19–24. https://doi.org/10.1198/000313001300339897
#' @export
#' @examples
#' \dontrun{
#' # Set up a within design with 2 factors, each with 2 levels
#' design_result <- ANOVA_design(design = "2w*2w",
#' n = 40, mu = c(1, 0, 1, 0),
#' sd = 2, r = 0.8,
#' labelnames = c("condition", "cheerful",
#'  "sad", "voice", "human", "robot"))
#'  
#' exact_result <- ANOVA_exact(design_result, 
#' alpha_level = 0.05, verbose = FALSE, 
#' emm = TRUE, contrast_type = "pairwise")
#' 
#' # Power for pairwise contrasts
#' exact_result$emm_results
#' 
#' # Corresponding emmeans contrasts
#' exact_result$emmeans$contrasts
#' 
#' # Manually recalculate power
#' emmeans_power(exact_result$emmeans$contrasts, 
#' alpha_level = 0.05)
#' 
#' # Calculate power for Bonferroni-adjusted pairwise comparisons
#' n_contrasts <- nrow(as.data.frame(exact_result$emmeans$contrasts))
#' emmeans_power(exact_result$emmeans$contrasts, 
#' alpha_level = 0.05 / n_contrasts)
#' 
#' # Calculate power for one-sided custom contrasts
#' exact_result$emmeans$emmeans
#' custom_contrast <- contrast(exact_result$emmeans$emmeans, 
#' list(robot_vs_sad_human = c(0, 1, -0.5, -0.5)))
#' emmeans_power(custom_contrast,
#'  alpha_level = 0.05 * 2)
#' 
#' # Calculate power for follow-up ANOVA
#' follow_up <- joint_tests(exact_result$emmeans$emmeans, 
#' by = "condition")
#' emmeans_power(follow_up, 
#' alpha_level = 0.05 / 2)
#' emmeans_power(emmeans(exact_result$emmeans$emmeans, 
#' pairwise ~ voice | condition)$contrasts, 
#' alpha_level = 0.05 / 2)
#' }

emmeans_power <- function(x, ...) {
  UseMethod("emmeans_power", x)
}

#' @rdname emmeans_power
#' @export

emmeans_power.emmGrid <- function(x, ...) {
  x_df <- as.data.frame(x)
  emmeans_power.data.frame(x_df, ...)
}

#' @rdname emmeans_power
#' @export

emmeans_power.summary_em <- function(x, ...) {
  NextMethod("emmeans_power", x, ...)
}

#' @rdname emmeans_power
#' @export

emmeans_power.data.frame <- function(x, 
                                     alpha_level = Superpower_options("alpha_level"),
                                     liberal_lambda = Superpower_options("liberal_lambda"), ...) {
  if (is.null(x$t.ratio) & is.null(x$F.ratio)) stop("emmGrid object 'x' does not contain test statistic (t or F). Please set `infer = TRUE` in emmeans.")
  
  # Need for exact; not necessary for power function
  # Convert t-ratio to F-stat
  if (is.null(x$F.ratio)) {
    x$F.ratio <- (x$t.ratio)^2
    x$df1 <- 1
    x$df2 <- x$df
    x <- select(x, -.data$t.ratio, -.data$df, -.data$SE, -matches("estimate|emmean"), -matches("\\.CL$"))
  }
  
  # Calculate partial eta^2
  x$pes <- x$F.ratio/(x$F.ratio + x$df2) 
  
  # Calculate Cohen's f
  x$f2 <- x$pes/(1 - x$pes)
  
  # Calculate noncentrality parameter
  x$lambda <- if (liberal_lambda == FALSE) {
    x$f2 * x$df2
  } else {
    x$f2 * (x$df2 + x$df1 + 1)
  }
  x$Ft <- qf((1 - alpha_level), x$df1, x$df2)
  
  # Calculate power
  x$power <- (1 - pf(x$Ft, x$df1, x$df2, x$lambda))*100
  
  x <- x %>% 
    mutate(
      partial_eta_squared = .data$pes,
      cohen_f = sqrt(.data$f2),
      non_centrality = .data$lambda
    ) %>%
    select(-.data$p.value,-.data$F.ratio,-.data$Ft,
           -.data$f2,-.data$lambda,-.data$pes, -.data$df1, -.data$df2, -matches("null")) %>%
    select(-.data$power, -.data$partial_eta_squared, -.data$cohen_f, -.data$non_centrality,
           everything(), .data$power, .data$partial_eta_squared, .data$cohen_f, .data$non_centrality)
  
  x
}