#' Convenience function to plot power across a range of sample sizes.
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance
#' @param min_n Minimum sample size in power curve.
#' @param max_n Maximum sample size in power curve.
#' @param plot Should power plot be printed (defaults to TRUE)
#' @return Returns plot with power curves for the ANOVA, and a dataframe with the summary data.
#' @examples
#' design_result <- ANOVA_design(design = "3b",
#'                              n = 20,
#'                              mu = c(0,0,0.3),
#'                              sd = 1,
#'                              labelnames = c("condition",
#'                              "cheerful", "neutral", "sad"))
#'
#' plot_power(design_result, min_n = 50, max_n = 70)
#' @section References:
#' too be added
#' @importFrom stats pf qf
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @import ggplot2
#' @export

plot_power <- function(design_result, alpha_level,
                       min_n = 7, max_n = 100,
                       plot = TRUE){
  design = design_result$design
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r
  labelnames <- design_result$labelnames
  n <- design_result$n


  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }

  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
  }

  #Errors with very small sample size; issue with mvrnorm function from MASS package
  if (design_result$n < prod(as.numeric(unlist(regmatches(design_result$design,
                                       gregexpr("[[:digit:]]+", design_result$design)))))
  ) {
    stop("plot_power cannot handle small sample sizes (n < the product of the factors) at this time; please increase the in ANOVA_design function.")
  }

  #Check to ensure there is a within subject factor -- if none --> no MANOVA
  run_manova <- grepl("w", design_result$design)

  Roy <- function(eig, q, df.res) {
    p <- length(eig)
    test <- max(eig)
    tmp1 <- max(p, q)
    tmp2 <- df.res - tmp1 + q
    c(test, (tmp2 * test)/tmp1, tmp1, tmp2)
  }

  Wilks <- function(eig, q, df.res)
  {
    test <- prod(1/(1 + eig))
    p <- length(eig)
    tmp1 <- df.res - 0.5 * (p - q + 1)
    tmp2 <- (p * q - 2)/4
    tmp3 <- p^2 + q^2 - 5
    tmp3 <- if (tmp3 > 0)
      sqrt(((p * q)^2 - 4)/tmp3)
    else 1
    c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q,
      p * q, tmp1 * tmp3 - 2 * tmp2)
  }

  HL <- function(eig, q, df.res)
  {
    test <- sum(eig)
    p <- length(eig)
    m <- 0.5 * (abs(p - q) - 1)
    n <- 0.5 * (df.res - p - 1)
    s <- min(p, q)
    tmp1 <- 2 * m + s + 1
    tmp2 <- 2 * (s * n + 1)
    c(test, (tmp2 * test)/s/s/tmp1, s * tmp1, tmp2)
  }

  Pillai <- function(eig, q, df.res)
  {
    test <- sum(eig/(1 + eig))
    p <- length(eig)
    s <- min(p, q)
    n <- 0.5 * (df.res - p - 1)
    m <- 0.5 * (abs(p - q) - 1)
    tmp1 <- 2 * m + s + 1
    tmp2 <- 2 * n + s + 1
    c(test, (tmp2/tmp1 * test)/(s - test), s * tmp1, s * tmp2)
  }

  #Only utilized if MANOVA output included (see run_manova)
  Anova.mlm.table <- function(x, ...)
  {
    test <- x$test
    repeated <- x$repeated
    ntests <- length(x$terms)
    tests <- matrix(NA, ntests, 4)
    if (!repeated)
      SSPE.qr <- qr(x$SSPE)
    for (term in 1:ntests) {
      eigs <- Re(eigen(qr.coef(if (repeated)
        qr(x$SSPE[[term]])
        else
          SSPE.qr,
        x$SSP[[term]]), symmetric = FALSE)$values)
      tests[term, 1:4] <- switch(
        test,
        Pillai = Pillai(eigs,
                        x$df[term], x$error.df),
        Wilks = Wilks(eigs,
                      x$df[term], x$error.df),
        `Hotelling-Lawley` =
          HL(eigs,
             x$df[term], x$error.df),
        Roy = Roy(eigs,
                  x$df[term], x$error.df)
      )
    }
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3],
                                   tests[ok, 4], lower.tail = FALSE))
    rownames(tests) <- x$terms
    colnames(tests) <- c("df", "test_stat", "approx_F", "num_Df",
                         "den_Df", "p.value")
    tests <- structure(as.data.frame(tests), heading = paste("\nType ",
                                                             x$type, if (repeated)
                                                               " Repeated Measures", " MANOVA Tests: ", test, " test
                                                             statistic",
                                                             sep = ""), class = c("anova", "data.frame"))
    invisible(tests)
  }

  #Do one ANOVA to get number of power columns
  exact_result <- ANOVA_exact(design_result, alpha_level = alpha_level,
                              verbose = FALSE)

  length_power <- length(exact_result$main_results$power)

  power_df <- as.data.frame(matrix(0, ncol = length_power + 1,
                                   nrow = max_n + 1 - min_n))
  power_df[,1] <- c((min_n):max_n)

  colnames(power_df) <- c("n", row.names(exact_result$main_results))

  if (run_manova == TRUE) {

  length_power_manova <- length(exact_result$manova_results$power)

  power_df_manova <- as.data.frame(matrix(0, ncol = length_power_manova + 1,
                                          nrow = max_n + 1 - min_n))
  power_df_manova[, 1] <- c((min_n):max_n)

  colnames(power_df_manova) <- c("n", row.names(exact_result$manova_results))

  }

  for (i in 1:(max_n + 1 - min_n)) {

    design_result <- ANOVA_design(design = design,
                                  n = i + min_n,
                                  mu = mu,
                                  sd = sd,
                                  r = r,
                                  labelnames = labelnames,
                                  plot = FALSE)
    dataframe <- design_result$dataframe

    dataframe$y <- suppressMessages({
      melt(as.data.frame(
        mvrnorm(
          n = design_result$n,
          mu = design_result$mu,
          Sigma = as.matrix(design_result$sigmatrix)
        )
      ))$value
    })

    # We perform the ANOVA using AFEX
    #Can be set to NICE to speed up, but required data grabbing from output the change.
    aov_result <-
      suppressMessages({
        aov_car(
          design_result$frml1,
          #here we use frml1 to enter fromula 1 as designed above on the basis of the design
          data = dataframe,
          include_aov = FALSE,
          #Need development code to get aov include function
          anova_table = list(es = "pes",
                             correction = "none")
        )
      }) #This reports PES not GES


    #Add additional statistics
    #Create dataframe from afex results
    anova_table <- as.data.frame(aov_result$anova_table)
    colnames(anova_table) <- c("num_Df", "den_Df", "MSE", "F", "pes", "p")

    anova_table$pes <- exact_result$main_results$partial_eta_squared
    #Calculate cohen's f
    anova_table$f2 <- anova_table$pes / (1 - anova_table$pes)
    #Calculate noncentrality
    anova_table$lambda <- anova_table$f2 * anova_table$den_Df

    #minusalpha<- 1-alpha_level
    anova_table$Ft <-
      qf((1 - alpha_level), anova_table$num_Df, anova_table$den_Df)
    #Calculate power
    anova_table$power <-
      (1 - pf(
        anova_table$Ft,
        anova_table$num_Df,
        anova_table$den_Df,
        anova_table$lambda
      )) * 100

    power_df[i, 2:(1 + length_power)] <- anova_table$power

    if (run_manova == TRUE) {
      manova_result <- Anova.mlm.table(aov_result$Anova)



      manova_result$f2 <- exact_result$manova_results$pillai_trace / (1 - exact_result$manova_results$pillai_trace)
      manova_result$lambda <-   manova_result$f2 *   manova_result$den_Df
      manova_result$Ft <- qf((1 - alpha_level), manova_result$num_Df,   manova_result$den_Df)
      manova_result$power <- round(1 - pf(manova_result$Ft,
                                          manova_result$num_Df,
                                          manova_result$den_Df,
                                          manova_result$lambda), 4) * 100

      power_df_manova[i, 2:(1 + length_power_manova)] <- manova_result$power
    }

  }

  plot_data <- suppressMessages(melt(power_df, id = c('n')))

  p1 <- ggplot(data = plot_data, aes(x = plot_data$n, y = plot_data$value)) +
    geom_line(size = 1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_bw() +
    labs(x = "Sample size per condition", y = "Power") +
    facet_grid(variable ~ .)

  if (run_manova == TRUE) {
    plot_data_manova <- suppressMessages(melt(power_df_manova, id = c('n')))

    p2 <- ggplot(data = plot_data_manova,
                 aes(x = plot_data_manova$n, y = plot_data_manova$value)) +
      geom_line(size = 1.5) +
      scale_x_continuous(limits = c(min_n, max_n)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
      theme_bw() +
      labs(x = "Sample size per condition", y = "Power") +
      facet_grid(variable ~ .)
  }

  if (plot == TRUE) {
    print(p1)
  }

  if (run_manova == FALSE) {
    p2 = NULL
    power_df_manova = NULL
    effect_sizes_manova = NULL
  }

  #Save effect sizes
  effect_sizes <- exact_result$main_results[,2:3]

  effect_sizes_manova <- exact_result$manova_results[,2:3]

  invisible(list(p1 = p1,
                 p2 = p2,
                 power_df = power_df,
                 power_df_manova = power_df_manova,
                 effect_sizes = effect_sizes,
                 effect_sizes_manova = effect_sizes_manova))
}
