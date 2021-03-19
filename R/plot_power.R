#' Convenience function to plot power across a range of sample sizes.
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance
#' @param min_n Minimum sample size in power curve. Cannot be less than or equal to the product of factors. E.g., if design = "2b*2b" then min_n must be at least 5 (2\*2+1=5)
#' @param max_n Maximum sample size in power curve.
#' @param desired_power Desired power (e.g., 80, 90). N per group will be highlighted to achieve this desired power in the plot. Defaults to 90.
#' @param plot Should power plot be printed automatically (defaults to TRUE)
#' @param emm Set to FALSE to not perform analysis of estimated marginal means
#' @param emm_model Set model type ("multivariate", or "univariate") for estimated marginal means
#' @param contrast_type Select the type of comparison for the estimated marginal means
#' @param emm_comp Set the comparisons for estimated marginal means comparisons. This is a factor name (a), combination of factor names (a+b), or for simple effects a | sign is needed (a|b)
#' @param verbose Set to FALSE to not print results (default = TRUE)
#' @param exact2 Logical indicator for which \code{ANOVA_exact} function (\code{ANOVA_exact} or \code{ANOVA_exact2}) to use in the plots. Default is FALSE which uses \code{ANOVA_exact} which has sample size limitations.
#' @param liberal_lambda Logical indicator of whether to use the liberal (cohen_f^2\*(num_df+den_df)) or conservative (cohen_f^2\*den_df) calculation of the noncentrality (lambda) parameter estimate. Default is FALSE.
#' @return Returns plot with power curves for the ANOVA, and a dataframe with the summary data.
#' 

#' \describe{
#'   \item{\code{"plot_ANOVA"}}{Plot of power curves from ANOVA results.}
#'   \item{\code{"plot_MANOVA"}}{Plot of power curves from MANOVA results. Returns NULL if no within-subject factors.}
#'   \item{\code{"plot_emm"}}{Plot of power curves from MANOVA results. Returns NULL if emm = FALSE.}
#'   \item{\code{"anova_n"}}{Achieved Power and Sample Size for ANOVA-level effects.}
#'   \item{\code{"manova_n"}}{Achieved Power and Sample Size for MANOVA-level effects.}
#'   \item{\code{"emm_n"}}{Achieved Power and Sample Size for estimated marginal means.}
#'   \item{\code{"power_df"}}{The tabulated ANOVA power results.}
#'   \item{\code{"power_df_manova"}}{The tabulated MANOVA power results. Returns NULL if no within-subject factors.}
#'   \item{\code{"power_df_emm"}}{The tabulated Estimated Marginal Means power results. Returns NULL if emm = FALSE.}
#'   \item{\code{"effect_sizes"}}{Effect sizes (partial eta-squared) from ANOVA results.}
#'   \item{\code{"effect_sizes_manova"}}{Effect sizes (Pillai's Trace) from MANOVA results. Returns NULL if no within-subject factors.}
#'   \item{\code{"effect_sizes_emm"}}{ Effect sizes (cohen's f) estimated marginal means results. Returns NULL if emm = FALSE.}
#'   
#' }
#' 
#' @examples
#' \dontrun{
#' design_result <- ANOVA_design(design = "3b",
#'                              n = 20,
#'                              mu = c(0,0,0.3),
#'                              sd = 1,
#'                              labelnames = c("condition",
#'                              "cheerful", "neutral", "sad"))
#'
#' plot_power(design_result, min_n = 50, max_n = 70, desired_power = 90)
#' }
#' @section References:
#' too be added
#' @importFrom stats pnorm pt qnorm qt as.formula median qf power.t.test pf sd power
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @importFrom graphics pairs
#' @importFrom magrittr '%>%'
#' @importFrom dplyr select mutate everything
#' @import emmeans
#' @import ggplot2
#' @export

plot_power <- function(design_result, 
                       alpha_level = Superpower_options("alpha_level"),
                       min_n = 7, max_n = 100,
                       desired_power = 90,
                       plot = Superpower_options("plot"),
                       emm = Superpower_options("emm"),
                       emm_model = Superpower_options("emm_model"),
                       contrast_type = Superpower_options("contrast_type"),
                       emm_comp,
                       verbose = Superpower_options("verbose"),
                       exact2 = FALSE,
                       liberal_lambda = Superpower_options("liberal_lambda")){
  
  #Need this to avoid "undefined" global error or no visible binding from occuring
  cohen_f <- partial_eta_squared <- non_centrality <- pairs_results_df <- value <- label <- variable <- achieved_power <- NULL
  #New checks for emmeans input
  if (missing(emm)) {
    emm = FALSE
  }
  
  if (missing(emm_model)) {
    emm_model = "multivariate"
  }
  
  #Follow if statements limit the possible input for emmeans specifications
  if (emm == TRUE) {
    if (is.element(emm_model, c("univariate", "multivariate")) == FALSE ) {
      stop("emm_model must be set to \"univariate\" or \"multivariate\". ")
    }
    if (is.element(contrast_type, 
                   c("pairwise", 
                     "revpairwise",
                     "eff",
                     "consec",
                     "poly",
                     "del.eff",
                     "trt.vs.ctrl",
                     "trt.vs.ctrl1",
                     "trt.vs.ctrlk",
                     "mean_chg"
                   )) == FALSE ) {
      stop("contrast_type must be of an accepted format. 
           The tukey & dunnett options currently not supported in ANOVA_exact. 
           See help(\"contrast-methods\") for details on the exact methods")
    }
  }
  
  design = design_result$design
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r
  labelnames <- design_result$labelnames
  n <- design_result$n
  if (length(n) != 1 ) {
    warning("Unequal n designs can only be passed to ANOVA_power")
  }
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  
  if (missing(emm_comp)) {
    emm_comp = as.character(frml2)[2]
  }
  
  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }
  
  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
  }
  
  #Errors with very small sample size; issue with mvrnorm function from MASS package
  if (exact2 == FALSE && design_result$n < (prod(as.numeric(unlist(regmatches(design_result$design,
                                                          gregexpr("[[:digit:]]+", design_result$design)))))+1)
  ) {
    stop("plot_power must have an ANOVA_design object with n > the product of the factors; please set exact2 argument to TRUE")
  }
  

  if (exact2 == FALSE && min_n < prod(as.numeric(unlist(regmatches(design_result$design,
                                                          gregexpr("[[:digit:]]+", design_result$design)))))+1
  ) {
    prod_fact = prod(as.numeric(unlist(regmatches(design_result$design,
                                                  gregexpr("[[:digit:]]+", design_result$design)))))+1
    warning("min_n <= the product of the factors; therefore min_n changed to ", prod_fact, "\n Set exact2 to TRUE to include this min_n")
    min_n = prod_fact
  }
  
  #Check to ensure there is a within subject factor -- if none --> no MANOVA
  run_manova <- grepl("w", design_result$design)
  
  
  
  
  
  #Do one ANOVA to get number of power columns
  #if (emm == FALSE) {
  #  exact_result <- ANOVA_exact(design_result, alpha_level = alpha_level,
  #                              verbose = FALSE)
  #} else {
    #Call emmeans with specifcations given in the function
    #Limited to specs and model
    #if (missing(emm_comp)) {
    #  emm_comp = as.character(frml2)[2]
    #}
    exact_result <- if (exact2 == FALSE) {
      ANOVA_exact(design_result, alpha_level = alpha_level,
                                emm = TRUE,
                                contrast_type = contrast_type,
                                emm_model = emm_model,
                                emm_comp = emm_comp,
                                verbose = FALSE,
                  liberal_lambda = liberal_lambda)
    } else if (exact2 == TRUE) {
      ANOVA_exact2(design_result, alpha_level = alpha_level,
                  emm = TRUE,
                  contrast_type = contrast_type,
                  emm_model = emm_model,
                  emm_comp = emm_comp,
                  verbose = FALSE,
                  liberal_lambda = liberal_lambda)
    }
  #}
  
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
  
  if (emm == TRUE) {
    
    length_power_emm <- length(exact_result$emm_results$power)
    
    power_df_emm <- as.data.frame(matrix(0, ncol = length_power_emm + 1,
                                         nrow = max_n + 1 - min_n))
    power_df_emm[,1] <- c((min_n):max_n)
    
    colnames(power_df_emm) <- c("n", as.character(exact_result$emm_results$contrast))
    
  } 
  
  for (i in 1:(max_n + 1 - min_n)) {
    
    design_result <- ANOVA_design(design = design,
                                  n = i + min_n - 1,
                                  mu = mu,
                                  sd = sd,
                                  r = r,
                                  labelnames = labelnames,
                                  plot = FALSE)
    
   
      exact_result <- if (exact2 == FALSE) {
        ANOVA_exact(design_result, alpha_level = alpha_level,
                    emm = TRUE,
                    contrast_type = contrast_type,
                    emm_model = emm_model,
                    emm_comp = emm_comp,
                    verbose = FALSE,
                    liberal_lambda = liberal_lambda)
      } else if (exact2 == TRUE) {
        ANOVA_exact2(design_result, alpha_level = alpha_level,
                     emm = TRUE,
                     contrast_type = contrast_type,
                     emm_model = emm_model,
                     emm_comp = emm_comp,
                     verbose = FALSE,
                     liberal_lambda = liberal_lambda)
      }
      power_df[i, 2:(1 + length_power)] <- exact_result$main_results$power
      
      if (run_manova == TRUE) {
        power_df_manova[i, 2:(1 + length_power_manova)] <- exact_result$manova_results$power
      }
      
     if (emm == TRUE) {
      #Call emmeans with specifcations given in the function
      #Limited to specs and model
      #Don't need to run ANOVA_exact twice

      power_df_emm[i, 2:(1 + length_power_emm)] <- exact_result$emm_results$power
    }
  }
  
  plot_data <- suppressMessages(melt(power_df, id = c('n')))
  plot_data$variable <- as.factor(plot_data$variable)
  
  #create data frame for annotation for desired power
  annotate_df <- as.data.frame(matrix(0, ncol = 4, nrow = length(row.names(exact_result$main_results)))) #three rows, for N, power, and variable label
  colnames(annotate_df) <- c("n", "power", "variable", "label") # add columns names
  annotate_df$variable <- as.factor(c(row.names(exact_result$main_results))) #add variable label names
  anova_n = annotate_df
  
  # Create a dataframe with columns for each effect and rows for the N and power for that N
  for (i in 1:length_power) {
    annotate_df[i,1] <- tryCatch(findInterval(desired_power, power_df[,(1 + i)]), error=function(e){max_n-min_n}) + min_n #use findinterval to find the first value in the vector before desired power. Add 1 (we want to achieve the power, not stop just short) then add min_n (because the vector starts at min_n, not 0)
    if(annotate_df[i,1] > max_n){annotate_df[i,1] <- max_n} # catches cases that do not reach desired power. Then just plot max_n
    if(annotate_df[i,1] == max_n){annotate_df[i,1] <- (min_n+max_n)/2} # We will plot that max power is not reached at midpoint of max n
    annotate_df[i,2] <- power_df[annotate_df[i,1]-min_n+1,(i+1)] #store exact power at N for which we pass desired power (for plot)
    if(annotate_df[i,2] < desired_power){annotate_df[i,4] <- "Desired Power Not Reached"}
    if(annotate_df[i,2] >= desired_power){annotate_df[i,4] <- annotate_df[i,1]}
    if(annotate_df[i,2] < desired_power){annotate_df[i,2] <- 5}
    
    # Repeat process for tabular results
    anova_n[i,1] <- tryCatch(findInterval(desired_power, power_df[,(1 + i)]), error=function(e){max_n-min_n}) + min_n #use findinterval to find the first value in the vector before desired power. Add 1 (we want to achieve the power, not stop just short) then add min_n (because the vector starts at min_n, not 0)
    if(anova_n[i,1] > max_n){anova_n[i,1] <- max_n} # catches cases that do not reach desired power. Then just plot max_n
    anova_n[i,2] <- power_df[anova_n[i,1]-min_n+1,(i+1)] #store exact power at N for which we pass desired power (for plot)
    if(anova_n[i,2] < desired_power){anova_n[i,4] <- "Desired Power Not Reached"}
    if(anova_n[i,2] >= desired_power){anova_n[i,4] <- "Desired Power Achieved"}

  }
  
  p1 <- ggplot(data = plot_data, aes(x = n, y = value)) +
    geom_line(size = 1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_bw() +
    labs(x = "Sample size per condition", y = "Power") +
    geom_line(y = desired_power, colour="red", alpha = 0.3, size = 1) + 
    geom_label(data = annotate_df, aes(x = n, y = power, label = label)) +
    facet_grid(variable ~ .)
  
  if (run_manova == TRUE) {
    plot_data_manova <- suppressMessages(melt(power_df_manova, id = c('n')))
    
    #create data frame for annotation for desired power for manova
    annotate_df_manova <- as.data.frame(matrix(0, ncol = 4, nrow = length(row.names(exact_result$manova_results)))) #three rows, for N, power, and variable label
    colnames(annotate_df_manova) <- c("n", "power", "variable", "label") # add columns names
    annotate_df_manova$variable <- as.factor(c(row.names(exact_result$manova_results))) #add variable label names
    
    manova_n = annotate_df_manova
    
    # Create a dataframe with columns for each effect and rows for the N and power for that N
    for (i in 1:length_power_manova) {
      annotate_df_manova[i,1] <- tryCatch(findInterval(desired_power, power_df_manova[,(1 + i)]), error=function(e){max_n-min_n}) + min_n #use findinterval to find the first value in the vector before desired power. Add 1 (we want to achieve the power, not stop just short) then add min_n (because the vector starts at min_n, not 0)
      if(annotate_df_manova[i,1] > max_n){annotate_df_manova[i,1] <- max_n} # catches cases that do not reach desired power. Then just plot max_n
      if(annotate_df_manova[i,1] == max_n){annotate_df_manova[i,1] <- (min_n+max_n)/2} # We will plot that max power is not reached at midpoint of max n
      annotate_df_manova[i,2] <- power_df_manova[annotate_df_manova[i,1]-min_n+1,(i+1)] #store exact power at N for which we pass desired power (for plot)
      if(annotate_df_manova[i,2] < desired_power){annotate_df_manova[i,4] <- "Desired Power Not Reached"}
      if(annotate_df_manova[i,2] >= desired_power){annotate_df_manova[i,4] <- annotate_df_manova[i,1]}
      if(annotate_df_manova[i,2] < desired_power){annotate_df_manova[i,2] <- 5}
      
      
      manova_n[i,1] <- tryCatch(findInterval(desired_power, power_df_manova[,(1 + i)]), error=function(e){max_n-min_n}) + min_n #use findinterval to find the first value in the vector before desired power. Add 1 (we want to achieve the power, not stop just short) then add min_n (because the vector starts at min_n, not 0)
      if(manova_n[i,1] > max_n){manova_n[i,1] <- max_n} # catches cases that do not reach desired power. Then just plot max_n
      manova_n[i,2] <- power_df_manova[manova_n[i,1]-min_n+1,(i+1)] #store exact power at N for which we pass desired power (for plot)
      if(manova_n[i,2] < desired_power){manova_n[i,4] <- "Desired Power Not Reached"}
      if(manova_n[i,2] >= desired_power){manova_n[i,4] <- "Desired Power Achieved"}
  
    }
    
    p2 <- ggplot(data = plot_data_manova,
                 aes(x = n, y = value)) +
      geom_line(size = 1.5) +
      scale_x_continuous(limits = c(min_n, max_n)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
      geom_line(y = desired_power, colour="red", alpha = 0.3, size = 1) + 
      geom_label(data = annotate_df_manova, aes(x = n, y = power, label = label)) +
      theme_bw() +
      labs(x = "Sample size per condition", y = "Power") +
      facet_grid(variable ~ .)
  }
  
  if (emm == TRUE) {
    plot_data_emm <- suppressMessages(melt(power_df_emm, id = c('n')))
    
    #create data frame for annotation for desired power for emmeans
    annotate_df_emm <- as.data.frame(matrix(0, ncol = 4, nrow = length(levels(exact_result$emm_results$contrast)))) #three rows, for N, power, and variable label
    colnames(annotate_df_emm) <- c("n", "power", "variable", "label") # add columns names
    annotate_df_emm$variable <- as.factor(levels(exact_result$emm_results$contrast)) #add variable label names
    emm_n = annotate_df_emm
    
    i<-1
    # Create a dataframe with columns for each effect and rows for the N and power for that N
    for (i in 1:length_power_emm) {
      annotate_df_emm[i,1] <- tryCatch(findInterval(desired_power, power_df_emm[,(1 + i)]), error=function(e){max_n-min_n}) + min_n #use findinterval to find the first value in the vector before desired power. Add 1 (we want to achieve the power, not stop just short) then add min_n (because the vector starts at min_n, not 0)
      if(annotate_df_emm[i,1] > max_n){annotate_df_emm[i,1] <- max_n} # catches cases that do not reach desired power. Then just plot max_n
      if(annotate_df_emm[i,1] == max_n){annotate_df_emm[i,1] <- (min_n+max_n)/2} # We will plot that max power is not reached at midpoint of max n
      annotate_df_emm[i,2] <- power_df_emm[annotate_df_emm[i,1]-min_n+1,(i+1)] #store exact power at N for which we pass desired power (for plot)
      if(annotate_df_emm[i,2] < desired_power){annotate_df_emm[i,4] <- "Desired Power Not Reached"}
      if(annotate_df_emm[i,2] >= desired_power){annotate_df_emm[i,4] <- annotate_df_emm[i,1]}
      if(annotate_df_emm[i,2] < desired_power){annotate_df_emm[i,2] <- 5}
      
      emm_n[i,1] <- tryCatch(findInterval(desired_power, power_df_emm[,(1 + i)]), error=function(e){max_n-min_n}) + min_n 
      if(emm_n[i,1] > max_n){emm_n[i,1] <- max_n} # catches cases that do not reach desired power. Then just plot max_n

      emm_n[i,2] <- power_df_emm[emm_n[i,1]-min_n+1,(i+1)] #store exact power at N for which we pass desired power (for plot)
      if(emm_n[i,2] < desired_power){emm_n[i,4] <- "Desired Power Not Reached"}
      if(emm_n[i,2] >= desired_power){emm_n[i,4] <- "Desired Power Achieved"}

    }
    
    p3 <- ggplot(data = plot_data_emm,
                 aes(x = n, y = value)) +
      geom_line(size = 1.5) +
      scale_x_continuous(limits = c(min_n, max_n)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
      geom_line(y = desired_power, colour="red", alpha = 0.3, size = 1) + 
      geom_label(data = annotate_df_emm, aes(x = n, y = power, label = label)) +
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
    manova_n = NULL
  }
  
  if (emm == FALSE) {
    p3 = NULL
    power_df_emm = NULL
    effect_sizes_emm = NULL
    emm_n = NULL
    
  }
  
  #Save effect sizes
  effect_sizes <- exact_result$main_results[,2:3]
  
  if (run_manova == TRUE) {
    effect_sizes_manova <- exact_result$manova_results[,2:3]
  }
  
  if (emm == TRUE) {
    effect_sizes_emm <- exact_result$emm_results %>%
      select(everything(),-non_centrality,-power)
  }
  
  # Save desired power data frames
  anova_n = anova_n %>%
    mutate(achieved_power = power,
           desired_power = desired_power) %>%
    select(variable, label, n, achieved_power, desired_power)
  
  if (run_manova == TRUE){
    manova_n = manova_n %>%
      mutate(achieved_power = power,
             desired_power = desired_power)  %>%
      select(variable, label, n, achieved_power, desired_power)
  }
  
  if (emm == TRUE){
    emm_n = emm_n %>%
      mutate(achieved_power = power,
             desired_power = desired_power)  %>%
      select(variable, label, n, achieved_power, desired_power)
  }
  
  
  if (verbose == TRUE) {
    # The section below should be blocked out when in Shiny
    cat("Achieved Power and Sample Size for ANOVA-level effects")
    cat("\n")
    print_anova_n = anova_n
    print_anova_n$achieved_power = round(anova_n$achieved_power,2)
    print(print_anova_n)
    if (emm == TRUE) {
      cat("\n")
      cat("Achieved Power and Sample Size for estimated marginal means")
      cat("\n")
      print_emm <- emm_n %>%
        mutate(achieved_power = round(achieved_power,2))
      print(print_emm)
    }
  }
  
  
  invisible(list(anova_n = anova_n,
                 manova_n = manova_n,
                 emm_n = emm_n,
                 plot_ANOVA = p1,
                 plot_MANOVA = p2,
                 plot_emm = p3,
                 power_df = power_df,
                 power_df_manova = power_df_manova,
                 power_df_emm = power_df_emm,
                 effect_sizes = effect_sizes,
                 effect_sizes_manova = effect_sizes_manova,
                 effect_sizes_emm = effect_sizes_emm))
}
