#' Simulation function used to estimate power
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance
#' @param correction Set a correction of violations of sphericity. This can be set to "none", "GG" Greenhouse-Geisser, and "HF" Huynh-Feldt
#' @param p_adjust Correction for multiple comparisons. This will adjust p values for ANOVA/MANOVA level effects; see ?p.adjust for options
#' @param emm Set to FALSE to not perform analysis of estimated marginal means
#' @param emm_model Set model type ("multivariate", or "univariate") for estimated marginal means
#' @param contrast_type Select the type of comparison for the estimated marginal means. Default is pairwise. See ?emmeans::`contrast-methods` for more details on acceptable methods.
#' @param emm_comp Set the comparisons for estimated marginal means comparisons. This is a factor name (a), combination of factor names (a+b), or for simple effects a | sign is needed (a|b)
#' @param emm_p_adjust Correction for multiple comparisons; default is "none". See ?summary.emmGrid for more details on acceptable methods.
#' @param nsims number of simulations to perform
#' @param seed Set seed for reproducible results
#' @param verbose Set to FALSE to not print results (default = TRUE)
#' @return Returns dataframe with simulation data (p-values and effect sizes), anova results (type 3 sums of squares) and simple effect results, and plots of p-value distribution.
#' 
#' \describe{
#'   \item{\code{"sim_data"}}{Output from every iteration of the simulation}
#'   \item{\code{"main_result"}}{The power analysis results for ANOVA effects.}
#'   \item{\code{"pc_results"}}{The power analysis results for pairwise comparisons.}
#'   \item{\code{"manova_results"}}{Default is "NULL". If a within-subjects factor is included, then the power of the multivariate (i.e. MANOVA) analyses will be provided.}
#'   \item{\code{"emm_results"}}{The power analysis results of the estimated marginal means.}
#'   \item{\code{"plot1"}}{Distribution of p-values from the ANOVA results.}
#'   \item{\code{"plot2"}}{Distribution of p-values from the pairwise comparisons results.}
#'   \item{\code{"correction"}}{The correction for sphericity applied to the simulation results.}
#'   \item{\code{"p_adjust"}}{The p-value adjustment applied to the simulation results for ANOVA/MANOVA omnibus tests and t-tests.}
#'   \item{\code{"emm_p_adjust"}}{The p-value adjustment applied to the simulation results for the estimated marginal means.}
#'   \item{\code{"nsims"}}{The number of simulations run.}
#'   \item{\code{"alpha_level"}}{The alpha level, significance cut-off, used for the power analysis.}
#'   \item{\code{"method"}}{Record of the function used to produce the simulation}
#' 
#' }
#' 
#' @examples
#' \dontrun{
#' ## Set up a within design with 2 factors, each with 2 levels,
#' ## with correlation between observations of 0.8,
#' ## 40 participants (who do all conditions), and standard deviation of 2
#' ## with a mean pattern of 1, 0, 1, 0, conditions labeled 'condition' and
#' ## 'voice', with names for levels of "cheerful", "sad", amd "human", "robot"
#' design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0),
#'       sd = 2, r = 0.8, labelnames = c("condition", "cheerful",
#'       "sad", "voice", "human", "robot"))
#' power_result <- ANOVA_power(design_result, alpha_level = 0.05,
#'       p_adjust = "none", seed = 2019, nsims = 10)
#'       }
#' @section References:
#' too be added
#' @importFrom stats pnorm pt qnorm qt as.formula median p.adjust pf sd power
#' @importFrom utils combn
#' @importFrom graphics pairs
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom parallel mc.reset.stream
#' @import pbmcapply
#' @import emmeans
#' @import ggplot2
#' @export
#'

ANOVA_power <- function(design_result, 
                        alpha_level = Superpower_options("alpha_level"), 
                        correction = Superpower_options("correction"),
                        p_adjust = "none", nsims = 1000, seed = NULL,
                        verbose = Superpower_options("verbose"),
                        emm = Superpower_options("emm"),
                        emm_model = Superpower_options("emm_model"),
                        contrast_type = Superpower_options("contrast_type"),
                        emm_p_adjust = "none",
                        emm_comp = NULL){
  
  #Need this to avoid "undefined" global error from occuring
  cohen_f <- partial_eta_squared <- non_centrality <- NULL
  
  #New checks for emmeans input
  if (is.null(emm)) {
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
                     "mean_chg",
                     "dunnett",
                     "tukey"
                   )) == FALSE ) {
      stop("contrast_type must be of an accepted format. 
           The tukey & dunnett options are not appropriate for models with within subjects factors. 
           See help(\"contrast-methods\") for details on the exact methods")
    }
    
    
    if (is.element(emm_p_adjust, 
                   c("dunnett",
                     "tukey",
                     "sidak",
                     "scheffe",
                     "dunnettx",
                     "mvt",
                     "holm", 
                     "hochberg", 
                     "hommel", 
                     "bonferroni", 
                     "BH", 
                     "BY", 
                     "fdr",
                     "none")) == FALSE ) {
      stop("emm_p_adjust must be of an acceptable format.
           See ?summary.emmGrid for details on the exact methods.")
    }
  }
  
  if (is.element(p_adjust, c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) == FALSE ) {
    stop("p_adjust must be of an acceptable adjustment method: see ?p.adjust")
  }
  
  if (is.element(correction, c("none", "GG", "HF")) == FALSE ) {
    stop("Correction for sphericity can only be none, GG, or HF")
  }
  
  if (nsims < 10) {
    stop("The number of repetitions in simulation must be at least 10; suggested at least 1000 for accurate results")
  }
  
  #Check to ensure there is a within subject factor -- if none --> no MANOVA
  run_manova <- grepl("w", design_result$design)
  
  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }
  
  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
  }
  
  ###############
  # 2. Read in Environment Data ----
  ###############
  
  design <- design_result$design #String used to specify the design
  
  if (grepl("w",design) && is.element(emm_p_adjust,
                                      c("dunnett",
                                        "tukey",
                                        "sidak",
                                        "scheffe",
                                        "dunnettx")) == TRUE ) {
    warning(
      "The emm_p_adjust selection is inappropriate for the specified design.
           Consider fdr or holm corrections. See ?summary.emmGrid"
    )
  }
  
  factornames <- design_result$factornames #Get factor names
  n <- design_result$n
  mu = design_result$mu # population means - should match up with the design
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  factors <- design_result$factors
  design_factors <- design_result$design_factors
  sigmatrix <- design_result$sigmatrix
  dataframe <- design_result$dataframe
  design_list <- design_result$design_list
  
  #Block this logical in Shiny context (at least for now)
  #to allow different n per condition:
  if (grepl("w", design_result$design) == TRUE && length(unique(design_result$n)) > 1)  {
    stop("Unequal group sizes are not possible when the design contains within factors")
  }
  n_vec <- n # store vector n as n - this is because the code below uses n as a single number, so quick fix for legacy reasons
  n <- max(n) # now set n to max n for ANOVA_design function
  
  ###############
  # 3. Specify factors for formula ----
  ###############
  
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
                                          data = dataframe, include_aov = FALSE,
                                          anova_table = list(es = "pes", 
                                                             p_adjust_method = p_adjust)) }) #This reports PES not GES
  if (emm == TRUE) {
    #Call emmeans with specifcations given in the function
    #Limited to specs and model
    if (missing(emm_comp)) {
      emm_comp = as.character(frml2)[2]
    }
    
    specs_formula <- as.formula(paste(contrast_type," ~ ",emm_comp))
    emm_result <- suppressMessages({emmeans(aov_result, 
                                            specs = specs_formula,
                                            model = emm_model,
                                            adjust = emm_p_adjust)})
    #plot_emm = plot(emm_result, comparisons = TRUE)
    #make comparison based on specs; adjust = "none" in exact; No solution for multcomp in exact simulation
    pairs_result_df <- emmeans_power(emm_result$contrasts, alpha_level = alpha_level)
    
    #rownames from contrasts non readable sticking to row number
    #rownames(pairs_result_df) <- as.character(pairs_result_df$contrast)
    #pairs_result_df$contrast <- NULL
    
    
    emm_sim_data <- as.data.frame(matrix(
      ncol = nrow(pairs_result_df)*2,
      nrow = nsims))
    
    names(emm_sim_data) = c(paste("p_",
                                  rownames(pairs_result_df),
                                  sep = ""),
                            paste("cohen_f_",
                                  rownames(pairs_result_df),
                                  sep = ""))
  } else{
    pairs_result_df = NULL
  }
  
  #Run MANOVA if within subject factor is included; otherwise ignored
  if (run_manova == TRUE) {
    manova_result <- Anova_mlm_table(aov_result$Anova)
  }
  ###############
  # 5. Set up dataframe for simulation results
  ###############
  
  #How many possible planned comparisons are there (to store p and es)
  possible_pc <- (((prod(
    as.numeric(strsplit(design, "\\D+")[[1]])
  )) ^ 2) - prod(as.numeric(strsplit(design, "\\D+")[[1]])))/2
  
  #create empty dataframe to store simulation results
  #number of columns for ANOVA results and planned comparisons, times 2 (p-values and effect sizes)
  
  
  
  paired_tests <- combn(unique(dataframe$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[lower.tri(sigmatrix)] #based on whether correlation is 0 or not, we can determine if we should run a paired or independent t-test
  
  #Dynamically create names for the data we will store
  #Again create rownames based on whether or not a MANOVA should be included

  
  
  ###############
  # 7. Start Simulation ----
  ###############
  #Set seed, from sim_design function by Lisa DeBruine
  if (!is.null(seed)) {
    # reinstate system seed after simulation
    sysSeed <- .GlobalEnv$.Random.seed
    on.exit({
      if (!is.null(sysSeed)) {
        .GlobalEnv$.Random.seed <- sysSeed
      } else {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    })
    mc.reset.stream()
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
  }
  
  if (length(grep("windows", Sys.info()["sysname"], ignore.case = TRUE))) {
    # Windows Machine does not support mc(*)apply
    x1 <- pbmclapply(1:nsims, gen_anova, 
                     design_result = design_result,
                     p_adjust = p_adjust,
                     correction = correction,
                     run_manova = run_manova,
                     emm = emm,
                     emm_model = emm_model,
                     emm_p_adjust = emm_p_adjust,
                     emm_comp = emm_comp,
                     aov_result = aov_result,
                     manova_result = manova_result,
                     contrast_type = contrast_type,
                     alpha_level = alpha_level,
                     mc.cores = 1)
  } else {
    x1 <- pbmclapply(1:nsims, gen_anova, 
                     design_result = design_result,
                     p_adjust = p_adjust,
                     correction = correction,
                     run_manova = run_manova,
                     emm = emm,
                     emm_model = emm_model,
                     emm_p_adjust = emm_p_adjust,
                     emm_comp = emm_comp,
                     aov_result = aov_result,
                     manova_result = manova_result,
                     contrast_type = contrast_type,
                     alpha_level = alpha_level)
  }
  
  sim_data = map(x1, "sim_data") %>%
    bind_rows()
  
  emm_sim_data = map(x1, "emm_sim_data") %>%
    bind_rows()
  ############################################
  #End Simulation              ###############
  
  
  ###############
  # 8. Plot Results ----
  ###############
  
  # melt the data into a long format for plots in ggplot2
  
  plotData <- suppressMessages(melt(sim_data[1:(2 ^ factors - 1)], value.name = 'p'))
  
  SalientLineColor <- "#535353"
  LineColor <- "Black"
  BackgroundColor <- "White"
  
  # plot each of the p-value distributions
  #create variable p to use in ggplot and prevent package check error.
  p <- plotData$p
  # Helper function for string wrapping.
  swr = function(string, nwrap = 10) {
    paste(strwrap(string, width = 10), collapse = "\n")
  }
  swr = Vectorize(swr)
  
  # Create line breaks in variable
  plotData$variable = swr(chartr("_:", "  ", plotData$variable))
  
  plt1 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "black",
                   fill = "white",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = "p") +
    theme_bw()
  #Plot p-value distributions for simple comparisons
  # melt the data into a ggplot friendly 'long' format
  p_paired <- sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]
  
  plotData <- suppressMessages(melt(p_paired, value.name = 'p'))
  #create variable p to use in ggplot and prevent package check error.
  p <- plotData$p
  # Create line breaks in variable
  plotData$variable = swr(chartr("_:", "  ", plotData$variable))
  
  # plot each of the p-value distributions
  plt2 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "black",
                   fill = "white",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = expression(p)) +
    theme_bw()
  ###############
  # 9. Sumary of power and effect sizes of main effects and contrasts ----
  ###############
  
  #Main effects and interactions from the ANOVA
  power = as.data.frame(apply(as.matrix(sim_data[(1:(2 ^ factors - 1))]), 2,
                              function(x) mean(ifelse(x < alpha_level, 1, 0) * 100)))
  
  es = as.data.frame(apply(as.matrix(sim_data[((2^factors):(2 * (2 ^ factors - 1)))]), 2,
                           function(x) mean(x)))
  
  main_results <- data.frame(power,es)
  names(main_results) = c("power","effect_size")
  
  #Data summary for pairwise comparisons
  power_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]), 2,
                                     function(x) mean(ifelse(x < alpha_level, 1, 0) * 100)))
  
  es_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + possible_pc + 1):(2*(2 ^ factors - 1) + 2 * possible_pc)]), 2,
                                  function(x) mean(x)))
  
  pc_results <- data.frame(power_paired, es_paired)
  names(pc_results) = c("power","effect_size")
  
  #Data summary for emmeans
  if (emm == TRUE) {
    emm_power = as.data.frame(apply(as.matrix(emm_sim_data[(1):(nrow(pairs_result_df))]), 2,
                                    function(x) mean(ifelse(x < alpha_level, 1, 0) * 100)))
    emm_es = as.data.frame(apply(as.matrix(emm_sim_data[((nrow(pairs_result_df) + 1):(nrow(pairs_result_df)*2))]), 2,
                                 function(x) mean(x)))
    
    emm_results <- data.frame(pairs_result_df$contrast,emm_power, emm_es)
    names(emm_results) = c("contrast","power","cohen_f")
  } else{
    emm_results = NULL
  }
  
  #Simulation results from MANOVA
  if (run_manova == TRUE) {
    power_MANOVA = as.data.frame(apply(as.matrix(sim_data[((2*(2 ^ factors - 1) + 2 * possible_pc + 1):(2 ^ factors + (2*(2 ^ factors - 1) + 2 * possible_pc)))]), 2,
                                       function(x) mean(ifelse(x < alpha_level, 1, 0) * 100)))
    
    manova_result <- data.frame(power_MANOVA)
    names(manova_result) = c("power")
  }
  
  #######################
  # Return Results ----
  #######################
  if (verbose == TRUE) {
    # The section below should be blocked out when in Shiny
    cat("Power and Effect sizes for ANOVA tests")
    cat("\n")
    print(main_results, digits = 4)
    cat("\n")
    cat("Power and Effect sizes for pairwise comparisons (t-tests)")
    cat("\n")
    print(pc_results, digits = 4)
    cat("\n")
    if (emm == TRUE) {
      cat("Power and Cohen's f from estimated marginal means")
      cat("\n")
      print(emm_results, digits = 4)
      cat("\n")
    }
    if (run_manova == TRUE) {
      cat("\n")
      cat("Within-Subject Factors Included: Check MANOVA Results")
      cat("\n")
    }
    cat("\n")
  }
  
  #Create empty value if no MANOVA results are included
  if (run_manova == FALSE) {
    manova_result = NULL
  }
  
  # Return results in list()

  structure(list(sim_data = sim_data,
                 main_results = main_results,
                 pc_results = pc_results,
                 manova_results = manova_result,
                 emm_results = emm_results,
                 plot1 = plt1,
                 plot2 = plt2,
                 correction = correction,
                 p_adjust = p_adjust,
                 emm_p_adjust = emm_p_adjust,
                 nsims = nsims,
                 alpha_level = alpha_level,
                 method = "ANOVA_power"),
            class = "sim_result")
}
