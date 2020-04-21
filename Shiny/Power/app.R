# ANOVA_power app
# simulation function Shiny app
#

library(shiny)
library(shinyMatrix)
library(shinyjs)
library(shinydashboard)
library(Superpower)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(afex)
library(emmeans)
library(ggplot2)
library(reshape2)
library(MASS)
library(magrittr)
library(dplyr)

Superpower_options(emm = TRUE,
                   verbose = FALSE,
                   plot = FALSE)

shiny_power <- function(design_result, 
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
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
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
  
  #to allow different n per condition:
  #if (grepl("w", design_result$design) == TRUE && length(unique(design_result$n)) > 1)  {
  #  stop("Unequal group sizes are not possible when the design contains within factors")
  #}
  n_vec <- n # store vector n as n - this is because the code below uses n as a single number, so quick fix for legacy reasons
  n <- max(n) # now set n to max n for ANOVA_design function
  
  ###############
  # 3. Specify factors for formula ----
  ###############
  
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
                                          data = dataframe, include_aov = FALSE,
                                          anova_table = list(es = "pes", p_adjust_method = p_adjust)) }) #This reports PES not GES
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
    pairs_result <- emm_result$contrasts
    pairs_result_df <- as.data.frame(pairs_result)
    #Need for exact; not necessary for power function
    #Convert t-ratio to F-stat
    pairs_result_df$F.value <- (pairs_result_df$t.ratio)^2
    #Calculate pes -- The formula for partial eta-squared is equation 13 from Lakens (2013)
    pairs_result_df$pes <- pairs_result_df$F.value/(pairs_result_df$F.value + pairs_result_df$df) 
    #Calculate cohen's f
    pairs_result_df$f2 <- pairs_result_df$pes/(1 - pairs_result_df$pes)
    #Calculate noncentrality
    pairs_result_df$lambda <- pairs_result_df$f2*pairs_result_df$df
    #minusalpha<- 1-alpha_level
    pairs_result_df$Ft <- qf((1 - alpha_level), 1, pairs_result_df$df)
    #Calculate power
    pairs_result_df$power <- (1 - pf(pairs_result_df$Ft, 1, pairs_result_df$df, pairs_result_df$lambda))*100
    
    pairs_result_df <- pairs_result_df %>% mutate(partial_eta_squared = .data$pes,
                                                  cohen_f = sqrt(.data$f2),
                                                  non_centrality = .data$lambda) %>%
      select(-.data$p.value,-.data$F.value,-.data$t.ratio,-.data$Ft,-.data$SE,
             -.data$f2,-.data$lambda,-.data$pes, -.data$estimate, -.data$df) %>%
      select(-.data$power, -.data$partial_eta_squared, -.data$cohen_f, -.data$non_centrality,
             .data$power, .data$partial_eta_squared, .data$cohen_f, .data$non_centrality)
    
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
    manova_result <- Superpower:::Anova_mlm_table(aov_result$Anova)
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
  
  if (run_manova == TRUE) {
    #create empty dataframe to store simulation results
    #number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
    #more columns added if MANOVA output included 2^factors
    sim_data <- as.data.frame(matrix(
      ncol = 2 * (2 ^ factors - 1) + (2 ^ factors) + 2 * possible_pc,
      nrow = nsims
    )) } else {
      
      sim_data <- as.data.frame(matrix(
        ncol = 2 * (2 ^ factors - 1) + 2 * possible_pc,
        nrow = nsims
      ))
      
    }
  
  
  paired_tests <- combn(unique(dataframe$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[lower.tri(sigmatrix)] #based on whether correlation is 0 or not, we can determine if we should run a paired or independent t-test
  
  #Dynamically create names for the data we will store
  #Again create rownames based on whether or not a MANOVA should be included
  if (run_manova == TRUE) {
    names(sim_data) = c(paste("anova_",
                              rownames(aov_result$anova_table),
                              sep = ""),
                        paste("anova_es_",
                              rownames(aov_result$anova_table),
                              sep = ""),
                        paste("p_",
                              paste(paired_tests[1,],paired_tests[2,],sep = "_"),
                              sep = ""),
                        paste("d_",
                              paste(paired_tests[1,],paired_tests[2,], sep = "_"),
                              sep = ""),
                        paste("manova_",
                              rownames(manova_result),
                              sep = ""))
  } else {
    names(sim_data) = c(paste("anova_",
                              rownames(aov_result$anova_table),
                              sep = ""),
                        paste("anova_es_",
                              rownames(aov_result$anova_table),
                              sep = ""),
                        paste("p_",
                              paste(paired_tests[1,],paired_tests[2,],sep = "_"),
                              sep = ""),
                        paste("d_",
                              paste(paired_tests[1,],paired_tests[2,], sep = "_"),
                              sep = ""))
  }
  
  
  ###############
  # 7. Start Simulation ----
  ###############
  withProgress(message = 'Running simulations', value = 0, { #block outside of Shiny
    for (i in 1:nsims) { #for each simulated experiment
      incProgress(1/nsims, detail = paste("Now running simulation", i, "out of",nsims,"simulations")) #Block outside of Shiny
      #We simulate a new y variable, melt it in long format, and add it to the dataframe (surpressing messages)
      
      dataframe <- design_result$dataframe # read in dataframe again, because we deleted rows from it below if unequal n
      
      dataframe$y <- suppressMessages({
        melt(as.data.frame(mvrnorm(
          n = n,
          mu = mu,
          Sigma = as.matrix(sigmatrix)
        )))$value
      })
      
      #NEW SECTION TO ALLOW UNEQUAL N
      #need if for single n
      if (length(n_vec) > 1) {
        for (k in 1:length(unique(dataframe$cond))) {
          #for each unique condition
          if ((n - n_vec[k]) > 0) {
            #only sample if we want to remove more than 0 rows
            dataframe <-
              dataframe[-sample(which(dataframe$cond  == unique(dataframe$cond)[k]) , (n -
                                                                                         n_vec[k])) ,]
          }
        }
      }
      # We perform the ANOVA using AFEX
      #Can be set to NICE to speed up, but required data grabbing from output the change.
      aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design
                                              data = dataframe, include_aov = FALSE, #Need development code to get aov_include function
                                              anova_table = list(es = "pes",
                                                                 p_adjust_method = p_adjust,
                                                                 correction = correction))}) #This reports PES not GES
      if (emm == TRUE) {
        emm_result <- suppressMessages({emmeans(aov_result, 
                                                specs = specs_formula,
                                                model = emm_model,
                                                adjust = emm_p_adjust)})
        #plot_emm = plot(emm_result, comparisons = TRUE)
        #make comparison based on specs; adjust = "none" in exact; No solution for multcomp in exact simulation
        pairs_result <- emm_result$contrasts
        pairs_result_df <- as.data.frame(pairs_result)
        #Need for exact; not necessary for power function
        #Convert t-ratio to F-stat
        pairs_result_df$F.value <- (pairs_result_df$t.ratio)^2
        #Calculate pes -- The formula for partial eta-squared is equation 13 from Lakens (2013)
        pairs_result_df$pes <- pairs_result_df$F.value/(pairs_result_df$F.value + pairs_result_df$df) 
        #Calculate cohen's f
        pairs_result_df$f2 <- pairs_result_df$pes/(1 - pairs_result_df$pes)
        
        
        pairs_result_df <- pairs_result_df %>% mutate(cohen_f = sqrt(.data$f2)) %>%
          select(-.data$F.value,-.data$t.ratio,-.data$SE,
                 -.data$f2,-.data$pes, -.data$estimate, -.data$df) %>%
          select(-.data$cohen_f, -.data$p.value,
                 .data$p.value, .data$cohen_f)
        
        emm_sim_data[i,] <- c(as.numeric(pairs_result_df$p.value), #p-value for contrast
                              as.numeric(pairs_result_df$cohen_f) #cohen f
        ) #
      }
      
      # Store MANOVA result if there are within subject factors
      if (run_manova == TRUE) {
        manova_result <- Superpower:::Anova_mlm_table(aov_result$Anova)
        manova_result$p.value <- p.adjust(manova_result$p.value, method = p_adjust)
      }
      
      for (j in 1:possible_pc) {
        x <- dataframe$y[which(dataframe$cond == paired_tests[1,j])]
        y <- dataframe$y[which(dataframe$cond == paired_tests[2,j])]
        #this can be sped up by tweaking the functions that are loaded to only give p and dz
        ifelse(within_between[j] == 0,
               t_test_res <- Superpower:::effect_size_d(x, y, alpha_level = alpha_level),
               t_test_res <- Superpower:::effect_size_d_paired(x, y, alpha_level = alpha_level))
        paired_p[j] <- t_test_res$p_value
        paired_d[j] <- ifelse(within_between[j] == 0,
                              t_test_res$d,
                              t_test_res$d_z)
      }
      
      # store p-values and effect sizes for calculations and plots.
      #If needed to create different row names if MANOVA is included
      if (run_manova == TRUE) {
        sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                          aov_result$anova_table[[5]], #partial eta squared
                          p.adjust(paired_p, method = p_adjust), #p-values for paired comparisons
                          paired_d, #effect sizes
                          manova_result[[6]]) #p-values for MANOVA
      } else {
        sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                          aov_result$anova_table[[5]], #partial eta squared
                          p.adjust(paired_p, method = p_adjust), #p-values for paired comparisons
                          paired_d) #effect sizes
      }
      
      
    }
  }) #close withProgress Block outside of Shiny
  
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
  
  #Create empty value if no MANOVA results are included
  if (run_manova == FALSE) {
    manova_result = NULL
  }
  
  # Return results in list()
  invisible(list(sim_data = sim_data,
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
                 alpha_level = alpha_level))
}


label_function <- function(design, labelnames = NULL) {
  #If labelnames are not provided, they are generated.
  #Store factor levels (used many times in the script, calculate once)
  factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
  
  if (is.null(labelnames)) {
    for (i1 in 1:length(factor_levels)){
      labelnames <- append(labelnames,paste(paste(letters[i1]), sep = ""))
      for (i2 in 1:factor_levels[i1]){
        labelnames <- append(labelnames,paste(paste(letters[i1]), paste(i2), sep = ""))
      }
    }
  }
  
  if (length(labelnames) != length(factor_levels) + sum(factor_levels)) {
    stop("Variable 'design' does not match the length of the labelnames")
  }
  
  ###############
  # 1. Specify Design and Simulation----
  ###############
  # String used to specify the design
  # Add numbers for each factor with 2 levels, e.g., 2 for a factor with 2 levels
  # Add a 'w' after the number for within factors, and a 'b' for between factors
  # Separate factors with a * (asterisk)
  # Thus "2b*3w) is a design with 2 between levels, and 3 within levels
  
  
  #Check if the design and sd match (either 1 or length of design)
  #if(length(sd) != 1 && prod(factor_levels) != length(sd)){stop("The SD must be a length of 1 or match the length of the study design")}
  
  #Check if the factors are of an acceptable number of levels
  if(any(factor_levels <= 0) == TRUE | any(factor_levels > 99) ) {
    stop("Each factor can only have between 2 and 99 levels")
  }
  
  ###############
  # 2. Create Factors and Design ----
  ###############
  
  #Count number of factors in design
  factors <- length(factor_levels)
  
  #Get factor names and labelnameslist
  labelnames1 <- labelnames[(1 + 1):(1+factor_levels[1])]
  if(factors > 1){labelnames2 <- labelnames[(factor_levels[1] + 3):((factor_levels[1] + 3) + factor_levels[2] - 1)]}
  if(factors > 2){labelnames3 <- labelnames[(factor_levels[2] + factor_levels[1] + 4):((factor_levels[2] + factor_levels[1] + 4) + factor_levels[3] - 1)]}
  
  factornames1 <- labelnames[1]
  if(factors > 1){factornames2 <- labelnames[factor_levels[1] + 2]}
  if(factors > 2){factornames3 <- labelnames[factor_levels[2] + factor_levels[1] + 3]}
  
  if(factors == 1){labelnameslist <- list(labelnames1)}
  if(factors == 2){labelnameslist <- list(labelnames1,labelnames2)}
  if(factors == 3){labelnameslist <- list(labelnames1,labelnames2,labelnames3)}
  
  if(factors == 1){factornames <- c(factornames1)}
  if(factors == 2){factornames <- c(factornames1,factornames2)}
  if(factors == 3){factornames <- c(factornames1,factornames2,factornames3)}
  
  #Specify within/between factors in design: Factors that are within are 1, between 0
  design_factors <- strsplit(gsub("[^A-Za-z]","",design),"",fixed = TRUE)[[1]]
  design_factors <- as.numeric(design_factors == "w") #if within design, set value to 1, otherwise to 0
  
  #Specify design list (similar as below)
  xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
  for(j in 1:factors){
    xxx <- cbind(xxx, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                         sep="_")),
                                           each = prod(factor_levels)/prod(factor_levels[1:j]),
                                           times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  design_list <- as.character(interaction(xxx[, 1:factors], sep = "_"))
  paste(design_list)
}

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA_power"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info_tab", icon = icon("info-circle")),
      menuItem("Design", tabName = "design_tab", icon = icon("bezier-curve")),
      menuItem("Power Simulation", tabName = "exact_tab", icon = icon("calculator")),
      conditionalPanel("input.sim >= 1",
                       downloadButton("report", "Download PDF Report")
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "info_tab",
              box(
                title = "Using this App",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                h5("This Shiny app is for performing Monte Carlo simuations of factorial experimental designs in order to estimate power for an ANOVA and follow-up pairwise comparisons.
                   This app allows you to violate the assumptions of homoscedascity and sphecity (for repeated measures). Also, the simulations take a considerable amount of time to run.
                   If you don't need/want to violate these assumptions please use the ANOVA_exact app."),
                a("Click here for the other app", href = "http://shiny.ieis.tue.nl/anova_exact/"),
                h3("The Design Tab"),
                h5("You must start with the Design tab in order to perform a power analysis. At this stage you must establish the parameters of the design (sample size, standard deviation, etc).
                   Once you click Submit Design the design details will appear and you can continue onto the power analysis."),
                h3("Power Simulation Tab"),
                h5("In this tab, you will setup the Monte Carlo simulation. You will have to specify a correction for multiple comparisons (default=none) and the alpha level (default=.05).
                   If you have repeated measures you will need to specify the sphericity correction (default=none)."),
                h3("Download your Simulation"),
                h5("Once your simulation is completed a button a button will appear on the sidebar to download a PDF")
              ),              
              box(
                title = "NEWS",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                strong("Current updates to Superpower's Power Shiny App"),
                h5("Option for estimated marginal means added"),
                h5("Now allows for unequal n input")),              
              box(
                title = "CITATION",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                strong("Please cite as:"),
                h5("Lakens, D., & Caldwell, A. R. (2019). \"Simulation-Based Power-Analysis for Factorial ANOVA Designs\". PsyArXiv, <https://doi.org/10.31234/osf.io/baxsf>.")
              )),
      # Design content
      tabItem(tabName = "design_tab",
              fluidRow(

                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  strong("Specify the factorial design below"), br(),
                  "*Must be specficied to continue*",

                  h5("Add numbers that specify the number of levels in the factors (e.g., 2 for a factor with 2 levels). Add a 'w' after the number for within factors, or 'b' for between factors. Seperate factors with an asterisks. Thus '2b*3w' is a design with two factors, the first of which has 2 between levels, and the second of which has 3 within levels."),

                  textInput(inputId = "design", label = "Design Input",
                            value = "2b*2w"),
                  
                  selectInput("labelChoice", "Would you like to enter factor and level names?",
                              c("No" = "no" ,
                                "Yes" = "yes" )),
                  
                  conditionalPanel(condition = "input.labelChoice == 'yes'",
                                   h5("Specify one word for each factor (e.g., AGE and SPEED) and the level of each factor (e.g., old and young for a factor age with 2 levels). First, name the factor (e.g., AGE) then name the levels (e.g., old and young)"),
                                   
                                   textInput("labelnames", label = "Factor & level labels",
                                             value = "AGE,old,young,SPEED,fast,slow")),

                  #h5("Specify one word for each factor (e.g., AGE and SPEED) and the level of each factor (e.g., old and young for a factor age with 2 levels)."),
                  #uiOutput("labelnames"),
                  
                  selectInput("nChoice", "Would you like to enter different sample sizes per cell?",
                              c("No" = "no" ,
                                "Yes" = "yes"
                              )),
                  
                  conditionalPanel(condition = "input.nChoice == 'no'",
                  uiOutput("sample_size")),
                  
                  conditionalPanel(condition = "input.nChoice == 'yes'",
                                   strong("Sample Size per Cell"),
                                   uiOutput("sample_size2")),
                  
                  selectInput("sdChoice", "Would you like to enter multiple standard deviations? 
                              *Warning: Violates homoscedascity assumption*",
                              c("No" = "no" ,
                                "Yes" = "yes"
                                )),
                  
                  conditionalPanel(condition = "input.sdChoice == 'yes'",                  
                  strong("Specify the list of standard deviations."),
                  uiOutput("sdMatrix") ),

                  conditionalPanel(condition = "input.sdChoice == 'no'", 
                  numericInput(inputId = "sd_common", label = "Common Standard Deviation",
                            min = 0,
                            step = .1,
                            value = 1.03)),
                  conditionalPanel(condition = "output.corr_display == true",
                  selectInput("rChoice", "Would you like to enter a correlation matrix (rather than a single correlation)? 
                              *Warning: may violate sphericity assumption*",
                              c("No" = "no" ,
                                "Yes" = "yes"
                              )),
                  
                  conditionalPanel(condition = "input.rChoice == 'no'", 
                                   numericInput(inputId = "r_common", label = "Common correlation among within-subjects factors", min = 0, max=.999999999999999, step = .05, value = .5)),

                  conditionalPanel(condition = "input.rChoice == 'yes'", 
                                   strong("Specify the correlation matrix. 
                                          If a correlation is entered across between subjects factor it will be reset to zero.
                                          Values only need to be entered in the upper triangular part and the app will take it from there."),
                                   
                                   uiOutput("rMatrix"))),

                  h5("Note that for each cell in the design, a mean must be provided. Thus, for a '2b*3w' design, 6 means need to be entered. Means need to be entered in the correct order. The app provides a plot so you can check if you entered means correctly."),

                  strong("Means for Each Cell in the Design"),
                  
                  uiOutput("muMatrix"),

                  #Button to initiate the design
                  h5("Click the button below to set up the design - Check the output to see if the design is as you intended, then you can run the simulation on the next tab."),
                  actionButton("designBut","Set-Up Design",
                               icon = icon("check-square"))

              ),
              
              box(
                title = "Design Output", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                verbatimTextOutput("DESIGN"),
                conditionalPanel("input.nChoice == 'yes'",
                                 paste("WARNING: Ensure sample sizes are the same between levels of within subjects factors (Otherwise the app will crash).")),
                plotOutput('plot'),
                tableOutput("corMat"),
                tableOutput("covMat")
              )
      )
      ),

      # Exact Power content
      tabItem(tabName = "exact_tab",
              h2("Simulate Power for Design"),

              fluidRow(

                box(
                  title = "Simulation Parameters", status = "warning", solidHeader = TRUE,
                  
                  
                  

                  conditionalPanel("input.designBut >= 1",
                                   selectInput("seedChoice", "Would you like to set a \"seed\" for reproducible simulations?",
                                               c("No" = "no" ,
                                                 "Yes" = "yes" )),
                                   conditionalPanel(condition = "input.seedChoice == 'yes'",
                                                    h5("Specify a seed number."),
                                                    
                                                    numericInput("seed_num", label = "Seed Number",
                                                              value = 2019, min = 1,max=1000000000000, step=1)),
                  selectInput("correction", "Sphericity Correction",
                              c("None" = "none",
                                "Greenhouse-Geisser" = "GG",
                                "Huynh-Feldt" = "HF")),
                  selectInput("emm", "Would you like to compare the estimated marginal means?",
                              c("No" = "no",
                                "Yes" = "yes"
                              )),
                  conditionalPanel("input.emm == 'yes'",
                                   h5("Keeping the default settings will result in all pairwise comparisons being performed."),
                                   selectInput("emm_model", "What model would you like to use for the estimated marginal means",
                                               c("Univariate" = "univariate",
                                                 "Multivariate" = "multivariate")),
                                   selectInput("contrast_type", "What type of comparisons would you like to make?",
                                               c("Pairwise" = "pairwise",
                                                 "Polynomial contrast" = "poly",
                                                 "Helmert" = "consec",
                                                 "Compare each level with the average over all levels" = "eff")),
                                   selectInput("emm_p_adjust", "What correction for multiple comparisons would you like to make for the estimated marginal means?
                                               Warning: Tukey and Scheffe are not appropriate when there are within-subjects factors",
                                               c("Holm-Bonferroni" = "holm",
                                                 "Bonferroni" = "bonferroni",
                                                 "False Discovery Rate" = "fdr",
                                                 "None" = "none",
                                                 "Tukey-Kramer"= "tukey",
                                                 "Scheffe" = "scheffe")),
                                   textInput(inputId = "emm_comp", 
                                             label = "What comparisons would you like to make with estimated marginal means?",
                                             value = "a + b"),
                                   textOutput("emm_formula"),
                                   h5("The addition sign ('+') will add factors for comparisons while factors after a vertical bar '|' specifies the names of predictors to condition on"),
                                   a("For more information on setting comparisons", href = "https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#formulas")
                  ),
                  
                  selectInput("padjust", "Select adjustment for multiple comparisons (Note: this is meant for *exploratory* ANOVAs). This will adjust the ANOVA-level and t-test (pairwise) comparison effects.",
                              c("None" = "none",
                                "Bonferroni" = "bonferroni",
                                "Holm-Bonferroni" = "holm",
                                "False Discovery Rate" = "fdr")),
                  
                  numericInput("nsims",
                              label = "Number of Simulations",
                              min = 100, step = 100, value = 2000),

                  #sliderInput("sig",
                  #            label = "Alpha Level",
                  #            min = 0, max = .2, value = 0.05),
                  
                  numericInput("sig", label = "Alpha Level", value = .05, 
                               min = .0000000000000000000000000000000001, 
                               max = 1,
                               step = .001),

                  actionButton("sim", "Show Results of Simulation",
                               icon = icon("print"))

                  )

                ),
                box(
                  title = "Power Analysis Output", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput('tableMain'),
                  conditionalPanel("input.emm == 'no'",
                                   tableOutput('tablePC')),
                  conditionalPanel("input.emm == 'yes'",
                                   tableOutput('tableEMM'))
                  
                  
                ) 
              )


      )
    ) #end tabItems
  ) ,#end dashboardBody
skin = "purple")


###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Define server logic
server <- function(input, output, session) {

  #Create set of reactive values
  values <- reactiveValues(design_result = 0,
                           power_result = 0,
                           power_curve = 0,
                           label_list = 0,
                           emm_output = 0)
  
values$label_list <- reactive({  
  
  design_labels <- ANOVA_design(design = input$design,
                                n = prod(
                                  as.numeric(
                                    unlist(
                                      regmatches
                                      (input$design,
                                        gregexpr("[[:digit:]]+",
                                                 input$design))))),
                                r = .5,
                                sd = as.numeric(matrix(c(1), 1, 
                                                       prod(as.numeric(strsplit(input$design, "\\D+")[[1]])))),
                                mu = as.numeric(matrix(c(1), 1, 
                                                       prod(as.numeric(strsplit(input$design, "\\D+")[[1]])))))
  colnames(design_labels$sigmatrix)

})
  

  
 output$sdMatrix <-  renderUI({matrixInput(
   "sdMatrix",
   value = matrix(c(1), 1, 
                  prod(as.numeric(strsplit(input$design, "\\D+")[[1]])),
                  dimnames = list(c("sd"),
                                  c(label_function(input$design)))),
   rows = list(names = TRUE),
   cols = list(names = TRUE),
   copy = TRUE,
   paste = TRUE
 )
 })
 
 output$muMatrix <-  renderUI({matrixInput(
   "muMatrix",
   value = matrix(c(1), 1, 
                  prod(as.numeric(strsplit(input$design, "\\D+")[[1]])),
                  dimnames = list(c("mu"),
                                  c(label_function(input$design)))),
   rows = list(names = TRUE),
   cols = list(names = TRUE),
   copy = TRUE,
   paste = TRUE
 )
 })
 
 output$rMatrix <-  renderUI({matrixInput(
   "rMatrix",
   value = matrix(diag(prod(as.numeric(strsplit(input$design, "\\D+")[[1]]))), 
                  prod(as.numeric(strsplit(input$design, "\\D+")[[1]])), 
                  prod(as.numeric(strsplit(input$design, "\\D+")[[1]])),
                  dimnames = list(c(label_function(input$design)),
                                  c(label_function(input$design)))) ,
   rows = list(names = TRUE),
   cols = list(names = TRUE),
   class = "numeric",
   copy = TRUE,
   paste = TRUE
 )
 })


 output$corr_display = reactive(grepl("w",as.character(input$design)))
 outputOptions(output, "corr_display", suspendWhenHidden = FALSE)
 
 output$sample_size <- renderUI({
   numericInput("sample_size", 
                label = "Sample Size per Cell",
                min = prod(
                  as.numeric(
                    unlist(
                      regmatches
                      (input$design,
                        gregexpr("[[:digit:]]+",
                                 input$design))))),
                max = 1000, value = 80, step = 1)
 })
 
 output$sample_size2 <-  renderUI({matrixInput(
   "sample_size2",
   value = matrix(c(1), 1, 
                  prod(as.numeric(strsplit(input$design, "\\D+")[[1]])),
                  dimnames = list(c("n"),
                                  c(label_function(input$design)))),
   rows = list(names = TRUE),
   cols = list(names = TRUE),
   copy = TRUE,
   paste = TRUE
 )
 })
 
 #Old sample size dynamic input
 #output$sample_size <- renderUI({sliderInput("sample_size",
 #            label = "Sample Size per Cell",
 #            min = prod(
 #              as.numeric(
 #                unlist(
 #                  regmatches
 #                  (input$design,
 #                    gregexpr("[[:digit:]]+",
 #                             input$design))))),
 #            max = 1000, value = 80)
 #})



  observeEvent(input$designBut, {

    values$design_result <- suppressWarnings(ANOVA_design(design = as.character(input$design),
                                                                      n = if(input$nChoice =="yes"){
                                                                        as.numeric(input$sample_size2)
                                                                      }
                                                                        else{as.numeric(input$sample_size)},
                                                                      mu = as.numeric(input$muMatrix),
                                                                      labelnames = if (input$labelChoice == "yes"){
                                                                        as.vector(unlist(strsplit(gsub("[[:space:]]", "",input$labelnames), ",")))
                                                                        }else{
                                                                          NULL
                                                                        },
                                                                      sd = if(input$sdChoice == "yes"){
                                                                        as.numeric(input$sdMatrix)
                                                                      }else{
                                                                        as.numeric(input$sd_common)
                                                                      },
                                                                      r = if(input$rChoice == "yes"){
                                                                        as.numeric(Matrix::forceSymmetric(input$rMatrix, uplo="U"))
                                                                      }else{
                                                                        as.numeric(input$r_common)
                                                                      },
                                                                      plot = FALSE))
    values$emm_output <- as.character(values$design_result$frml2)[2] 
    updateTextInput(session, "emm_comp", value = values$emm_output)
    
  })

  output$emm_formula <- renderText({
    paste("Enter",as.character(values$design_result$frml2[2]), " above to receive results for all pairwise comparisons")})

  #Output text for ANOVA design
  output$DESIGN <- renderText({
    req(input$designBut)

    paste("The design is set as", values$design_result$string,
          "
          ",
          "Model formula: ", deparse(values$design_result$frml1),
          "
          ",
          "Sample size per cell n = ", deparse(values$design_result$n))
  })

  #Output of correlation and standard deviation matrix
  output$covMat <- renderTable(colnames = FALSE,
                               caption = "Variance-Covariance Matrix",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               {
                                 req(input$designBut)
                                 values$design_result$sigmatrix

                               })
  
  output$corMat <- renderTable(colnames = FALSE,
                               caption = "Correlation Matrix",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               {
                                 req(input$designBut)
                                 values$design_result$cor_mat
                                 
                               })
  #Output plot of the design
  output$plot <- renderPlot({
    req(input$designBut)
    values$design_result$meansplot})

  #Runs EXACT simulation and saves result as reactive value
  observeEvent(input$sim, {values$power_result <- shiny_power(values$design_result,
                                                              correction = input$correction,
                                                              p_adjust = input$padjust,
                                                              nsims = input$nsims,
                                                              alpha_level = input$sig,
                                                              verbose = FALSE,
                                                              emm = if (input$emm == "yes") {
                                                                TRUE
                                                              } else{FALSE},
                                                              emm_model = as.character(input$emm_model),
                                                              contrast_type = as.character(input$contrast_type),
                                                              emm_comp = as.character(input$emm_comp),
                                                              emm_p_adjust = as.character(input$emm_p_adjust),
                                                              seed = if (input$seedChoice == "no"){
                                                                NULL
                                                              } else {as.numeric(input$seed_num)})


  })

  #Table output of ANOVA level effects; rownames needed
  output$tableMain <-  renderTable({
    req(input$sim)
    values$power_result$main_results},
    caption = "Power for ANOVA Effects",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE)

  #Table output of pairwise comparisons; rownames needed
  output$tablePC <-  renderTable({
    req(input$sim)
    values$power_result$pc_result},
    caption = "Power for Pairwise Comparisons with t-tests",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE)

  output$tableEMM <-  renderTable({
    req(input$sim)
    values$power_result$emm_results},
    caption = "Power for Estimated Marginal Means Comparisons",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE)

  #Create downloadable report in markdown TINYTEX NEEDS TO BE INSTALLED
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(tablePC = values$power_result$pc_result,
                     tableMain = values$power_result$main_results,
                     tableEMM = values$power_result$emm_results,
                     means_plot = values$design_result$meansplot,
                     n = values$design_result$n,
                     model = deparse(values$design_result$frml1),
                     design = values$design_result$string,
                     cor_mat = values$design_result$cor_mat,
                     sigmatrix = values$design_result$sigmatrix,
                     alpha_level = values$power_result$alpha_level,
                     nsims = values$power_result$nsims,
                     p_adjust = values$power_result$p_adjust,
                     correction = values$power_result$correction,
                     manova = values$power_result$manova_result,
                     emm_p_adjust = input$emm_p_adjust,
                     emm = input$emm,
                     emm_model = input$emm_model)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Run the application
shinyApp(ui = ui, server = server)

