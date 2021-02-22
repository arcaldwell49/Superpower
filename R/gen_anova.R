gen_anova = function(i,
                     design_result,
                     p_adjust = "none",
                     correction = "none",
                     run_manova = FALSE,
                     emm = FALSE,
                     emm_model = "multivariate",
                     emm_p_adjust = "none",
                     emm_comp,
                     aov_result,
                     manova_result,
                     contrast_type = "pairwise",
                     alpha_level) { #for each simulated experiment
  
  dataframe <- design_result$dataframe # read in dataframe again, because we deleted rows from it below if unequal n
  n_vec <- design_result$n # store vector n as n - this is because the code below uses n as a single number, so quick fix for legacy reasons
  n <- max(n_vec) # now set n to max n for ANOVA_design function
  factornames <- design_result$factornames #Get factor names
  mu = design_result$mu # population means - should match up with the design
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  factors <- design_result$factors
  design_factors <- design_result$design_factors
  sigmatrix <- design_result$sigmatrix
  dataframe <- design_result$dataframe
  design_list <- design_result$design_list
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  design <- design_result$design
  #aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
  #                                        data = dataframe, 
  #                                        include_aov = FALSE,
  #                                        anova_table = list(es = "pes", 
  #                                                           p_adjust_method = p_adjust)) }) #This reports PES not GES
  possible_pc <- (((prod(
    as.numeric(strsplit(design, "\\D+")[[1]])
  )) ^ 2) - prod(as.numeric(strsplit(design, "\\D+")[[1]])))/2
  
  dataframe$y <- suppressMessages({
    melt(as.data.frame(mvrnorm(
      n = n,
      mu = mu,
      Sigma = as.matrix(sigmatrix)
    )))$value
  })
  
  paired_tests <- combn(unique(dataframe$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[lower.tri(sigmatrix)] #
  
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
  
  # Create Data Sets
  if (run_manova == TRUE) {
    #create empty dataframe to store simulation results
    #number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
    #more columns added if MANOVA output included 2^factors
    sim_data <- as.data.frame(matrix(
      ncol = 2 * (2 ^ factors - 1) + (2 ^ factors) + 2 * possible_pc,
      nrow = 1
    )) } else {
      
      sim_data <- as.data.frame(matrix(
        ncol = 2 * (2 ^ factors - 1) + 2 * possible_pc,
        nrow = 1
      ))
      
    }
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
  # We perform the ANOVA using AFEX
  #Can be set to NICE to speed up, but required data grabbing from output the change.
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design
                                          data = dataframe, 
                                          include_aov = FALSE, #Need development code to get aov_include function
                                          anova_table = list(es = "pes",
                                                             p_adjust_method = p_adjust,
                                                             correction = correction))}) #This reports PES not GES
  if (emm == TRUE) {
    if (missing(emm_comp)) {
      emm_comp = as.character(frml2)[2]
    }
    
    specs_formula <- as.formula(paste(contrast_type," ~ ",emm_comp))
    emm_result <- suppressMessages({emmeans(aov_result, 
                                            specs = specs_formula,
                                            model = emm_model,
                                            adjust = emm_p_adjust)})
    pairs_result_df <- emmeans_power(emm_result$contrasts, alpha_level = alpha_level)

    emm_sim_data <- as.data.frame(matrix(
      ncol = nrow(pairs_result_df)*2,
      nrow = 1))
    
    names(emm_sim_data) = c(paste("p_",
                                  rownames(pairs_result_df),
                                  sep = ""),
                            paste("cohen_f_",
                                  rownames(pairs_result_df),
                                  sep = ""))
    
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
    
    
    pairs_result_df <- pairs_result_df %>% 
      mutate(cohen_f = sqrt(.data$f2)) %>%
      select(-.data$F.value,-.data$t.ratio,-.data$SE,
             -.data$f2,-.data$pes, -.data$estimate, -.data$df) %>%
      select(-.data$cohen_f, -.data$p.value,
             .data$p.value, .data$cohen_f)
    
    emm_sim_data[1,] <- c(as.numeric(pairs_result_df$p.value), #p-value for contrast
                          as.numeric(pairs_result_df$cohen_f) # cohen f
    ) #
  } else{
    emm_sim_data = NULL
  }
  
  # Store MANOVA result if there are within subject factors
  if (run_manova == TRUE) {
    manova_result <- Anova_mlm_table(aov_result$Anova) # ::: in Shiny
    manova_result$p.value <- p.adjust(manova_result$p.value, method = p_adjust)
  }
  
  for (j in 1:possible_pc) {
    x <- dataframe$y[which(dataframe$cond == paired_tests[1,j])]
    y <- dataframe$y[which(dataframe$cond == paired_tests[2,j])]
    #this can be sped up by tweaking the functions that are loaded to only give p and dz
    ifelse(within_between[j] == 0,
           t_test_res <- effect_size_d(x, y, alpha_level = alpha_level), # ::: in Shiny
           t_test_res <- effect_size_d_paired(x, y, alpha_level = alpha_level)) # ::: in Shiny
    paired_p[j] <- t_test_res$p_value
    paired_d[j] <- ifelse(within_between[j] == 0,
                          t_test_res$d,
                          t_test_res$d_z)
  }
  
  # store p-values and effect sizes for calculations and plots.
  #If needed to create different row names if MANOVA is included
  if (run_manova == TRUE) {
    sim_data[1,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                      aov_result$anova_table[[5]], #partial eta squared
                      p.adjust(paired_p, method = p_adjust), #p-values for paired comparisons
                      paired_d, #effect sizes
                      manova_result[[6]]) #p-values for MANOVA
  } else {
    sim_data[1,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                      aov_result$anova_table[[5]], #partial eta squared
                      p.adjust(paired_p, method = p_adjust), #p-values for paired comparisons
                      paired_d) #effect sizes
  }
  
  return(list(
    sim_data = as.vector(sim_data),
    emm_sim_data = as.vector(emm_sim_data)
  ))
  
}



