
#@importFrom stats pnorm pt qnorm qt as.formula median qf power.t.test pf
#@importFrom base which %in%
#@importFrom utils combn
#@importFrom reshape2 melt
#@importFrom MASS mvrnorm
#@importFrom afex aov_car
#@import emmeans 
#@import ggplot2
#@export


emm_exact <- function(design_result, 
                      alpha_level = 0.05,
                      emm_model = "univariate",
                      verbose = TRUE,
                      specs) {

  
  #Errors with very small sample size; issue with mvrnorm function from MASS package
  if (design_result$n < prod(as.numeric(unlist(regmatches(design_result$design,
                                                          gregexpr("[[:digit:]]+", design_result$design)))))
  ) {
    stop("Cannot handle small sample sizes (n < the product of the factors) at this time; please pass this design_result to the ANOVA_power function to simulate power")
  }
  
  round_dig <- 4 #Set digits to which you want to round the output.
  
  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }
  
  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
  }
  
  #Read in all variables from the design_result object
  design <- design_result$design #String used to specify the design
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
  
  
  
  ###############
  #Specify factors for formula ----
  ###############
  
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
                                          data = dataframe, 
                                          include_aov = if(emm_model == "univariate"){
                                            TRUE
                                          } else {
                                            FALSE
                                          },
                                          anova_table = list(es = "pes")) }) #This reports PES not GES
  
  #We simulate a new y variable, melt it in long format, and add it to the dataframe (surpressing messages)
  #empirical set to true to create "exact" dataset
  
  dataframe$y <- suppressMessages({
    melt(as.data.frame(mvrnorm(
      n = n,
      mu = mu,
      Sigma = as.matrix(sigmatrix),
      empirical = TRUE
    )))$value
  })
  
  # We perform the ANOVA using AFEX
  #Can be set to NICE to speed up, but required data grabbing from output the change.
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design
                                          data = dataframe, 
                                          include_aov = if(emm_model == "univariate"){
                                            TRUE
                                          } else {
                                            FALSE
                                          }, #Need development code to get aov_include function
                                          anova_table = list(es = "pes",
                                                             correction = "none"))}) #This reports PES not GES
  #Call emmeans with specifcations given in the function
  #Limited to specs and model
  emm_result <- emmeans(aov_result, 
                        specs = specs,
                        model = emm_model)
  #make comparison based on specs; adjust = "none" in exact; No solution for multcomp in exact simulation
  pairs_result <- pairs(emm_result, adjust = "none")
  pairs_result_df <- as.data.frame(pairs_result)
  #Need for exact; not necessary for power function
  #Convert t-ratio to F-stat
  pairs_result_df$F.value <- (pairs_result_df$t.ratio)^2
  #Calculate pes -- The formula for partial eta-squared is equation 13 from Lakens (2013)
  pairs_result_df$pes <- pairs_result_df$F.value/(pairs_result_df$F.value+pairs_result_df$df) 
  #Calculate cohen's f
  pairs_result_df$f2 <- pairs_result_df$pes/(1 - pairs_result_df$pes)
  #Calculate noncentrality
  pairs_result_df$lambda <- pairs_result_df$f2*pairs_result_df$df
  #minusalpha<- 1-alpha_level
  pairs_result_df$Ft <- qf((1 - alpha_level), 1, pairs_result_df$df)
  #Calculate power
  pairs_result_df$power <- (1 - pf(pairs_result_df$Ft, 1, pairs_result_df$df, pairs_result_df$lambda))*100
  
  
  pairs_result_df <- pairs_result_df[ , -which(names(pairs_result_df) %in% c("p.value"))]
  

  # Return results in list()
  invisible(list(dataframe = dataframe,
                 aov_result = aov_result,
                 emm_result = pairs_result_df,
                 alpha_level = alpha_level))
  
}
