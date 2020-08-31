monte_gen = function(design_result,n){
  
  design <- design_result$design #String used to specify the design
  factornames <- design_result$factornames #Get factor names
  mu = design_result$mu # population means - should match up with the design
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  factors <- design_result$factors
  design_factors <- design_result$design_factors
  sigmatrix <- design_result$sigmatrix
  design_list <- design_result$design_list
  labelnameslist <- design_result$labelnameslist
  factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
  ###############
  # 4. Create Dataframe based on Design ----
  ###############
  
  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  dataframe <- as.data.frame(mvrnorm(n = n,
                                     mu = mu,
                                     Sigma = sigmatrix,
                                     empirical = FALSE))
  dataframe$subject<-as.factor(c(1:n)) #create temp subject variable just for merging
  #Melt dataframe
  dataframe <- melt(dataframe,
                    id.vars = "subject",
                    variable.name = "cond",
                    value.name = "y")
  
  # Let's break this down - it's a bit tricky. First, we want to create a list of labelnames that will indicate the factors.
  # We are looping this over the number of factors.
  # This: factor_levels - takes the string used to specify the design and turn it in a list.
  # we take the labelnames and factornames and combine them
  # We repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
  # We then bind these together with the existing dataframe.
  for(j in 1:factors){
    dataframe <- cbind(dataframe, as.factor(unlist(rep(as.list(paste(factornames[[j]],
                                                                     labelnameslist[[j]],
                                                                     sep="_")),
                                                       each = n*prod(factor_levels)/prod(factor_levels[1:j]),
                                                       times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  #Rename the factor variables that were just created
  names(dataframe)[4:(3+factors)] <- factornames[1:factors]
  
  #Create subject column
  subject <- 1:n #Set subject to 1 to the number of subjects collected
  
  for(j2 in length(design_factors):1){ #for each factor in the design, from last to first
    #if w: repeat current string as often as the levels in the current factor (e.g., 3)
    #id b: repeat current string + max of current subject
    if(design_factors[j2] == 1){subject <- rep(subject,factor_levels[j2])}
    subject_length <- length(subject) #store current length - to append to string of this length below
    if(design_factors[j2] == 0){
      for(j3 in 2:factor_levels[j2]){
        subject <- append(subject,subject[1:subject_length]+max(subject))
      }
    }
  }
  
  #Overwrite subject columns in dataframe
  dataframe$subject <- subject
  #For the correlation matrix, we want the names of each possible comparison of means
  #Need to identify which columns from dataframe to pull the factor names from
  if (factors == 1) {
    cond_col <- c(4)
  } else if (factors == 2) {
    cond_col <- c(4, 5)
  } else {
    cond_col <- c(4, 5, 6)
  }
  
  dataframe$cond <- as.character(interaction(dataframe[, cond_col], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
  return(dataframe)
}

exact_gen = function(design_result){
  
  design <- design_result$design #String used to specify the design
  factornames <- design_result$factornames #Get factor names
  mu = design_result$mu # population means - should match up with the design
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  factors <- design_result$factors
  design_factors <- design_result$design_factors
  sigmatrix <- design_result$sigmatrix
  design_list <- design_result$design_list
  labelnameslist <- design_result$labelnameslist
  factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
  if (design_result$n < prod(as.numeric(unlist(regmatches(design_result$design,
                                                          gregexpr("[[:digit:]]+", 
                                                                   design_result$design)))))+60) {
    n = prod(as.numeric(unlist(regmatches(design_result$design,
                                          gregexpr("[[:digit:]]+",
                                                   design_result$design)))))+60
  } else {
    n = design_result$n
  }
  ###############
  # 4. Create Dataframe based on Design ----
  ###############
  
  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  dataframe <- as.data.frame(mvrnorm(n = n,
                                     mu = mu,
                                     Sigma = sigmatrix,
                                     empirical = TRUE))
  dataframe$subject<-as.factor(c(1:n)) #create temp subject variable just for merging
  #Melt dataframe
  dataframe <- melt(dataframe,
                    id.vars = "subject",
                    variable.name = "cond",
                    value.name = "y")
  
  # Let's break this down - it's a bit tricky. First, we want to create a list of labelnames that will indicate the factors.
  # We are looping this over the number of factors.
  # This: factor_levels - takes the string used to specify the design and turn it in a list.
  # we take the labelnames and factornames and combine them
  # We repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
  # We then bind these together with the existing dataframe.
  for(j in 1:factors){
    dataframe <- cbind(dataframe, as.factor(unlist(rep(as.list(paste(factornames[[j]],
                                                                     labelnameslist[[j]],
                                                                     sep="_")),
                                                       each = n*prod(factor_levels)/prod(factor_levels[1:j]),
                                                       times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  #Rename the factor variables that were just created
  names(dataframe)[4:(3+factors)] <- factornames[1:factors]
  
  #Create subject column
  subject <- 1:n #Set subject to 1 to the number of subjects collected
  
  for(j2 in length(design_factors):1){ #for each factor in the design, from last to first
    #if w: repeat current string as often as the levels in the current factor (e.g., 3)
    #id b: repeat current string + max of current subject
    if(design_factors[j2] == 1){subject <- rep(subject,factor_levels[j2])}
    subject_length <- length(subject) #store current length - to append to string of this length below
    if(design_factors[j2] == 0){
      for(j3 in 2:factor_levels[j2]){
        subject <- append(subject,subject[1:subject_length]+max(subject))
      }
    }
  }
  
  #Overwrite subject columns in dataframe
  dataframe$subject <- subject
  #For the correlation matrix, we want the names of each possible comparison of means
  #Need to identify which columns from dataframe to pull the factor names from
  if (factors == 1) {
    cond_col <- c(4)
  } else if (factors == 2) {
    cond_col <- c(4, 5)
  } else {
    cond_col <- c(4, 5, 6)
  }
  
  dataframe$cond <- as.character(interaction(dataframe[, cond_col], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
  return(dataframe)
}


