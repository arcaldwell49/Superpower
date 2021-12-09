#' Design function used to specify the parameters to be used in simulations
#' @param design String specifying the ANOVA design.
#' @param n Sample size in each condition
#' @param mu Vector specifying mean for each condition
#' @param sd standard deviation for all conditions (or a vector specifying the sd for each condition)
#' @param r Correlation between dependent variables (single value or matrix)
#' @param label_list An optional list to specify the factor names and condition (recommended, if not used factors and levels are indicated by letters and numbers).
#' @param labelnames Optional vector to specifying factor and condition names. This parameter is deprecated and will be overridden by input from label_list. 
#' @param plot Should means plot be printed (defaults to TRUE)
#' @return Returns single list with simulated data, design, design list, factor names, formulas for ANOVA, means, sd, correlation, sample size per condition, correlation matrix, covariance matrix, design string, labelnames, labelnameslist, factor names, meansplot
#' 
#' \describe{
#'   \item{\code{"dataframe"}}{A sample dataframe of what data could look like given the proposed parameters.}
#'   \item{\code{"design"}}{\code{aov} The design string, e.g. "2b*2w".}
#'   \item{\code{"design_list"}}{The list of variables in the design.}
#'   \item{\code{"frml1"}}{The first formula created for this design.}
#'   \item{\code{"frml2"}}{The second formula created for this design.}
#'   \item{\code{"mu"}}{Vector of means.}
#'   \item{\code{"sd"}}{Vector of standard deviations.}
#'   \item{\code{"r"}}{Common correlation coefficient.}
#'   \item{\code{"n"}}{Sample size per cell. Can be entered as a single value or list of sample sizes for each condition. If unequal n is entered then the design can only be passed onto ANOVA_power.}
#'   \item{\code{"cor_mat"}}{The correlation matrix.}
#'   \item{\code{"sigmatrix"}}{The variance-covariance matrix.}
#'   \item{\code{"design_factors"}}{Total number of within-subjects factors.}
#'   \item{\code{"labelnames"}}{List of the label names.}
#'   \item{\code{"labelnameslist"}}{Secondary list of labelnames}
#'   \item{\code{"factornames"}}{List of the factor titles.}
#'   \item{\code{"meansplot"}}{Plot of the experimental design.}
#' 
#' }

#' @examples
#' ## Set up a within design with 2 factors, each with 2 levels,
#' ## with correlation between observations of 0.8,
#' ## 40 participants (who do all conditions), and standard deviation of 2
#' ## with a mean pattern of 1, 0, 1, 0, conditions labeled 'condition' and
#' ## 'voice', with names for levels of "cheerful", "sad", and "human", "robot"
#' ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8,
#'       label_list= list(condition = c("cheerful", "sad"), 
#'       voice = c("human", "robot")))
#' @section Warnings:
#' Varying the sd or r (e.g., entering multiple values) violates assumptions of homoscedascity and sphericity respectively
#' @importFrom stats pnorm pt qnorm qt as.formula median
#' @importFrom utils combn
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @importFrom grDevices colorRampPalette
#' @import ggplot2
#' @export
#'

ANOVA_design <- function(design, n, mu, sd, r = 0,
                         label_list = NULL,
                         labelnames = NULL,
                         plot = Superpower_options("plot")){
  
  #Check String for an acceptable digits and factor (w or b)
  if (grepl("^(\\d{1,3}(w|b)\\*){0,2}\\d{1,3}(w|b)$", design, ignore.case = FALSE, perl = TRUE) == FALSE) {
    stop("Problem in the design argument: must input number of levels as integer (2-999) and factor-type (between or within) as lower case b (between) or w (within)")
  }
  
  #Ensure sd is greater than 0
  if (any(sd <= 0)) {
    stop("Standard deviation (sd) is less than or equal to zero; input a value greater than zero")
  }
  
  #Ensure, if single correlation is input, that it is between 0 and 1
  if (any(r < -1) | any(r > 1.0) ) {
    stop("Correlation must be greater than -1 and less than 1")
  }
  
  #Ensure proper n input
  #if (length(n) != 1 ) {
    #warning("Warning: Unequal n designs can only be passed to ANOVA_power")
    #unequal_design = TRUE
    n_vec <- n # store vector n as n - this is because the code below uses n as a single number, so quick fix for legacy reasons
    n <- max(n) # now set n to max n for ANOVA_design function
  #}
  
  #If labelnames are not provided, they are generated.
  #Store factor levels (used many times in the script, calculate once)
  factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
  
  if(!is.null(label_list)){

    labelnames = vector()
    for(i in 1:length(label_list)){
      labelnames = append(labelnames, names(label_list)[i])
      labelnames = append(labelnames, label_list[[i]])
    }
  }
  
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
  
  #Check if design and means match up - if not, throw an error and stop
  if(prod(factor_levels) != length(mu)){stop("the length of the vector with means does not match the study design")}
  
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
  design_list <- as.character(interaction(xxx[, 1:factors], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
  
  ###############
  # 3. Create Correlation and Covariance Matrix ----
  ###############
  
  #Create empty matrix
  sigmatrix <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))
  
  #NEW CODE, JUST FOR SINGLE correlation entry
  
  #single number
  cors <- r
  vars <- length(design_list)
  
  #Code by Lisa De Bruine. Allows multiple inputs for r - only use single value now.
  #from: https://github.com/debruine/faux/blob/master/R/rnorm_multi.R
  #Now stored in non-exported function
  
  cor_mat <- generate_cor_matrix(vars = vars, cors = cors)
  
  sd_for_sigma <- sd #added to prevent changing sd which is now passed on
  if (length(sd_for_sigma) == 1) {
    sd_for_sigma <- rep(sd_for_sigma, vars)
  } else if (length(sd_for_sigma) != vars) {
    stop("the length of sd_for_sigma must be 1 or vars");
  }
  
  sigma <- (sd_for_sigma %*% t(sd_for_sigma)) * cor_mat #Our earlier code had a bug, with SD on the diagonal. Not correct! Thanks Lisa.
  
  #General approach: For each factor in the list of the design, save the first item (e.g., a1b1)
  #Then for each factor in the design, if 1, set number to wildcard
  i1 <- 1
  i2 <- 1
  for (i1 in 1:length(design_list)) {
    design_list_split <- unlist(strsplit(design_list[i1], "_"))
    #current_factor <- design_list_split[c(2,4,6)[1:length(design)]] #this creates a string of 2, 2,4 or 2,4,6 depending on the length of the design for below
    for (i2 in 1:length(design_factors)) {
      #We set each number that is within to a wildcard, so that all within participant factors are matched
      if (design_factors[i2] == 1) {
        design_list_split[i2] <- "\\w+"
      }
    }
    sigmatrix[i1, ] <-
      as.numeric(grepl(paste0(design_list_split, collapse = "_"), design_list)) # compare factors that match with current factor, given wildcard, save list to sigmatrix
  }
  
  #Now multiply the matrix we just created (that says what is within, and what is between,  with the original covariance matrix)
  #So factors manipulated within are correlated, those manipulated between are not.
  cor_mat <- sigmatrix*cor_mat
  sigmatrix <- sigma*sigmatrix
  row.names(sigmatrix) <- design_list
  colnames(sigmatrix) <- design_list
  row.names(cor_mat) <- design_list
  colnames(cor_mat) <- design_list
  
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
  
  ###############
  # 5. Specify factors for formula ----
  ###############
  if(factors == 1 & sum(design_factors) == 1){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(subject/",factornames[1],")",sep=""))}
  if(factors == 1 & sum(design_factors) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(1 | subject)",sep=""))}
  
  if(factors == 2){
    if(sum(design_factors) == 2){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[1],"*",factornames[2],")"))}
    if(sum(design_factors) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"  + Error(1 | subject)"))}
    if(all(design_factors == c(1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[1],")"))}
    if(all(design_factors == c(0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[2],")"))}
  }
  
  if(factors == 3){
    if(sum(design_factors) == 3){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[2],"*",factornames[3],")"))}
    if(sum(design_factors) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(1 | subject)"))}
    if(all(design_factors == c(1, 0, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],")"))}
    if(all(design_factors == c(0, 1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[2],")"))}
    if(all(design_factors == c(0, 0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[3],")"))}
    if(all(design_factors == c(1, 1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[2],")"))}
    if(all(design_factors == c(0, 1, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[2],"*",factornames[3],")"))}
    if(all(design_factors == c(1, 0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[3],")"))}
  }
  
  #Specify second formula used for plotting
  if(factors == 1){frml2 <- as.formula(paste("~",factornames[1]))}
  if(factors == 2){frml2 <- as.formula(paste("~",factornames[1],"+",factornames[2]))}
  if(factors == 3){frml2 <- as.formula(paste("~",factornames[1],"+",factornames[2],"+",factornames[3]))}
  
  ###############
  # 6. Create plot of means to visualize the design ----
  ###############
  
  dataframe_means <- data.frame(mu, sd)
  for(j in 1:factors){
    dataframe_means <- cbind(dataframe_means, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                                                 sep="")),
                                                                   each = prod(factor_levels)/prod(factor_levels[1:j]),
                                                                   times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  
  if(factors == 1){
    names(dataframe_means) <- c("mu","SD",factornames[1])
    dataframe_means[,factornames[1]] <- ordered(dataframe_means[,factornames[1]], levels = labelnameslist[[1]])
  }
  if(factors == 2){
    names(dataframe_means)<-c("mu","SD",factornames[1],factornames[2])
    dataframe_means[,factornames[1]] <- ordered(dataframe_means[,factornames[1]], levels = labelnameslist[[1]])
    dataframe_means[,factornames[2]] <- ordered(dataframe_means[,factornames[2]], levels = labelnameslist[[2]])
  }
  
  if(factors == 3) {
    names(dataframe_means) <- c("mu","SD",factornames[1],factornames[2],factornames[3])
    dataframe_means[,factornames[1]] <- ordered(dataframe_means[,factornames[1]], levels = labelnameslist[[1]])
    dataframe_means[,factornames[2]] <- ordered(dataframe_means[,factornames[2]], levels = labelnameslist[[2]])
    dataframe_means[,factornames[3]] <- ordered(dataframe_means[,factornames[3]], levels = labelnameslist[[3]])
  }
  
  if(factors == 1){meansplot = ggplot(dataframe_means, aes_string(y = "mu", x = factornames[1]))}
  if(factors == 2){meansplot = ggplot(dataframe_means, aes_string(y = "mu", x = factornames[1], colour = factornames[2]))}
  if(factors == 3){meansplot = ggplot(dataframe_means, aes_string(y = "mu", x = factornames[1], colour = factornames[2])) + facet_wrap(  paste("~",factornames[3],sep=""))}
  
  #Set custom color palette if factor 2 has a length greater than 8
  if (factors >= 2 && length(labelnameslist[[2]]) >= 9) {
    
    meansplot2 = meansplot +
      geom_point(position = position_dodge(width = 0.9), shape = 10, size = 5, stat = "identity") + #Personal preference for sd -- ARC
      geom_errorbar(aes(ymin = mu - sd, ymax = mu + sd),
                    position = position_dodge(width = 0.9), size = .6, width = .3) +
      #coord_cartesian(ylim = c(min(mu) - max(sd), max(mu) + max(sd))) +
      theme_bw(base_size = 16) + ggtitle("Means for each condition in the design") 
    
  } else {
    
    meansplot2 = meansplot +
      geom_point(position = position_dodge(width = 0.9), shape = 10, size = 5, stat = "identity") + #Personal preference for sd -- ARC
      geom_errorbar(aes(ymin = mu - sd, ymax = mu + sd),
                    position = position_dodge(width = 0.9), size = .6, width = .3) +
      #coord_cartesian(ylim = c(min(mu) - sd, max(mu) + sd)) +
      theme_bw() + ggtitle("Means for each condition in the design") 
  }
  if (plot == TRUE) {
    print(meansplot2)  #should be blocked in Shiny context
  }
  
  # Return results in list()
  ## Now S3 method
  structure(list(dataframe = dataframe,
                 design = design,
                 design_list = design_list,
                 factors = factors,
                 frml1 = frml1,
                 frml2 = frml2,
                 mu = mu,
                 sd = sd,
                 r = r,
                 n = n_vec, #we save n_vec, as inputted, not max_n
                 cor_mat = cor_mat,
                 sigmatrix = sigmatrix,
                 design_factors = design_factors,
                 labelnames = labelnames,
                 labelnameslist = labelnameslist,
                 factornames = factornames,
                 meansplot = meansplot2),
            class = "design_aov")
}
