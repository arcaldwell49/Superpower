#' Design function to specify the details for the simulation
#' @param string String specifying the ANOVA design.
#' @param n Sample size in each condition
#' @param mu Vector specifying mean for each condition
#' @param sd standard deviation for all conditions
#' @param r Correlation between dependent variables (single value or matrix)
#' @param labelnames Vector specifying factor and condition names
#' @param plot Should means plot be printed (defaults to TRUE)
#' @return Returns Single data-frame with simulated data, design, design list, factor names, formulas for ANOVA, means, sd, correlation, sample size per condition, correlation matrix, covariance matrix, design string, label names, factor names, meansplot
#' @examples
#' ## Set up a within design with 2 factors, each with 2 levels,
#' ## with correlation between observations of 0.8,
#' ## 40 participants (who do all conditions), and standard deviation of 2
#' ## with a mean pattern of 1, 0, 1, 0, conditions labeled 'condition' and
#' ## 'voice', with names for levels of "cheerful", "sad", and "human", "robot"
#' ANOVA_design(string = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8,
#'       labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
#' @section References:
#' too be added
#' @importFrom stats pnorm pt qnorm qt as.formula median
#' @importFrom utils combn
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @importFrom grDevices colorRampPalette
#' @import ggplot2
#' @export
#'

ANOVA_design <- function(string, n, mu, sd, r = 0, labelnames, plot = TRUE){

  if (length(labelnames) != length(as.numeric(strsplit(string, "\\D+")[[1]])) + sum(as.numeric(strsplit(string, "\\D+")[[1]]))) {
    stop("Design (string) does not match the length of the labelnames")
  }

  ###############
  # 1. Specify Design and Simulation----
  ###############
  # String used to specify the design
  # Add numers for each factor with 2 levels, e.g., 2 for a factor with 2 levels
  # Add a w after the number for within factors, and a b for between factors
  # Seperate factors with a * (asteriks)
  # Thus "2b*3w) is a design with 2 between levels, and 3 within levels

  #Check if design an means match up - if not, throw an error and stop
  if(prod(as.numeric(strsplit(string, "\\D+")[[1]])) != length(mu)){stop("the length of the vector with means does not match the study design")}

  ###############
  # 2. Create Dataframe based on Design ----
  ###############

  #Count number of factors in design
  factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

  #Get factor names and labelnameslist
  labelnames1 <- labelnames[(1 + 1):(1+as.numeric(strsplit(string, "\\D+")[[1]])[1])]
  if(factors > 1){labelnames2 <- labelnames[(as.numeric(strsplit(string, "\\D+")[[1]])[1] + 3):((as.numeric(strsplit(string, "\\D+")[[1]])[1] + 3) + as.numeric(strsplit(string, "\\D+")[[1]])[2] - 1)]}
  if(factors > 2){labelnames3 <- labelnames[(as.numeric(strsplit(string, "\\D+")[[1]])[2] + as.numeric(strsplit(string, "\\D+")[[1]])[1] + 4):((as.numeric(strsplit(string, "\\D+")[[1]])[2] + as.numeric(strsplit(string, "\\D+")[[1]])[1] + 4) + as.numeric(strsplit(string, "\\D+")[[1]])[3] - 1)]}

  factornames1 <- labelnames[1]
  if(factors > 1){factornames2 <- labelnames[as.numeric(strsplit(string, "\\D+")[[1]])[1] + 2]}
  if(factors > 2){factornames3 <- labelnames[as.numeric(strsplit(string, "\\D+")[[1]])[2] + as.numeric(strsplit(string, "\\D+")[[1]])[1] + 3]}

  if(factors == 1){labelnameslist <- list(labelnames1)}
  if(factors == 2){labelnameslist <- list(labelnames1,labelnames2)}
  if(factors == 3){labelnameslist <- list(labelnames1,labelnames2,labelnames3)}

  if(factors == 1){factornames <- c(factornames1)}
  if(factors == 2){factornames <- c(factornames1,factornames2)}
  if(factors == 3){factornames <- c(factornames1,factornames2,factornames3)}

  #Specify within/between factors in design: Factors that are within are 1, between 0
  design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed = TRUE)[[1]]
  design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0

  mu2 <- mu
  sd2 <- sd
  sigmatrix_2 <- matrix(0, length(mu),length(mu)) #create temp matrix filled with value of correlation, nrow and ncol set to length in mu

  #The loop below is to avoid issues with creating the matrix associated with having a sd < r
  while (sd2 < min(r)) {
    sd2 <- sd2*10
    mu2 <- mu2*10
  }

  diag(sigmatrix_2) <- sd2 # replace the diagonal with the sd


  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  dataframe <- as.data.frame(mvrnorm(n=n,
                                     mu=mu2,
                                     Sigma=sigmatrix_2,
                                     empirical = FALSE))
  dataframe$subject<-as.factor(c(1:n)) #create temp subject variable just for merging
  #Melt dataframe
  dataframe <- melt(dataframe,
                    id.vars = "subject",
                    variable.name = "cond",
                    value.name = "y")

  # Let's break this down - it's a bit tricky. First, we want to create a list of labelnames that will indicate the factors.
  # We are looping this over the number of factors.
  # This: as.numeric(strsplit(string, "\\D+")[[1]]) - takes the string used to specify the design and turn it in a list.
  # we take the labelnames and factornames and combine them
  # We repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
  # We then bind these together with the existing dataframe.
  for(j in 1:factors){
    dataframe <- cbind(dataframe, as.factor(unlist(rep(as.list(paste(factornames[[j]],
                                                                     labelnameslist[[j]],
                                                                     sep="_")),
                                                       each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
    ))))
  }
  #Rename the factor variables that were just created
  names(dataframe)[4:(3+factors)] <- factornames[1:factors]

  #Create subject column
  subject <- 1:n #Set subject to 1 to the number of subjects collected

  for(j2 in length(design):1){ #for each factor in the design, from last to first
    #if w: repeat current string as often as the levels in the current factor (e.g., 3)
    #id b: repeat current string + max of current subject
    if(design[j2] == 1){subject <- rep(subject,as.numeric(strsplit(string, "\\D+")[[1]])[j2])}
    subject_length <- length(subject) #store current length - to append to string of this length below
    if(design[j2] == 0){
      for(j3 in 2:as.numeric(strsplit(string, "\\D+")[[1]])[j2]){
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
  # 3. Specify factors for formula ----
  ###############
  if(factors == 1 & sum(design) == 1){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(subject/",factornames[1],")",sep=""))}
  if(factors == 1 & sum(design) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(1 | subject)",sep=""))}

  if(factors == 2){
    if(sum(design) == 2){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[1],"*",factornames[2],")"))}
    if(sum(design) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"  + Error(1 | subject)"))}
    if(all(design == c(1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[1],")"))}
    if(all(design == c(0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[2],")"))}
  }

  if(factors == 3){
    if(sum(design) == 3){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[2],"*",factornames[3],")"))}
    if(sum(design) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(1 | subject)"))}
    if(all(design == c(1, 0, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],")"))}
    if(all(design == c(0, 1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[2],")"))}
    if(all(design == c(0, 0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[3],")"))}
    if(all(design == c(1, 1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[2],")"))}
    if(all(design == c(0, 1, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[2],"*",factornames[3],")"))}
    if(all(design == c(1, 0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[3],")"))}
  }

  #Specify second formula used for plotting
  if(factors == 1){frml2 <- as.formula(paste("~",factornames[1]))}
  if(factors == 2){frml2 <- as.formula(paste("~",factornames[1],"+",factornames[2]))}
  if(factors == 3){frml2 <- as.formula(paste("~",factornames[1],"+",factornames[2],"+",factornames[3]))}

  ############################################
  #Specify factors for formula ###############
  design_list <- unique(apply((dataframe)[4:(3+factors)], 1, paste, collapse="_"))

  ###############
  # 4. Create Covariance Matrix ----
  ###############

  #Create empty matrix
  sigmatrix <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))

  #NEW CODE, JUST FOR SINGLE correlation entry

  #single number
  cors <- r
  vars <- length(design_list)

  #Code by Lisa De Bruine. Allows multiple inputs for r - only use single value now.
  #from: https://github.com/debruine/faux/blob/master/R/rnorm_multi.R
  # correlation matrix
  generate_cor_matrix  <- function(vars = 3, cors = 0, mu = 0, sd = 1) {
    if (length(mu) == 1) {
      mu <- rep(mu, vars)
    } else if (length(mu) != vars) {
      stop("the length of mu must be 1 or vars");
    }

    if (length(sd) == 1) {
      sd <- rep(sd, vars)
    } else if (length(sd) != vars) {
      stop("the length of sd must be 1 or vars");
    }

    # correlation matrix
    if (class(cors) == "numeric" & length(cors) == 1) {
      if (cors >=-1 & cors <=1) {
        cors = rep(cors, vars*(vars-1)/2)
      } else {
        stop("cors must be between -1 and 1")
      }
    }

    if (class(cors) == "matrix") {
      if (!is.numeric(cors)) {
        stop("cors matrix not numeric")
      } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
        stop("cors matrix wrong dimensions")
      } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
        stop("cors matrix not symmetric")
      } else {
        cor_mat <- cors
      }
    } else if (length(cors) == vars*vars) {
      cor_mat <- matrix(cors, vars)
    } else if (length(cors) == vars*(vars-1)/2) {
      # generate full matrix from vector of upper right triangle

      cor_mat <- matrix(nrow=vars, ncol = vars)
      upcounter = 1
      lowcounter = 1
      for (col in 1:vars) {
        for (row in 1:vars) {
          if (row == col) {
            # diagonal
            cor_mat[row, col] = 1
          } else if (row > col) {
            # lower left triangle
            cor_mat[row, col] = cors[lowcounter]
            lowcounter <- lowcounter + 1
          }
        }
      }
      for (row in 1:vars) {
        for (col in 1:vars) {
          if (row < col) {
            # upper right triangle
            cor_mat[row, col] = cors[upcounter]
            upcounter <- upcounter + 1
          }
        }
      }
    }

    # check matrix is positive definite
    tol <- 1e-08
    ev <- eigen(cor_mat, only.values = TRUE)$values
    if (sum(ev < tol)) {
      stop("correlation matrix not positive definite")
    }

    return(cor_mat)
  }
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
  for(i1 in 1:length(design_list)){
    design_list_split <- unlist(strsplit(design_list[i1],"_"))
    current_factor <- design_list_split[c(2,4,6)[1:length(design)]] #this creates a string of 2, 2,4 or 2,4,6 depending on the length of the design for below
    for(i2 in 1:length(design)){
      #We set each number that is within to a wildcard, so that all within subject factors are matched

      if(design[i2]==1){current_factor[i2] <- "\\w+"}

    }
    ifelse(factors == 1,
           current_factor <- paste0(c(design_list_split[1]),
                                    "_",
                                    current_factor,
                                    collapse="_"),
           ifelse(factors == 2,
                  current_factor <- paste0(c(design_list_split[c(1,3)]),
                                           "_",
                                           current_factor,
                                           collapse="_"),
                  current_factor <- paste0(c(design_list_split[c(1,3,5)]),
                                           "_",
                                           current_factor,
                                           collapse="_")))



    sigmatrix[i1,]<-as.numeric(grepl(current_factor, design_list)) # compare factors that match with current factor, given wildcard, save list to sigmatrix
  }

  #Now multiply the matrix we just created (that says what is within, and what is between,  with the original covariance matrix)
  #So factors manipulated within are correlated, those manipulated between are not.
  cor_mat <- sigmatrix*cor_mat
  sigmatrix <- sigma*sigmatrix
  row.names(sigmatrix) <- design_list
  colnames(sigmatrix) <- design_list

  ###############
  # 6. Create plot of means to vizualize the design ----
  ###############

  #Changed to SD so that way the authors can visually check to make sure the SD matches that of the intended input -- ARC

  dataframe_means <- data.frame(mu, sd)
  for(j in 1:factors){
    dataframe_means <- cbind(dataframe_means, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                                                 sep="")),
                                                                   each = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                                                   times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
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
      geom_point(position = position_dodge(width=0.9), shape = 10, size=5, stat="identity") + #Personal preference for sd -- ARC
      geom_errorbar(aes(ymin = mu-sd, ymax = mu+sd),
                    position = position_dodge(width=0.9), size=.6, width=.3) +
      coord_cartesian(ylim=c(min(mu)-sd, max(mu)+sd)) +
      theme_bw(base_size = 16) + ggtitle("Means for each condition in the design") +
      scale_colour_brewer(palette = "Dark2")

  } else {

    meansplot2 = meansplot +
      geom_point(position = position_dodge(width=0.9), shape = 10, size=5, stat="identity") + #Personal preference for sd -- ARC
      geom_errorbar(aes(ymin = mu-sd, ymax = mu+sd),
                    position = position_dodge(width=0.9), size=.6, width=.3) +
      coord_cartesian(ylim=c(min(mu)-sd, max(mu)+sd)) +
      theme_bw() + ggtitle("Means for each condition in the design") +
      scale_colour_brewer(palette = "Dark2")
  }
  if(plot == TRUE){
    print(meansplot2)  #should be blocked in Shiny context
  }

  # Return results in list()
  invisible(list(dataframe = dataframe,
                 design = design,
                 design_list = design_list,
                 factors = factors,
                 frml1 = frml1,
                 frml2 = frml2,
                 mu = mu,
                 sd = sd,
                 r = r,
                 n = n,
                 cor_mat = cor_mat,
                 sigmatrix = sigmatrix,
                 string = string,
                 labelnames = labelnameslist,
                 factornames = factornames,
                 meansplot = meansplot2))
}

