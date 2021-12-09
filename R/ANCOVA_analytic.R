#' Power Calculations for Factorial ANCOVAs
#' 
#' Complete power analyses for ANCOVA omnibus tests and contrasts. This function does not support within subjects factors.
#' 
#' @param design Output from the ANOVA_design function
#' @param mu Vector specifying mean for each condition
#' @param n Sample size in each condition
#' @param sd Standard deviation for all conditions (or a vector specifying the sd for each condition)
#' @param r2 Coefficient of Determination of the model with only the covariates
#' @param n_cov Number of covariates
#' @param alpha_level Alpha level used to determine statistical significance
#' @param beta_level Type II error probability (power/100-1)
#' @param cmats List of matrices for specific contrasts of interest
#' @param label_list An optional list to specify the factor names and condition (recommended, if not used factors and levels are indicated by letters and numbers).
#' @param design_result Output from the ANOVA_design function
#' @param round_up Logical indicator (default = TRUE) for whether to round up sample size calculations to nearest whole number
#' 
#' @return
#' One, or two, data frames containing the power analysis results from the power analysis for the omnibus ANCOVA (main_results) or contrast tests (contrast_results).
#' In addition, every F-test (aov_list and con_list) is included in a list of power.htest results. 
#' Lastly, a (design_param) list containing the design parameters is also included in the results.
#' @examples
#' # Simple 2x3 ANCOVA
#' 
#' ANCOVA_analytic(
#' design = "2b*3b",
#' mu = c(400, 450, 500,
#'       400, 500, 600),
#' n_cov = 3,
#' sd = 100,
#' r2 = .25,
#' alpha_level = .05,
#' beta_level = .2,
#' round_up = TRUE
#' )
#' @section References:
#' Shieh, G. (2020). Power analysis and sample size planning in ANCOVA designs. Psychometrika, 85(1), 101-120.
#' @importFrom stats uniroot pf df qf contr.sum dt dbeta qtukey model.matrix terms optim contrasts
#' @export
#'


ANCOVA_analytic <- function(design,
                            mu,
                            n = NULL,
                            sd,
                            r2 = NULL,
                            n_cov,
                            alpha_level = Superpower_options("alpha_level"),
                            beta_level = NULL,
                            cmats = list(),
                            label_list = NULL,
                            design_result = NULL,
                            round_up = TRUE) {
  
  METHOD <- "Power Calculation for ANCOVA"
  METHOD2 <- "Power Calculation for ANCOVA contrast"
  TYPE = "Exact"
  if(!is.null(design_result)){
    mu = design_result$mu
    sd = design_result$sd
    n = design_result$n

    
    factornames = design_result$factornames
    frml3 = as.formula(paste(gsub("\\+", "*",design_result$frml2),collapse=" "))
    labelnameslist = design_result$labelnameslist
    design = design_result$design
  } else{
    
    #Check String for an acceptable digits and factor (w or b)
    if (grepl("^(\\d{1,3}(w|b)\\*){0,2}\\d{1,3}(w|b)$", design, ignore.case = FALSE, perl = TRUE) == FALSE) {
      stop("Problem in the design argument: must input number of levels as integer (2-999) and factor-type (between or within) as lower case b (between) or w (within)")
    }
    
    #Ensure sd is greater than 0
    if (any(sd <= 0) || length(sd) != 1) {
      stop("Standard deviation (sd) is less than or equal to zero; input a single value greater than zero")
    }
    
    #Ensure, if single correlation is input, that it is between 0 and 1
    if(!is.null(r2)){
      if ((r2 <= 0) | r2 >= 1 ) {
        stop("Coefficient of Determination must be greater than -1 and less than 1")
      }
    }

    
    factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
    if(prod(factor_levels) != length(mu)){
      stop("Length of means does not match design.")
    }
    labelnames = NULL
    if (is.null(label_list)) {
      label_list = list()
      for (i1 in 1:length(factor_levels)){
        label_list1 = NULL
        labelnames <- append(labelnames,paste(paste(letters[i1]), sep = ""))
        
        for (i2 in 1:factor_levels[i1]){
          labelnames <- append(labelnames,paste(paste(letters[i1]), paste(i2), sep = ""))
          label_list1 = c(label_list1,paste(paste(letters[i1]), paste(i2), sep = ""))
        }
       
        label_list[[i1]] = as.vector(label_list1)
        names(label_list)[i1] = paste(paste(letters[i1]), sep = "")
        
      }
      
    } else{
      for(i in 1:length(label_list)){
        
        labelnames = append(labelnames, names(label_list)[i])
        labelnames = append(labelnames, label_list[[i]])
      }
    
    }
    
    labelnameslist = label_list
    factornames = names(label_list)

  }
  
  if( missing(mu) ||  missing(sd) || missing(n_cov)){
    stop("mu, sd, and n_cov are missing and must be provided")
  }
  
  n_grp = length(mu)
  
  if(!is.null(n)){
    if(length(n)==1){
      nvec = rep(n,length(mu))
    } else {
      nvec = n
      }
    
    if(length(nvec) != length(mu)){
      stop("Length of n and mu do not match")
    }
  } else {
    nvec = NULL
  }
  factorlist = list()
  cons = cmats
  if(length(factornames) == 1){
    con_df = data.frame(a = 1:length(labelnameslist[[1]]))
    con_df$a = as.factor(con_df$a)
    contrasts(con_df$a) = "contr.sum"
    con_mat = model.matrix(~a,con_df)
    colnames(con_mat) = attr(con_mat, "assign")
    cmat_a = t(as.matrix(con_mat[,which(colnames(con_mat)==1)]))
    cmats = list(a = cmat_a)
    factorlist = factornames
  } else if(length(factornames) == 2){
    con_df = as.data.frame(expand.grid(b = 1:length(labelnameslist[[2]]),
                                       a = 1:length(labelnameslist[[1]])))
    con_df$a = as.factor(con_df$a)
    con_df$b = as.factor(con_df$b)
    contrasts(con_df$a) = "contr.sum"
    contrasts(con_df$b) = "contr.sum"
    #colnames(con_df) = design_result$factornames
    con_mat = model.matrix(~a*b,con_df)
    colnames(con_mat) = attr(con_mat, "assign")
    cmat_a = t(as.matrix(con_mat[,which(colnames(con_mat)==1)]))
    cmat_b = t(as.matrix(con_mat[,which(colnames(con_mat)==2)]))
    cmat_ab = t(as.matrix(con_mat[,which(colnames(con_mat)==3)]))
    cmats = list(a = cmat_a, b = cmat_b, ab=cmat_ab)
    factorlist = c(factornames[1],
                    factornames[2],
                    paste0(factornames[1], ":",
                           factornames[2]))
  } else {
    con_df = as.data.frame(expand.grid(c = 1:length(labelnameslist[[3]]),
                                       b = 1:length(labelnameslist[[2]]),
                                       a = 1:length(labelnameslist[[1]])))
    con_df$a = as.factor(con_df$a)
    con_df$b = as.factor(con_df$b)
    con_df$c = as.factor(con_df$c)
    contrasts(con_df$a) = "contr.sum"
    contrasts(con_df$b) = "contr.sum"
    contrasts(con_df$c) = "contr.sum"
    #colnames(con_df) = design_result$factornames
    con_mat = model.matrix(~a*b*c,con_df)
    colnames(con_mat) = attr(con_mat, "assign")
    cmat_a = t(as.matrix(con_mat[,which(colnames(con_mat)==1)]))
    cmat_b = t(as.matrix(con_mat[,which(colnames(con_mat)==2)]))
    cmat_c = t(as.matrix(con_mat[,which(colnames(con_mat)==3)]))
    cmat_ab = t(as.matrix(con_mat[,which(colnames(con_mat)==4)]))
    cmat_ac = t(as.matrix(con_mat[,which(colnames(con_mat)==5)]))
    cmat_bc = t(as.matrix(con_mat[,which(colnames(con_mat)==6)]))
    cmat_abc = t(as.matrix(con_mat[,which(colnames(con_mat)==7)]))
    cmats = list(a = cmat_a, b = cmat_b, c = cmat_c,
                 ab = cmat_ab, ac = cmat_ac, bc = cmat_bc,
                 abc = cmat_abc)
    factorlist = c(factornames[1],
                    factornames[2],
                    factornames[3],
                    paste0(factornames[1], ":",
                           factornames[2]),
                    paste0(factornames[1], ":",
                           factornames[3]),
                    paste0(factornames[2], ":",
                           factornames[3]),
                    paste0(factornames[1], ":",
                           factornames[2], ":",
                           factornames[3]))
  }
  #cmat = t(contr.sum(length(nvec))) # Contrast matrix
  # Create matrix for Wald statistic (W star; eq 10 from Shieh)
  pow_res = list()
  pow_res2 = list()
  con_res = list()
  con_res2 = list()
  if(grepl("w", design)){
    stop("Design contains a within subject factor. \n This is not supported by this function at this time")
  }
  
  if (!is.null(beta_level)){
    pow = 1 - beta_level
  } 
  
  p.body = quote({
    pow_anc_meth(
    cmat,
    mu, 
    nvec, 
    n_cov, 
    r2, 
    sd, 
     alpha_level
  )})
  
  if (is.null(beta_level)){
    for(i1 in 1:length(cmats)){
      cmat = cmats[[i1]]
      res <- eval(p.body)
      beta_level <- 1-res$pow
      pow_res[[i1]] = list(
        cmat = cmat,
        mu = mu, 
        nvec = nvec, 
        n_cov = n_cov, 
        r2 = res$r2, 
        sd = sd, 
        alpha_level = res$alpha_level,
        pow = res$pow,
        beta_level = beta_level,
        num_df = res$num_df,
        den_df = res$den_df,
        N_tot = res$N_tot
      )
    }
    names(pow_res) = names(cmats)
    
    if(length(cons) != 0){
      for(i1 in 1:length(cons)){
        cmat = cons[[i1]]
        res <- eval(p.body)
        beta_level <- 1-res$pow
        con_res[[i1]] = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )
      }
      names(con_res) = names(cons)
    }

  } else if(is.null(r2)) {
    
    for(i1 in 1:length(cmats)){
      cmat = cmats[[i1]]
      
      r2 = optim(par=.5,
                 fn=function(r2){ abs(eval(p.body)$pow - pow)}, 
                 #c(.001, .999), 
                 upper = .999,
                 lower = .001,
                 method= "Brent")$par
      # Uniroot produces error
      #r2 <- uniroot(function(r2) eval(p.body)$pow - 
      #                                    pow, c(1e-10, 1 - 1e-10))$root
      res <- eval(p.body) 
      beta_level <- 1-res$pow
      pow_res[[i1]] = list(
        cmat = cmat,
        mu = mu, 
        nvec = res$nvec, 
        n_cov = n_cov, 
        r2 = res$r2, 
        sd = sd, 
        alpha_level = res$alpha_level,
        pow = res$pow,
        beta_level = res$beta_level,
        num_df = res$num_df,
        den_df = res$den_df,
        N_tot = res$N_tot
      )
    }
    names(pow_res) = names(cmats)
    
    if(length(cons) != 0){
      for(i1 in 1:length(cons)){
        cmat = cons[[i1]]
        r2 <- uniroot(function(r2) eval(p.body)$pow - 
                        pow, c(1e-10, 1 - 1e-10))$root
        res <- eval(p.body)
        beta_level <- 1-res$pow
        con_res[[i1]] = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )
      }
      names(con_res) = names(cons)
    }
    
  } else if (is.null(alpha_level)) {
    
    for(i1 in 1:length(cmats)){
      cmat = cmats[[i1]]
      alpha_level <- uniroot(function(alpha_level) eval(p.body)$pow - 
                               pow, c(1e-10, 1 - 1e-10))$root
      res <- eval(p.body)
      beta_level <- 1-res$pow
      pow_res[[i1]] = list(
        cmat = cmat,
        mu = mu, 
        nvec = nvec, 
        n_cov = n_cov, 
        r2 = res$r2, 
        sd = sd, 
        alpha_level = res$alpha_level,
        pow = res$pow,
        beta_level = beta_level,
        num_df = res$num_df,
        den_df = res$den_df,
        N_tot = res$N_tot
      )
    }
    names(pow_res) = names(cmats)
    
    if(length(cons) != 0){
      for(i1 in 1:length(cons)){
        cmat = cons[[i1]]
        alpha_level <- uniroot(function(alpha_level) eval(p.body)$pow - 
                                 pow, c(1e-10, 1 - 1e-10))$root
        res <- eval(p.body)
        beta_level <- 1-res$pow
        con_res[[i1]] = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )
      }
      names(con_res) = names(cons)
    }
    
  } else if (is.null(n)){
    
    p.body2 <- quote({
      pow_anc_meth(cmat,
                   mu,
                   nvec = rep(N_tot / length(mu), length(mu)),
                   n_cov,
                   r2,
                   sd,
                   alpha_level)
    })
    
    for(i1 in 1:length(cmats)){
      cmat = cmats[[i1]]
      
      N_tot <- optim(par=(2*length(mu)+n_cov),
                     fn=function(N_tot){ abs(eval(p.body2)$pow - pow)}, 
                     c(2*length(mu)+n_cov, 1000000000), 
                     control=list(warn.1d.NelderMead = FALSE))$par
      res <- eval(p.body2)
      beta_level <- 1-res$pow
      if(round_up == TRUE && (!(N_tot%%1==0) || !any(res$nvec%%1==0))) {
        N_tot = length(mu)*ceiling(N_tot / length(mu))

        res <- eval(p.body2)

      }
      pow_res[[i1]] = list(
        cmat = cmat,
        mu = mu, 
        nvec = res$nvec, 
        n_cov = res$n_cov, 
        r2 = res$r2, 
        sd = sd, 
        alpha_level = res$alpha_level,
        pow = res$pow,
        beta_level = res$beta_level,
        num_df = res$num_df,
        den_df = res$den_df,
        N_tot = res$N_tot
      )
    }
    names(pow_res) = names(cmats)
    
    if(length(cons) != 0){
      for(i1 in 1:length(cons)){
        cmat = cons[[i1]]
        N_tot <- optim(par=(2*length(mu)+n_cov),
                       fn=function(N_tot){ abs(eval(p.body2)$pow - pow)}, 
                       c(2*length(mu)+n_cov, 1000000000), 
                       control=list(warn.1d.NelderMead = FALSE))$par
        res <- eval(p.body2)
        beta_level <- 1-res$pow
        if(round_up == TRUE && (!(N_tot%%1==0) || !any(res$nvec%%1==0))) {
          N_tot = length(mu)*ceiling(N_tot / length(mu))
          
          res <- eval(p.body2)
          
        }
        con_res[[i1]] = list(
          cmat = cmat,
          mu = mu, 
          nvec = nvec, 
          n_cov = n_cov, 
          r2 = res$r2, 
          sd = sd, 
          alpha_level = res$alpha_level,
          pow = res$pow,
          beta_level = beta_level,
          num_df = res$num_df,
          den_df = res$den_df,
          N_tot = res$N_tot
        )
      }
      names(con_res) = names(cons)
    }
    
    
  }else {
    stop("internal error: exactly one of n, r2, beta_level, and alpha_level must be NULL")
  }

  df_pow = data.frame(factor = character(),
                      N_tot = integer(),
                      n_cov = integer(),
                      r2 = double(),
                      alpha_level = double(),
                      beta_level = double(),
                      power = double())
  
  for(i1 in 1:length(pow_res)){
    df_pow[i1,]$factor = factorlist[i1]
    df_pow[i1,]$N_tot = pow_res[[i1]]$N_tot
    df_pow[i1,]$n_cov = pow_res[[i1]]$n_cov
    df_pow[i1,]$r2 = pow_res[[i1]]$r2
    df_pow[i1,]$alpha_level = pow_res[[i1]]$alpha_level
    df_pow[i1,]$beta_level = pow_res[[i1]]$beta_level
    df_pow[i1,]$power = pow_res[[i1]]$pow*100
    
    pow_res2[[i1]] =   structure(list(dfs = c(pow_res[[i1]]$num_df,
                                              pow_res[[i1]]$den_df), 
                                      N = pow_res[[i1]]$N_tot, 
                                      n = pow_res[[i1]]$nvec,
                                      n_cov = pow_res[[i1]]$n_cov,
                                      mu = pow_res[[i1]]$mu,
                                      sd = pow_res[[i1]]$sd,
                                      r2 = pow_res[[i1]]$r2,
                                      alpha_level = pow_res[[i1]]$alpha_level, 
                                      beta_level = pow_res[[i1]]$beta_level,
                                      power = pow_res[[i1]]$pow*100,
                                      type = TYPE,
                                      method = METHOD), class = "power.htest")
  }
  names(pow_res2) = names(pow_res)
  rownames(df_pow)  = NULL
  
  if(length(cons) == 0){
    con_res = NULL
    con_res2 = NULL
    df_con = NULL
  } else{
    df_con = data.frame(contrast = character(),
                        N_tot = integer(),
                        n_cov = integer(),
                        r2 = double(),
                        alpha_level = double(),
                        beta_level = double(),
                        power = double())
    
    for(i1 in 1:length(con_res)){
      df_con[i1,]$contrast = names(con_res)[i1]
      df_con[i1,]$N_tot = con_res[[i1]]$N_tot
      df_con[i1,]$n_cov = con_res[[i1]]$n_cov
      df_con[i1,]$r2 = con_res[[i1]]$r2
      df_con[i1,]$alpha_level = con_res[[i1]]$alpha_level
      df_con[i1,]$beta_level = con_res[[i1]]$beta_level
      df_con[i1,]$power = con_res[[i1]]$pow*100
      
      con_res2[[i1]] =   structure(list(dfs = c(con_res[[i1]]$num_df,
                                                con_res[[i1]]$den_df), 
                                        N = con_res[[i1]]$N_tot, 
                                        n = con_res[[i1]]$nvec,
                                        n_cov = con_res[[i1]]$n_cov,
                                        contrast = con_res[[i1]]$cmat,
                                        mu = con_res[[i1]]$mu,
                                        sd = con_res[[i1]]$sd,
                                        r2 = con_res[[i1]]$r2,
                                        alpha_level = con_res[[i1]]$alpha_level, 
                                        beta_level = con_res[[i1]]$beta_level,
                                        power = con_res[[i1]]$pow*100,
                                        type = TYPE,
                                        method = METHOD2), class = "power.htest")
    }
    names(con_res2) = names(con_res)
    rownames(df_con)  = NULL
  }


  structure(
    list(
      main_results = df_pow,
      aov_list = pow_res2,
      contrast_results = df_con,
      con_list = con_res2,
      design_params = list(
        design = design,
        mu = mu,
        sd = sd,
        label_list = label_list
      )
    ),
    class = "ancova_power"
  )
  
  
}
