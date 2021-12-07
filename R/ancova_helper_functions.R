
# Shieh function for one way ANOVA
pwr_ancova_shieh <- function (mu, # Vector of Group Means
                              n, # Group Sample Sizes
                              n_cov, # Number of Covariates
                              r2, # Coefficient of Determination
                              sd, # SD UNADJUSTED
                              alpha_level # Alpha level
) {
  n_grp = length(mu)
  mu = mu
  nvec = n
  var_e = (sd^2*(1-r2))
  cmat = t(contr.sum(length(nvec))) # Contrast matrix
  # Create matrix for Wald statistic (W star; eq 10 from Shieh)
  numint <- 2000
  dd <- 1e-5
  coevec <- c(1, 
              rep(c(4, 2), numint / 2 - 1),
              4, 1)
  bl <- dd
  bu <- 1 - dd
  intl <- (bu - bl) / numint
  bvec <- bl + intl * (0:numint)
  cmu <- cmat %*% matrix(mu, n_grp, 1)
  # Derive other details
  num_df <- nrow(cmat)
  N_tot <- sum(nvec)
  qmat <- diag(N_tot / nvec)
  # Matrix multiplication to get lower case gamma squared (eq. 23 from Shieh)
  l_gamma2 <- t(cmu) %*% solve(cmat %*% qmat %*% t(cmat)) %*% cmu / var_e
  
  N_tot <- sum(nvec)
  den_df <- N_tot - n_grp - n_cov
  dfx <- den_df + 1
  b <- n_cov / dfx
  # Get critical F-statistic
  fcrit <- qf(1 - alpha_level, num_df, den_df)
  # Solution differs by number of covariates
  if (n_cov == 1) {
    tl <- qt(dd, dfx)
    tu <- qt(1 - dd, dfx)
    intl <- (tu - tl) / numint
    tvec <- tl + intl * (0:numint)
    wtpdf <- (intl / 3) * coevec * dt(tvec, dfx)
    
    pow <- sum(wtpdf * pf(
      fcrit,
      num_df,
      den_df,
      c(N_tot * l_gamma2) / (1 + b * tvec ^ 2),
      lower.tail = FALSE
    ))
  }
  else {
    wbpdf <- (intl / 3) * coevec * dbeta(bvec, dfx / 2, n_cov / 2)
    pow <- sum(wbpdf * pf(fcrit,
                          num_df,
                          den_df,
                          c(N_tot * l_gamma2) * bvec,
                          lower.tail = FALSE))
  }
  
  
  return(pow)
  
}

# Keppel (PASS) function for one way ANOVA
pwr_ancova_keppel = function(mu, # Vector of Group Means
                             n, # Group Sample Sizes
                             n_cov, # Number of Covariates
                             r2, # Coefficient of Determination
                             sd, # SD UNADJUSTED
                             alpha_level # Alpha level
){
  num_df = length(mu) - 1
  N_tot = sum(n)
  
  pow = pf(qf(alpha_level, 
              num_df,
              (N_tot - length(mu) - n_cov ), 
              lower.tail =  FALSE), 
           num_df, 
           (N_tot - length(mu) - n_cov ), 
           (anc_var_m(n,mu)/(sd^2*(1-r2))) * (N_tot / length(mu)) * length(mu), 
           lower.tail = FALSE)
  
  return(pow)
}

# helper function for adding
anc_var_m = function(n, mu) {
  N = sum(n)
  
  G = length(mu)
  
  mu_bar = sum((n/N) * mu)
  
  N_bar = N/G
  
  var_m = sum((n/N)*(mu-mu_bar)^2)
  
  return(var_m)
  
}

# selection of type of method for ANCOVA power

pwr_method <- function(mu,
                       n,
                       n_cov,
                       r2,
                       sd,
                       alpha_level, type) {
  switch(type,
         exact = pwr_ancova_shieh(mu,
                                  n,
                                  n_cov,
                                  r2,
                                  sd,
                                  alpha_level),
         approx = pwr_ancova_keppel(mu,
                                    n,
                                    n_cov,
                                    r2,
                                    sd,
                                    alpha_level))
}





