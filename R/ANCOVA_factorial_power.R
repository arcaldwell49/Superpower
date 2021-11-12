pow_anc_meth = function(cmat,
                        mu, # Vector of Group Means
                        nvec, # Group Sample Sizes
                        n_cov, # Number of Covariates
                        r2, # Coefficient of Determination
                        sd, # SD UNADJUSTED
                        alpha_level # Alpha level
                        ){
  
  n_grp = length(mu)
  mu = mu
  var_e = (sd^2*(1-r2))
  
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
  l_gamma2 <-
    t(cmu) %*% solve(cmat %*% qmat %*% t(cmat)) %*% cmu / var_e
  
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
                          lower.tail = FALSE)
    )
  }
  
  return(list(pow = pow,
              num_df = num_df,
              den_df = den_df,
              N_tot = N_tot,
              beta_level = 1-pow,
              alpha_level = alpha_level,
              cmat = cmat,
              mu = mu, 
              nvec = nvec, 
              n_cov = n_cov, 
              r2 = r2, 
              sd = sd))
}


