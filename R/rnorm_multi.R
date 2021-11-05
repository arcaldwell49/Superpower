rnorm_multi <- function(n = 100, vars = NULL, mu = 0, sd = 1, r = 0,
                        varnames = NULL, empirical = FALSE, 
                        as.matrix = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
    #   # reinstate system seed after simulation
    #   gs <- global_seed(); on.exit(global_seed(gs))
  }
  
  # error handling ----
  if ( !is.numeric(n) || n %% 1 > 0 || n < 1 ) {
    stop("n must be an integer > 0")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  # try to guess vars if not set ----
  if (is.null(vars)) {
    if (!is.null(varnames)) {
      vars <- length(varnames)
    } else if (length(mu) > 1) {
      vars <- length(mu)
    } else if (length(sd) > 1) {
      vars <- length(sd)
    } else if (is.matrix(r)) {
      vars <- ncol(r)
    }
    
    if (is.null(vars)) {
      stop("The number of variables (vars) was not explicitly set and can't be guessed from the input.")
    }
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  } else {
    # get rid of names
    #mu <- as.matrix(mu) %>% as.vector()
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  } else {
    # get rid of names
    #sd <- as.matrix(sd) %>% as.vector()
  }
  
  if (n == 1 & empirical == TRUE) {
    warning("When n = 1 and empirical = TRUE, returned data are equal to mu")
    mvn <- mu
    cor_mat <- r # for name-checking later
  } else {
    # get data from mvn ----
    cor_mat <- cormat(r, vars)
    sigma <- (sd %*% t(sd)) * cor_mat
    # tryCatch({
    #   mvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
    # }, error = function(e) {
    #   stop("The correlated variables could not be generated. If empirical = TRUE, try increasing the N or setting empirical = FALSE.")
    # })
    
    err <- "The correlated variables could not be generated."
    if (empirical) err <- paste(err, "Try increasing the N or setting empirical = FALSE.")
    
    # code from MASS:mvrnorm
    p <- length(mu)
    if (!all(dim(sigma) == c(p, p))) stop(err)
    eS <- eigen(sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -1e-06 * abs(ev[1L]))) stop(paste(err))
    X <- matrix(stats::rnorm(p * n), n)
    if (empirical) {
      X <- scale(X, TRUE, FALSE)
      X <- X %*% svd(X, nu = 0)$v
      X <- scale(X, FALSE, TRUE)
    }
    tryCatch({
      X <- drop(mu) + eS$vectors %*% 
        diag(sqrt(pmax(ev, 0)), p) %*%  t(X)
    }, error = function(e) { stop(err) })
    
    mvn <- t(X)
  }
  
  # coerce to matrix if vector when n == 1
  if (n == 1) mvn <- matrix(mvn, nrow = 1)
  
  if (length(varnames) == vars) {
    colnames(mvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if r was a matrix with names, use that
    colnames(mvn) <- colnames(cor_mat)
  } else if (!is.null(names(mu))) {
    #use mu names 
    colnames(mvn) <- names(mu)
  } else if (!is.null(names(sd))) {
    #use sd names 
    colnames(mvn) <- names(sd)
  } else {
    colnames(mvn) <- make_id(ncol(mvn), "X")
  }
  
  if (as.matrix == TRUE) mvn else data.frame(mvn, check.names = FALSE)
}

cormat = function (cors = 0, vars = 3) 
{
  if (is.numeric(cors) & length(cors) == 1) {
    if (cors >= -1 & cors <= 1) {
      cors = rep(cors, vars * (vars - 1)/2)
    }
    else {
      stop("cors must be between -1 and 1")
    }
  }
  if (vars == 1) {
    cor_mat <- matrix(1, nrow = 1)
  }
  else if (is.matrix(cors)) {
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    }
    else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    }
    else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    }
    else {
      cor_mat <- cors
    }
  }
  else if (length(cors) == vars * vars) {
    cor_mat <- matrix(cors, vars)
  }
  else if (length(cors) == vars * (vars - 1)/2) {
    cor_mat <- cormat_from_triangle(cors)
  }
  if (!is_pos_def(cor_mat)) {
    stop("correlation matrix not positive definite")
  }
  return(cor_mat)
}

cormat_from_triangle <- function (cors) 
{
  vars <- ceiling(sqrt(2 * length(cors)))
  if (length(cors) != vars * (vars - 1)/2) 
    stop("you don't have the right number of correlations")
  cor_mat <- matrix(nrow = vars, ncol = vars)
  upcounter = 1
  lowcounter = 1
  for (col in 1:vars) {
    for (row in 1:vars) {
      if (row == col) {
        cor_mat[row, col] = 1
      }
      else if (row > col) {
        cor_mat[row, col] = cors[lowcounter]
        lowcounter <- lowcounter + 1
      }
    }
  }
  for (row in 1:vars) {
    for (col in 1:vars) {
      if (row < col) {
        cor_mat[row, col] = cors[upcounter]
        upcounter <- upcounter + 1
      }
    }
  }
  cor_mat
}

is_pos_def <- function (cor_mat, tol = 1e-08) 
{
  ev <- eigen(cor_mat, only.values = TRUE)$values
  sum(ev < tol) == 0
}

make_id <- function(n = 100, prefix = "S", digits = 0, suffix = "") 
{
  if (!is.numeric(n)) 
    stop("n must be numeric")
  if (length(n) == 1) 
    n <- 1:n
  max_digits <- as.character(n) %>% nchar() %>% max() %>% 
    max(digits)
  max_decimal <- as.character(n) %>% sub("(^\\d*\\.|^\\d*$)", 
                                         "", .) %>% nchar() %>% max()
  fmt <- paste0(prefix, "%0", max_digits, ".", max_decimal, 
                "f", suffix)
  sprintf(fmt, n)
}

