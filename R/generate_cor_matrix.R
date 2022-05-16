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
  if (inherits(cors, "numeric") & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (inherits(cors,  "matrix")) {
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