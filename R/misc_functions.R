ci_binom = function(centx,n,conf_level=.95){
  p <- centx/100
  x <- p*n
  alpha <- 1 - conf_level
  alpha2 <- 0.5 * alpha
  z <- qnorm(1 - alpha2)
  z2 <- z * z
  # Wilson, E. B. (1927). Probable Inference, the Law of Succession, and Statistical Inference. Journal of the American Statistical Association, 22(158), 209–212. https://doi.org/10.1080/01621459.1927.10502953 
  p1 <- p + 0.5 * z2/n
  p2 <- z * sqrt((p * (1 - p) + 0.25 * z2/n)/n)
  p3 <- 1 + z2/n
  lcl <- (p1 - p2)/p3
  ucl <- (p1 + p2)/p3
  res = c(lcl,ucl)
  return(res)
}



smean.sdl = function (x, mult = 1, na.rm = TRUE) {
  if (na.rm) 
    x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) 
    return(c(Mean = NA, Lower = NA, Upper = NA))
  xbar <- sum(x)/n
  sd <- sqrt(sum((x - xbar)^2)/(n - 1))
  c(Mean = xbar, Lower = xbar - mult * sd, Upper = xbar + 
      mult * sd)
}