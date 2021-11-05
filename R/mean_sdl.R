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