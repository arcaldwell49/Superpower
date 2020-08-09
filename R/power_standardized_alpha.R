#' Optimizing function to achieve desired power based on a standardized alpha level.
#'
#' Because the standardized alpha depends on the sample size (N), and the power depends on the sample size, deciding upon the sample size to achieve a desired power requires an iterative procedure. Increasing the sample size reduces the standardized alpha, which requires an increase in the sample size for the power analysis, which reduces the standardized alpha. THis function takes a power analysis function that outputs the power as a function of the desired power, the alpha level, as a function of N(x).
#' @param power_function Function that outputs the power, calculated with an analytic function.
#' @param alpha The unstandardized alpha level (e.g., 0.05), independent of the sample size.
#' @param power The desired power, i.e., the outcome of the power calculation you would like to achieve.
#' @param standardize_N The sample size you want to use to standardize the alpha level for. Defaults to 100 (base on Good, 1982).
#' @param verbose Print each iteration of the optimization function if TRUE. Defaults to FALSE.
#' @return
#' mean_mat = matrix of the means
#'
#' @examples
#' res <- power_standardized_alpha(power_function = "pwr::pwr.t.test(d = 0.3,
#' n = x, sig.level = a_stan, type = 'two.sample',
#' alternative = 'two.sided')$power", power = 0.9, alpha = 0.05)
#' res$N
#' @section References:
#' too be added
#' @importFrom stats optimize
#' @export
#'

power_standardized_alpha <- function(power_function, alpha = 0.05, power = 0.8, standardize_N = 100, verbose = FALSE) {

  #Run optimize to find the minimum
  res <- optimize(f,
                  c(3, 1000),
                  tol = 0.001,
                  power_function = power_function,
                  power = power)
  a_stan <- alpha/sqrt(ceiling(res$minimum)/standardize_N)
  #Store results
  invisible(list(N = ceiling(res$minimum),
                 a_stan = a_stan,
                 objective = res$objective
  ))
}

#Define the function to be minimized
f = function(x, power_function, power) {
  #Calculate standardized alpha based on current N (x), the unstandardized alpha, and standardizer N
  a_stan <- alpha/sqrt(x/standardize_N)
  #Calculate power
  y <- eval(parse(text=paste(power_function)))
  if(verbose == TRUE){
    print(x, y, max(y - power, power - y))
  }
  max(y - power, power - y)
}
