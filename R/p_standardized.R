#' Compute standardized alpha level based on unstandardized alpha level and the number of observations N.
#' @param p The observed p-value.
#' @param N The number of observations (e.g., the sample size) in the dataset
#' @param standardize_N The nuber of observations (e.g., the sample size) you want to use to standardize the alpha level for. Defaults to 100 (base on Good, 1982).
#' @examples
#' ## Check it yields .05 for N = 100:
#' p_standardized(p = 0.05, N = 100)
#' ## Check it yields .05 for N = 200, p = 0.03535534:
#' p_standardized(p = 0.03535534, N = 200)
#' ## What is a standardized p-value for p = .05 and N = 200?
#' p_standardized(p = 0.05, N = 200)
#' ## You can change the standardization N, repeating the example above:
#' p_standardized(p = 0.05, N = 100, standardize_N = 200)
#' @section References:
#' Good, I. J. (1982). C140. Standardized tail-area probabilities. Journal of Statistical Computation and Simulation, 16(1), 65–66. <https://doi.org/10.1080/00949658208810607>
#' @export
#'
p_standardized <- function(p, N, standardize_N = 100){
  p * sqrt(N/standardize_N)
}
