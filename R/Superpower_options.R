#' Set/get global Superpower options
#'
#' Global Superpower options are used, for example, by \code{\link{ANOVA_exact}} (et al.)
#' and \code{\link{ANOVA_power}}. But can be changed in each functions directly using
#' an argument (which has precedence over the global options).
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2)
#'   a name of an option element, then returns its' value; (3) a name-value pair
#'   which sets the corresponding option to the new value (and returns nothing),
#'   (4) a list with option-value pairs which sets all the corresponding
#'   arguments. The example show all possible cases.
#' 
#' @details The following arguments are currently set:
#' \itemize{
#' \item \code{verbose} should verbose (printed results) be set to true? Default is \code{TRUE}.
#' \item \code{emm} Option to perform analysis of estimated marginal means. Default is \code{FALSE}.
#' \item \code{emm_model} Model type ("multivariate", or "univariate") for estimated marginal means. Default is \code{"multivariate"}.
#' \item \code{contrast_type} The type of comparison for the estimated marginal means. Default is \code{"pairwise"}. See ?emmeans::'contrast-methods' for more details on acceptable methods.
#' \item \code{plot} Option to automatically print plots. Default is \code{FALSE}.
#' \item \code{alpha_level} Alpha level used to determine statistical significance. Default is .05.
#' \item \code{correction} Option to set a correction for sphericity violations. Default is no correction. This can be set to "none", "GG" Greenhouse-Geisser, and "HF" Huynh-Feldt
#' \item \code{liberal_lambda} Option to set a logical indicator of whether to use the liberal (cohen_f^2\*(num_df+den_df)) or conservative (cohen_f^2\*den_df) calculation of the noncentrality (lambda) parameter estimate. Default is FALSE.
#' }
#' 
#' @note All options are saved in the global R \code{\link{options}} with prefix
#'   \code{Superpower.}
#'   
#' @return depends on input, see above.
#' @export


Superpower_options <- function(...) {
  dots <- list(...)
  #browser()
  if (length(dots) == 0) {  # branch to get all Superpower options
    op <- options()
    Superpower_op <- op[grepl("^Superpower.", names(op))]
    names(Superpower_op) <- sub("^Superpower.", "", names(Superpower_op))
    return(Superpower_op)
  } else if (is.list(dots[[1]])) {  # set several Superpower options as a list:
    newop <- dots[[1]]
    names(newop) <- paste0("Superpower.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    newop <- dots
    names(newop) <- paste0("Superpower.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {  # get a single Superpower options
    if (length(dots) > 1) stop("Superpower_options() can only return the value of a single option.", call. = FALSE)
    return(getOption(paste0("Superpower.", unlist(dots))))
  } else {
    warning("Unsupported command to Superpower_options(), nothing done.", call. = FALSE)
  }
}