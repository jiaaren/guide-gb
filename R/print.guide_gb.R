#' Print a guide_gb model
#'
#' @param x A "guide_gb" object.
#' @param ... Additional arguments.
#' @export
print.guide_gb <- function(x, ...) {
  cat("GUIDE Gradient Boosting Model\n")
  cat("Type:", x$type, "\n")
  cat("Number of observations:", x$nobs, "\n")
  cat("Number of predictors:", length(x$predictors), "\n")
  cat("Number of iterations:", x$fit$iterations, "\n")
  cat("Learning rate (eta):", x$eta, "\n")
}
