#' Summary of guide_gb model
#'
#' @param object A "guide_gb" object.
#' @param ... Additional arguments.
#' @export
summary.guide_gb <- function(object, ...) {
  cat("GUIDE Gradient Boosting Model Summary\n")
  cat("======================================\n\n")
  cat("Model type:", object$type, "\n")
  cat("Number of observations:", object$nobs, "\n")
  cat("Number of predictors:", length(object$predictors), "\n")
  cat("Predictors:", paste(object$predictors, collapse = ", "), "\n\n")
  cat("Training configuration:\n")
  cat("  Iterations:", object$fit$iterations, "\n")
  cat("  Learning rate:", object$eta, "\n")
  cat("\nTraining error history:\n")
  if (object$type == "regression") {
    cat("  Initial MSE:", object$fit$err[1], "\n")
    cat("  Final MSE:", object$fit$err[length(object$fit$err)], "\n")
  } else {
    cat("  Initial log-likelihood:", object$fit$err[1], "\n")
    cat("  Final log-likelihood:", object$fit$err[length(object$fit$err)], "\n")
  }
  if (!is.null(object$fit$err_val)) {
    cat("\nValidation error history:\n")
    if (object$type == "regression") {
      cat("  Initial validation MSE:", object$fit$err_val[1], "\n")
      cat("  Final validation MSE:", object$fit$err_val[length(object$fit$err_val)], "\n")
    } else {
      cat("  Initial validation log-likelihood:", object$fit$err_val[1], "\n")
      cat("  Final validation log-likelihood:", object$fit$err_val[length(object$fit$err_val)], "\n")
    }
  }
}
