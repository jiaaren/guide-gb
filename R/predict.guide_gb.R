#' Predict from a guide_gb model
#'
#' @param object A fitted "guide_gb" object.
#' @param newdata A data frame of new predictor values.
#' @param n_trees Number of trees to use for prediction (default: all).
#' @param type Type of prediction: "link" for raw predictions (log odds for classification), 
#'   "response" for probabilities (classification only).
#' @param ... Additional arguments passed to underlying predict functions.
#'
#' @return Predicted values (numeric for regression, log odds or probabilities for classification).
predict.guide_gb <- function(object, newdata, n_trees = NULL, type = c("link", "response"), ...) {
  type <- match.arg(type)
  pred_func <- get_pred_func(object$guide_pred_type)
  n_trees <- n_trees %||% object$fit$iterations
  # raise error if n_trees exceeds fitted iterations
  if (max(n_trees) > object$fit$iterations) {
    stop(paste(max(n_trees), "n_trees exceeds fitted iterations of", object$fit$iterations))
  }
  # handle missingness indicators
  for (col in object$missing_num_vars) {
    missing_col <- paste0(col, ".NA")
    newdata[[missing_col]] <- ifelse(is.na(newdata[[col]]), 1, 0)
  }

  if (object$type == "regression") {
    return(make_regressor_prediction(object$fit, x = newdata, pred_func = pred_func, n_trees = n_trees, ...))
  }
  if (object$type == "binary_classification") {
    predLogOdds <- make_classifier_prediction(object$fit, x = newdata, n_trees = n_trees)
    if (type == "response") {
      return(plogis(predLogOdds))
    }
    return(predLogOdds)
  }
}
