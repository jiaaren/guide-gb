#' Predict from a custom model
#'
#' @param object A fitted "my_model" object.
#' @param newdata A data frame of new predictor values.
#' @param ... Additional arguments passed to underlying predict functions.
#'
#' @return Predicted values (numeric for regression, class labels for classification).
#' @export
predict.my_model <- function(object, newdata, ...) {
  if (object$type == "regression") {
    return(predict(object$fit, newdata = newdata, ...))
  }
  if (object$type == "classification") {
    probs <- predict(object$fit, newdata = newdata, type = "response", ...)
    preds <- ifelse(probs > 0.5, 1, 0)
    return(preds)
  }
}
