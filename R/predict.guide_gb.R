make_prediction_tree <- function(x, tree_func) {
  # Create myfunc inside an environment where bip exists
  environment(tree_func) <- environment()
  node <- c()
  fitvar <- c()
  pred <- c()
  cols <- colnames(x)
  for(i in 1:nrow(x)){
    for (col in cols) {
      assign(col, as.numeric(x[[col]][i]))
    }
    tmp <- tree_func()
    node[i] <- as.numeric(tmp[1])
    fitvar[i] <- tmp[2]
    pred[i] <- as.numeric(tmp[3])
  }
  return (data.frame(node, fitvar, pred))
}

make_regressor_prediction <- function(fit, x) {
  # explore how to use node and fitvar next time
  results = lapply(fit$trees, function(f) make_prediction_tree(x, f)$pred)
  results = do.call(cbind, results)
  mult = sweep(results, 2, fit$eta, `*`)
  rowSums(mult) + fit$basepred
}
# iteration_pred <- t(apply((sweep(results, 2, model$fit$eta, `*`)), 1, cumsum) + mean(y))
# sense check over predictions
# apply(iteration_pred, 2, function(pred){ rmse(pred - y) }) - model$fit$err

#' Predict from a custom model
#'
#' @param object A fitted "my_model" object.
#' @param newdata A data frame of new predictor values.
#' @param ... Additional arguments passed to underlying predict functions.
#'
#' @return Predicted values (numeric for regression, class labels for classification).
#' @export
predict.guide_gb <- function(object, newdata, ...) {
  if (object$type == "regression") {
    return(make_regressor_prediction(object$fit, x = newdata, ...))
  }
  if (object$type == "classification") {
    probs <- predict(object$fit, newdata = newdata, type = "response", ...)
    preds <- ifelse(probs > 0.5, 1, 0)
    return(preds)
  }
}
