make_prediction_tree_regressor <- function(x, tree_func) {
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
  results = lapply(fit$trees, function(f) make_prediction_tree_regressor(x, f)$pred)
  results = do.call(cbind, results)
  mult = sweep(results, 2, fit$eta, `*`)
  rowSums(mult) + fit$basepred
}
# iteration_pred <- t(apply((sweep(results, 2, model$fit$eta, `*`)), 1, cumsum) + mean(y))
# sense check over predictions
# apply(iteration_pred, 2, function(pred){ rmse(pred - y) }) - model$fit$err

fit <- model$fit

make_prediction_tree_classifier(x, fit$trees[[1]])
tree_func <- fit$trees[[1]]

make_prediction_tree_classifier <- function(x, tree_func) {
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
    pred[i] <- as.numeric(tmp[2])
  }
  return (data.frame(node, pred))
}

# nodes <- results[[1]]
# tree_map <- fit$tree_maps[[1]]

map_logodds <- function(tree_map, nodes) {
  sapply(nodes, function(n) tree_map[[as.character(n)]])
}

# returns a matrix of gradients, rows = observations, cols = iterations
get_iteration_gradients_classifier <- function(fit, x) {
  # classifier references node
  results = lapply(fit$trees, function(f) make_prediction_tree_classifier(x, f)$node)
  mapped_results <- mapply(map_logodds, fit$tree_maps, results, SIMPLIFY = TRUE)
  mult = sweep(mapped_results, 2, fit$eta, `*`)
  return(mult)
}

make_classifier_prediction <- function(fit, x) {
  mult = get_iteration_gradients_classifier(fit, x)
  predLogOdds <- rowSums(mult) + fit$basepred
  pred <- 1/(1+exp(-predLogOdds))
  return(pred)
}

sense_check_calc <- function(object, newdata, ...) {
  if (object$type == "regression") {
    preds <- make_regressor_prediction(object$fit, x = newdata, ...)
    return(preds)
  }
  if (object$type == "binary_classification") {
    mult = get_iteration_gradients_classifier(object$fit, newdata)
    loglik_matrix <- t(apply(mult, 1, cumsum)) + object$fit$basepred
    apply(loglik_matrix, 2, function(predLogOdds){ pred <- 1/(1+exp(-predLogOdds)); loglik(y, pred) }) - object$fit$err
  }
}

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
  if (object$type == "binary_classification") {
    return(make_classifier_prediction(object$fit, x = newdata, ...))
    # probs <- predict(object$fit, newdata = newdata, type = "response", ...)
    # preds <- ifelse(probs > 0.5, 1, 0)
    # return(preds)
  }
}
