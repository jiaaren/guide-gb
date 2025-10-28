make_prediction_tree_a <- function(x, tree_func) {
  # initialise
  n <- nrow(x)
  node <- numeric(n)
  pred <- numeric(n)
  # obtain function body and environment
  func_body <- body(tree_func)
  func_enclosure <- environment(tree_func)
  for(i in 1:n) {
    # performing lapply is faster than as.list()
    # row_list <- as.list(x[i, ])
    row_list <- lapply(x, `[`, i)
    # evaluate the function body in an environment
    tmp <- eval(func_body, envir = row_list, enclos = func_enclosure)
    node[i] <- as.numeric(tmp[1])
    pred[i] <- as.numeric(tmp[2])
  }
  return (data.frame(node, pred))
}

make_prediction_tree_b <- function(x, tree_func) {
  # initialise
  n <- nrow(x)
  node <- numeric(n)
  pred <- numeric(n)
  # obtain function body and environment
  func_body <- body(tree_func)
  func_enclosure <- environment(tree_func)
  for(i in 1:n) {
    # performing lapply is faster than as.list()
    # row_list <- as.list(x[i, ])
    row_list <- lapply(x, `[`, i)
    # evaluate the function body in an environment
    tmp <- eval(func_body, envir = row_list, enclos = func_enclosure)
    node[i] <- as.numeric(tmp[1])
    pred[i] <- as.numeric(tmp[3])
  }
  return (data.frame(node, pred))
}

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

# fit <- model$fit
# pred_func <- get_pred_func(model$guide_pred_type)
# results <- lapply(fit$trees, function(f) pred_func(test_x, f)$pred)
# results <- do.call(cbind, results)

# # cumsum results
# mult <- sweep(results, 2, fit$eta, `*`)
# iteration_pred <- t(apply(mult, 1, cumsum) + fit$basepred)
# # calculate RMSE at each iteration
# # sense check over predictions
# res <- apply(iteration_pred, 2, function(pred){ rmse(pred - test_y) })
# # find lowest iteration RMSE, idx
# min(res)
# which.min(res)
# # round(res - fit$err, 2)



make_regressor_prediction <- function(fit, x, pred_func) {
  # explore how to use node and fitvar next time
  results <- lapply(fit$trees, function(f) pred_func(x, f)$pred)
  results <- do.call(cbind, results)
  mult <- sweep(results, 2, fit$eta, `*`)
  rowSums(mult) + fit$basepred
}

## visualising benefits of early stopping
# mult <- mult + fit$basepred
# iteration_pred <- t(apply(mult, 1, cumsum) + fit$basepred)
# # sense check over predictions
# res <- apply(iteration_pred, 2, function(pred){ rmse(pred - y) })
# round(res - fit$err, 2)


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
  results = lapply(fit$trees, function(f) make_prediction_tree_a(x, f)$node)
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

get_pred_func <- function(guide_pred_type) {
  if (guide_pred_type == "a") {
    return(make_prediction_tree_a)
  } else if (guide_pred_type == "b") {
    return(make_prediction_tree_b)
  } else {
    stop("Unknown guide_pred_type")
  }
}

sense_check_calc <- function(object, newdata, ...) {
  pred_func <- get_pred_func(object$guide_pred_type)
  if (object$type == "regression") {
    preds <- make_regressor_prediction(object$fit, x = newdata, pred_func=pred_func)
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
  pred_func <- get_pred_func(object$guide_pred_type)
  for (col in object$missing_num_vars) {
    missing_col <- paste0(col, ".NA")
    newdata[[missing_col]] <- is.na(newdata[[col]])
    # not sure which one is it, to inspect R generated code
    # newdata[[missing_col]] <- ifelse(is.na(newdata[[col]]), 1, 0)
  }

  if (object$type == "regression") {
    return(make_regressor_prediction(object$fit, x = newdata, pred_func=pred_func, ...))
  }
  if (object$type == "binary_classification") {
    return(make_classifier_prediction(object$fit, x = newdata, ...))
    # probs <- predict(object$fit, newdata = newdata, type = "response", ...)
    # preds <- ifelse(probs > 0.5, 1, 0)
    # return(preds)
  }
}
