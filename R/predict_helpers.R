#' Make predictions using tree function (type a)
#' 
#' @param x Data frame
#' @param tree_func Tree prediction function
#' @return Data frame with node and pred columns
#' @keywords internal
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
  return(data.frame(node, pred))
}

#' Make predictions using tree function (type b)
#' 
#' @param x Data frame
#' @param tree_func Tree prediction function
#' @return Data frame with node and pred columns
#' @keywords internal
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
  return(data.frame(node, pred))
}

#' Make predictions for regressor
#' 
#' @param x Data frame
#' @param tree_func Tree prediction function
#' @return Data frame with node, fitvar, and pred columns
#' @keywords internal
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
  return(data.frame(node, fitvar, pred))
}

#' Make predictions for classifier
#' 
#' @param x Data frame
#' @param tree_func Tree prediction function
#' @return Data frame with node and pred columns
#' @keywords internal
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
  return(data.frame(node, pred))
}

#' Map tree nodes to log odds
#' 
#' @param tree_map Tree mapping list
#' @param nodes Vector of node IDs
#' @return Vector of log odds values
#' @keywords internal
map_logodds <- function(tree_map, nodes) {
  sapply(nodes, function(n) tree_map[[as.character(n)]])
}

#' Get iteration gradients for regressor
#' 
#' Returns a matrix of gradients, rows = observations, cols = iterations
#' 
#' @param fit Fitted model object
#' @param x Data frame
#' @param pred_func Prediction function
#' @param n_trees Number of trees (NULL for all)
#' @return Matrix of gradients
#' @keywords internal
get_iteration_gradients_regressor <- function(fit, x, pred_func, n_trees = NULL) {
  # n_trees passed here cannot be a vector, hence max
  n_trees <- max(n_trees %||% fit$iterations)
  # TODO: explore how to use node and fitvar next time
  results <- lapply(fit$trees[1:n_trees], function(f) pred_func(x, f)$pred)
  results <- do.call(cbind, results)
  sweep(results, 2, fit$eta[1:n_trees], `*`)
}

#' Get iteration gradients for classifier
#' 
#' Returns a matrix of gradients, rows = observations, cols = iterations
#' 
#' @param fit Fitted model object
#' @param x Data frame
#' @param n_trees Number of trees (NULL for all)
#' @return Matrix of gradients
#' @keywords internal
get_iteration_gradients_classifier <- function(fit, x, n_trees = NULL) {
  # n_trees passed here cannot be a vector, hence max
  n_trees <- max(n_trees %||% fit$iterations) 
  # classifier references node
  results <- lapply(fit$trees[1:n_trees], function(f) make_prediction_tree_a(x, f)$node)
  mapped_results <- mapply(map_logodds, fit$tree_maps, results, SIMPLIFY = TRUE)
  sweep(mapped_results, 2, fit$eta[1:n_trees], `*`)
}

#' Make regressor predictions
#' 
#' @param fit Fitted model object
#' @param x Data frame
#' @param pred_func Prediction function
#' @param n_trees Number of trees
#' @return Vector or matrix of predictions
#' @keywords internal
make_regressor_prediction <- function(fit, x, pred_func, n_trees) {
  mult <- get_iteration_gradients_regressor(fit, x, pred_func, n_trees)
  # check if n_trees is a scalar or vector
  if (length(n_trees) > 1) {
    iteration_pred <- t(apply(mult, 1, cumsum)) + fit$basepred
    return(iteration_pred[, n_trees, drop = FALSE])
  }
  rowSums(mult) + fit$basepred
}

#' Make classifier predictions
#' 
#' @param fit Fitted model object
#' @param x Data frame
#' @param n_trees Number of trees
#' @return Vector or matrix of log odds predictions
#' @keywords internal
make_classifier_prediction <- function(fit, x, n_trees) {
  mult <- get_iteration_gradients_classifier(fit, x, n_trees)
  if (length(n_trees) > 1) {
    iteration_pred_logodds <- t(apply(mult, 1, cumsum)) + fit$basepred
    return(iteration_pred_logodds[, n_trees, drop = FALSE])
  }
  rowSums(mult) + fit$basepred
}

#' Sense check calculation for model diagnostics
#' 
#' @param object Model object
#' @param train_x Training data
#' @param train_y Training target
#' @param ... Additional arguments
#' @return Vector of differences
#' @keywords internal
sense_check_calc <- function(object, train_x, train_y, ...) {
  pred_func <- get_pred_func(object$guide_pred_type)
  if (object$type == "regression") {
    mult <- get_iteration_gradients_regressor(object$fit, train_x, pred_func)
    pred_matrix <- t(apply(mult, 1, cumsum)) + object$fit$basepred
    mse_vec <- apply(pred_matrix, 2, function(pred){ mse(train_y, pred) })
    return(mse_vec - object$fit$err)
  }
  if (object$type == "binary_classification") {
    mult <- get_iteration_gradients_classifier(object$fit, train_x)
    loglik_matrix <- t(apply(mult, 1, cumsum)) + object$fit$basepred
    apply(loglik_matrix, 2, function(predLogOdds){ loglik2(actual = train_y, log_odds = predLogOdds) }) - object$fit$err
  }
}
