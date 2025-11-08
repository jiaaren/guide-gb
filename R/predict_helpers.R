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
