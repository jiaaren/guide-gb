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

# returns a matrix of gradients, rows = observations, cols = iterations
get_iteration_gradients_regressor <- function(fit, x, pred_func, n_trees = NULL) {
  # n_trees passed here cannot be a vector, hence max
  n_trees <- max(n_trees %||% fit$iterations)
  # TODO: explore how to use node and fitvar next time
  results <- lapply(fit$trees[1:n_trees], function(f) pred_func(x, f)$pred)
  results <- do.call(cbind, results)
  sweep(results, 2, fit$eta[1:n_trees], `*`)
}

make_regressor_prediction <- function(fit, x, pred_func, n_trees) {
  mult <- get_iteration_gradients_regressor(fit, x, pred_func, n_trees)
  # check if n_trees is a scalar or vector
  if (length(n_trees) > 1) {
    iteration_pred <- t(apply(mult, 1, cumsum)) + fit$basepred
    return(iteration_pred[, n_trees, drop = FALSE])
  }
  rowSums(mult) + fit$basepred
}

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
get_iteration_gradients_classifier <- function(fit, x, n_trees = NULL) {
  # n_trees passed here cannot be a vector, hence max
  n_trees <- max(n_trees %||% fit$iterations) 
  # classifier references node
  results <- lapply(fit$trees[1:n_trees], function(f) make_prediction_tree_a(x, f)$node)
  mapped_results <- mapply(map_logodds, fit$tree_maps, results, SIMPLIFY = TRUE)
  sweep(mapped_results, 2, fit$eta[1:n_trees], `*`)
}

make_classifier_prediction <- function(fit, x, n_trees) {
  mult <- get_iteration_gradients_classifier(fit, x, n_trees)
  if (length(n_trees) > 1) {
    iteration_pred_logodds <- t(apply(mult, 1, cumsum)) + fit$basepred
    return(iteration_pred_logodds[, n_trees, drop = FALSE])
  }
  rowSums(mult) + fit$basepred
}

get_pred_func <- function(guide_pred_type) {
  if (guide_pred_type == "a") {
    make_prediction_tree_a
  } else if (guide_pred_type == "b") {
    make_prediction_tree_b
  } else {
    stop("Unknown guide_pred_type")
  }
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

#' Predict from a custom model
#'
#' @param object A fitted "my_model" object.
#' @param newdata A data frame of new predictor values.
#' @param ... Additional arguments passed to underlying predict functions.
#'
#' @return Predicted values (numeric for regression, class labels for classification).
#' @export
# If type="response" then gbm converts back to the same scale as the outcome. Currently the only
# effect this will have is returning probabilities for bernoulli and expected counts for poisson. For the
# other distributions "response" and "link" return the same.
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
    newdata[[missing_col]] <- is.na(newdata[[col]])
    # not sure which one is it, to inspect R generated code
    # newdata[[missing_col]] <- ifelse(is.na(newdata[[col]]), 1, 0)
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
