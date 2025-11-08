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
