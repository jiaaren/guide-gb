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
