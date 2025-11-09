# Handles binary cases
# accepts when y is either 1 or 0, where positive class is assumed to be 1
fit_binary_classifier <- function(x, y, guide_path, run_folder, eta, iterations,
                                  bagging, bag_fraction, bag_seed=NULL, val_x = NULL, val_y = NULL,
                                  early_stop_rounds = NULL, has_early_stop = FALSE, has_watchlist = FALSE,
                                  fit_pred_exact = FALSE) {
  # keep track of current path and change path to run_folder
  row.names(x) <- NULL # reset row names
  curr_path <- getwd()
  setwd(run_folder)

  # initialise supplementary df, used for GUIDE input
  n <- nrow(x)
  supp <- data.frame(resid = rep(0, n))
  if (is.null(bag_seed)) bag_seed <- sample.int(1e6, 1)

  # initialise predictions with log odds
  countPos <- sum(y)
  countNeg <- length(y) - countPos
  init.log.odds <- log(countPos / countNeg)
  log.odds <- rep(init.log.odds, length(y))
  prev_train_err <- loglik2(actual = y, log_odds = log.odds)
  print(paste("train log likelihood:", prev_train_err))

  # initialise return values
  eta_vec <- numeric(iterations)
  err_vec <- numeric(iterations)
  trees <- vector("list", iterations)
  tree_maps <- vector("list", iterations)

  # initialise with default predictions for validation set
  if (has_watchlist) {
    log.odds_val <- rep(init.log.odds, nrow(val_x))
    err_val_vec <- numeric(iterations)
  }
  # pre-set bagging indices if bagging is enabled
  if (bagging) {
    set.seed(bag_seed)
    bag_indices <- lapply(1:iterations, function(it) {
      sample.int(n, size = floor(n * bag_fraction), replace = FALSE)
    })
  }

  # iterate gradient boosting
  for (it in 1:iterations) {
    # compute residuals
    y_pred <- 1 / (1 + exp(-log.odds))
    supp$resid <- y - y_pred
    # if bagging, create istrain indicator
    if (bagging) {
      set.seed(bag_seed + it) # ensure different seed per iteration
      bag_train_idx <- sample.int(n, size = floor(n * bag_fraction), replace = FALSE)
      write.csv(cbind(x, supp)[bag_train_idx, ], file.path(run_folder, "data.csv"), row.names = FALSE)
    } else {
      write.csv(cbind(x, supp), file.path(run_folder, "data.csv"), row.names = FALSE)
    }
    run_guide_command(guide_path)
    code <- trim_file_at_marker("data.R")
    # code predicted will be parsed
    eval(parse(text = code))
    trees[[it]] <- predicted
    # read fitted predictions (should be on residuals)
    if (fit_pred_exact) {
      fitted <- make_prediction_tree_a(x, predicted)
      fitted$train <- "n"
      if (!bagging) fitted$train <- "y"
      else fitted[bag_train_idx, "train"] <- "y"
    } else {
      fitted <- read.table("data.fitted", header = TRUE)
    }
    # update tree map
    tmp_tree_map <- list()
    for (node in unique(fitted$node)) {
      # NOTE: Highly important to only use training data to compute predLogOdds
      bool_node_train <- fitted$train == "y" & fitted$node == node
      # resid would have the same values as fitted_observed
      numerator <- sum(supp$resid[bool_node_train])
      denominator <- sum(y_pred[bool_node_train] * (1 - y_pred[bool_node_train]))
      predLogOdds <- ifelse(denominator == 0, 0, numerator / denominator)
      fitted[fitted$node == node, "predLogOdds"] <- predLogOdds
      tmp_tree_map[[as.character(node)]] <- predLogOdds
    }
    tree_maps[[it]] <- tmp_tree_map
    # update predictions
    log.odds <- log.odds + fitted$predLogOdds * eta
    eta_vec[it] <- eta
    new_train_err <- loglik2(actual = y, log_odds = log.odds)
    err_vec[it] <- new_train_err
    # compute validation error if watchlist is provided
    if (has_watchlist) {
      # compute validation error
      val_fitted <- make_prediction_tree_a(val_x, predicted)
      val_fitted$predLogOdds <- sapply(val_fitted$node, function(n) tmp_tree_map[[as.character(n)]])
      log.odds_val <- log.odds_val + val_fitted$predLogOdds * eta
      new_val_err <- loglik2(actual = val_y, log_odds = log.odds_val)
      err_val_vec[it] <- new_val_err
    }
    if (it %% PRINT_ITERATIONS == 0) {
      if (has_watchlist) {
        print(paste("Completed iteration", it, "; train loglik:", new_train_err,
                    "; val loglik:", new_val_err))
      } else {
        print(paste("Completed iteration", it, "; train loglik:", new_train_err))
      }
    }
    # early stopping check
    if (has_early_stop) {
      if (it == 1 || new_val_err < best_val_err) {
        best_val_err <- new_val_err
        best_iter <- it
        no_improve_count <- 0
      } else
        no_improve_count <- no_improve_count + 1
      if (no_improve_count >= early_stop_rounds) break
    }
  }
  # trim trees, eta_vec, err_vec, tree_maps to actual iterations
  if (has_early_stop && best_iter < it) {
    trees <- trees[1:best_iter]
    eta_vec <- eta_vec[1:best_iter]
    err_vec <- err_vec[1:best_iter]
    err_val_vec <- err_val_vec[1:best_iter]
    tree_maps <- tree_maps[1:best_iter]
  }
  # reset path to current path after fitting
  setwd(curr_path)
  return(list(
    basepred = init.log.odds,
    trees = trees,
    tree_maps = tree_maps,
    iterations = if (has_early_stop) best_iter else it,
    eta = eta_vec,
    err = err_vec,
    err_val = if (has_watchlist) err_val_vec else NULL
  ))
}
