#' Fit regression model using GUIDE
#' 
#' @param x Training data frame
#' @param y Target vector
#' @param guide_path Path to GUIDE executable
#' @param run_folder Folder for GUIDE files
#' @param eta Learning rate
#' @param iterations Number of boosting iterations
#' @param bagging Whether bagging is enabled
#' @param bag_fraction Fraction of data for bagging
#' @param complexity Model complexity
#' @param bag_seed Random seed for bagging
#' @param val_x Validation data frame
#' @param val_y Validation target vector
#' @param early_stop_rounds Early stopping rounds
#' @param has_early_stop Whether early stopping is enabled
#' @param has_watchlist Whether validation set is provided
#' @param fit_pred_exact Whether to fit predictions exactly
#' @param guide_pred_type GUIDE prediction type
#' @param missing_num_vars Variables with missing values
#' @return List with fitted model components
#' @keywords internal
fit_regression <- function(x, y, guide_path, run_folder, eta, iterations,
                          bagging, bag_fraction, complexity, bag_seed = NULL, 
                          val_x = NULL, val_y = NULL,
                          early_stop_rounds = NULL, has_early_stop = FALSE, 
                          has_watchlist = FALSE,
                          fit_pred_exact = FALSE, guide_pred_type, missing_num_vars) {
  # keep track of current path and change path to run_folder
  row.names(x) <- NULL # reset row names
  curr_path <- getwd()
  setwd(run_folder)
  
  # initialise supplementary df, used for GUIDE input
  n <- nrow(x)
  n_cols <- ncol(x)
  supp <- data.frame(resid = rep(0, n))
  if (is.null(bag_seed)) bag_seed <- sample.int(1e6, 1)
  
  # initialise predictions with mean of y
  pred_y <- mean(y)
  y_pred <- rep(pred_y, length(y))
  prev_train_err <- mse(y, y_pred)
  print(paste("train mse:", prev_train_err))
  
  # pre-allocate return values
  eta_vec <- numeric(iterations)
  err_vec <- numeric(iterations)
  trees <- vector("list", iterations)
  
  # get pred_func based on root prediction
  pred_func <- get_pred_func(guide_pred_type)
  # add numerical columns with missingness indicators
  if (complexity == "stepwise" && fit_pred_exact) {
    for (col in missing_num_vars) {
      x[[paste0(col, ".NA")]] <- ifelse(is.na(x[[col]]), 1, 0)
      if (has_watchlist)
        val_x[[paste0(col, ".NA")]] <- ifelse(is.na(val_x[[col]]), 1, 0)
    }
  }
  # initialise with default predictions for validation set
  if (has_watchlist) {
    y_pred_val <- rep(pred_y, nrow(val_x))
    err_val_vec <- numeric(iterations)
  }
  # pre-set bagging indices if bagging is enabled
  if (bagging) {
    set.seed(bag_seed)
    bag_indices <- lapply(1:iterations, function(it) {
      sample.int(n, size = floor(n * bag_fraction), replace = FALSE)
    })
  }
  
  # cache column subset for x
  x_subset <- x[, 1:n_cols]
  
  # iterate gradient boosting
  for (it in 1:iterations) {
    # compute residuals
    supp$resid <- y - y_pred
    # if bagging, create istrain indicator
    if (bagging) {
      bag_train_idx <- bag_indices[[it]]
      write.csv(cbind(x_subset, supp)[bag_train_idx, ], file.path(run_folder, "data.csv"), row.names = FALSE)
    } else {
      write.csv(cbind(x_subset, supp), file.path(run_folder, "data.csv"), row.names = FALSE)
    }
    # fit guide tree (external call)
    exec_out <- system(paste(guide_path, "< data.in"), intern = TRUE)
    code <- trim_file_at_marker("data.R")
    # code predicted will be parsed
    eval(parse(text = code))
    trees[[it]] <- predicted
    if (fit_pred_exact) {
      # read fitted predictions (should be on residuals)
      fitted <- pred_func(x, predicted)
      y_pred <- y_pred + eta * fitted$pred
    } else {
      # read fitted from file vs parsed code, and update predictions
      fitted <- read.table("data.fitted", header = TRUE)
      y_pred <- y_pred + eta * fitted$predicted
    }
    # compute training MSE against true target
    eta_vec[it] <- eta
    new_train_err <- mse(y, y_pred)
    err_vec[it] <- new_train_err
    # compute validation error if watchlist is provided
    if (has_watchlist) {
      # compute validation error
      val_fitted <- pred_func(val_x, predicted)
      y_pred_val <- y_pred_val + eta * val_fitted$pred
      new_val_err <- mse(val_y, y_pred_val)
      err_val_vec[it] <- new_val_err
      print(paste("train mse after iteration", it, ":", new_train_err,
                  "; val mse:", new_val_err))
    } else {
      print(paste("train mse after iteration", it, ":", new_train_err))
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
  # trim trees, eta_vec, err_vec to actual iterations
  if (has_early_stop && it < iterations) {
    trees <- trees[1:best_iter]
    eta_vec <- eta_vec[1:best_iter]
    err_vec <- err_vec[1:best_iter]
    err_val_vec <- err_val_vec[1:best_iter]
  }
  # reset path to current path after fitting
  setwd(curr_path)
  return(list(
    basepred = pred_y,
    trees = trees,
    iterations = if (has_early_stop) best_iter else it,
    eta = eta_vec,
    err = err_vec,
    err_val = if (has_watchlist) err_val_vec else NULL
  ))
}

#' Fit binary classifier using GUIDE
#' 
#' Handles binary cases where y is either 1 or 0, with positive class = 1
#' 
#' @param x Training data frame
#' @param y Target vector (0 or 1)
#' @param guide_path Path to GUIDE executable
#' @param run_folder Folder for GUIDE files
#' @param eta Learning rate
#' @param iterations Number of boosting iterations
#' @param bagging Whether bagging is enabled
#' @param bag_fraction Fraction of data for bagging
#' @param bag_seed Random seed for bagging
#' @param val_x Validation data frame
#' @param val_y Validation target vector
#' @param early_stop_rounds Early stopping rounds
#' @param has_early_stop Whether early stopping is enabled
#' @param has_watchlist Whether validation set is provided
#' @param fit_pred_exact Whether to fit predictions exactly
#' @return List with fitted model components
#' @keywords internal
fit_binary_classifier <- function(x, y, guide_path, run_folder, eta, iterations,
                                 bagging, bag_fraction, bag_seed = NULL, 
                                 val_x = NULL, val_y = NULL,
                                 early_stop_rounds = NULL, has_early_stop = FALSE, 
                                 has_watchlist = FALSE,
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
    # fit guide tree (external call)
    exec_out <- system(paste(guide_path, "< data.in"), intern = TRUE)
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
      print(paste("train loglik after iteration", it, ":", new_train_err,
                  "; val loglik:", new_val_err))
    } else {
      print(paste("train loglik after iteration", it, ":", new_train_err))
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
  if (has_early_stop && it < iterations) {
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
