# fitting regressor model
fit_regression <- function(x, y, guide_path, run_folder, eta, iterations,
                           bagging, bag_fraction, complexity, bag_seed=NULL, val_x = NULL, val_y = NULL,
                           early_stop_rounds = NULL, has_early_stop = FALSE, has_watchlist = FALSE,
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
    run_guide_command(guide_path)
    # result <- system2(guide_path, stdin = "data.in", stdout = TRUE, stderr = TRUE)
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
    }
    if (it %% PRINT_ITERATIONS == 0) {
      if (has_watchlist) {
          print(paste("train mse after iteration", it, ":", new_train_err,
                      "; val mse:", new_val_err))
      } else {
        print(paste("Completed iteration", it, "; train mse:", new_train_err))
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
