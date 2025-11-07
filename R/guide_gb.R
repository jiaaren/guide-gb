#' Fit a custom regression or classification model
#'
#' @param x A data frame of predictors.
#' @param y A vector of outcomes.
#' @param type Either "regression" or "classification".
#'
#' @return An object of class "guide_gb".
#' @export
#'
#' @examples
#' x <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
#' y <- 2 + 3*x$x1 - x$x2 + rnorm(50)
#' model <- guide_gb(x, y, type = "regression")
#' predict(model, newdata = x)
# source('/Users/jkhong/Desktop/guide-gb/R/predict.guide_gb.R')

# metrics
mse <- function(actual, pred) {
  mean((actual - pred)^2)
}

rmse <- function(actual, pred) {
  sqrt(mse(actual, pred))
}

loglik <- function(actual, pred) {
  -2 * mean(actual * log(pred) + (1 - actual) * log(1 - pred))
}

loglik2 <- function(actual, log_odds) {
  -2 * mean(actual * log_odds - log(1 + exp(log_odds)))
}

# utils
trim_file_at_marker <- function(file, marker = "## end of function") {
  # read file lines
  lines <- readLines(file)
  idx <- grep(marker, lines, fixed = TRUE)
  if (length(idx) > 0) {
    lines <- lines[seq_len(idx[1])]
  }
  return(lines)
}

# fitting regressor model
fit_regression <- function(x, y, guide_path, run_folder, eta, iterations, epsilon,
                           bagging, bag_fraction, bag_seed=NULL, val_x = NULL, val_y = NULL,
                           early_stop_rounds = NULL, has_early_stop = FALSE, has_watchlist = FALSE,
                           fit_pred_exact = FALSE, guide_pred_type) {
  # keep track of current path and change path to run_folder
  row.names(x) <- NULL # reset row names
  curr_path <- getwd()
  setwd(run_folder)

  # initialise supplementary df, used for GUIDE input
  n <- nrow(x)
  supp <- data.frame(resid = rep(0, n))
  if (is.null(bag_seed)) bag_seed <- sample.int(1e6, 1)

  # initialise predictions with mean of y
  pred_y <- mean(y)
  y_pred <- rep(pred_y, length(y))
  prev_train_err <- mse(y, y_pred)
  print(paste("train mse:", prev_train_err))

  # initialise return values
  eta_vec <- c()
  err_vec <- c()
  trees <- list()

  # get pred_func based on root prediction
  pred_func <- get_pred_func(guide_pred_type)

  # iterate gradient boosting
  for (it in 1:iterations) {
    # compute residuals
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
    if (fit_pred_exact) {
      # read fitted predictions (should be on residuals)
      fitted <- pred_func(x, predicted)
      y_pred <- y_pred + eta * fitted$pred
    } else {
      # read fitted from file vs parsed code, and update predictions
      fitted <- read.table("data.fitted", header = TRUE)
      y_pred <- y_pred + eta * fitted$predicted
    }
    eta_vec[it] <- eta
    # compute training MSE against true target
    new_train_err <- mse(y, y_pred)
    err_vec[it] <- new_train_err
    print(paste('train mse after iteration', it, ':', new_train_err)) 
    # stopping criterion
    if (abs(new_train_err - prev_train_err) < epsilon) break
    prev_train_err <- new_train_err
  }
  # reset path to current path after fitting
  setwd(curr_path)
  return(list(
    basepred = pred_y,
    trees = trees,
    iterations = it,
    eta = eta_vec,
    err = err_vec
  ))
}

# Handles binary cases
# accepts when y is either 1 or 0, where positive class is assumed to be 1
fit_binary_classifier <- function(x, y, guide_path, run_folder, eta, iterations, epsilon,
                                  bagging, bag_fraction, bag_seed=NULL, fit_pred_exact = FALSE,
                                  val_x = NULL, val_y = NULL, early_stop_rounds = NULL,
                                  has_early_stop = FALSE, has_watchlist = FALSE) {
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
  eta_vec <- c()
  err_vec <- c()
  trees <- list()
  tree_maps <- list()
  
  # iterate gradient boosting
  for (it in 1:iterations) {
    # compute residuals
    y_pred <- 1/(1+exp(-log.odds))
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
    if (fit_pred_exact) fitted <- read.table("data.fitted", header = TRUE)
    else {
      fitted <- make_prediction_tree_a(x, predicted)
      fitted$train <- "n"
      if (!bagging) fitted$train <- "y"
      else fitted$train[bag_train_idx] <- "y"
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
    # compute new train error of pred against true target
    new_train_err <- loglik2(actual = y, log_odds = log.odds)
    err_vec[it] <- new_train_err
    print(paste("train loglik after iteration", it, ":", new_train_err)) 
    # stopping criterion
    if (abs(new_train_err - prev_train_err) < epsilon) break
    prev_train_err <- new_train_err
  }
  # reset path to current path after fitting
  setwd(curr_path)
  return(list(
    basepred = init.log.odds,
    trees = trees,
    tree_maps = tree_maps,
    iterations = it,
    eta = eta_vec,
    err = err_vec
  ))
}

dsc_clean <- function(dsc_lines) {
  dsc_lines <- trimws(dsc_lines)
  dsc_lines[dsc_lines != ""]
}

dsc_add_weight <- function(dsc_lines, weight_var = "istrain") {
  # get idx of last variable
  last_line <- dsc_lines[length(dsc_lines)]
  last_var_idx <- as.integer(unlist(strsplit(last_line, "\\s+"))[1])
  dsc_lines <- c(dsc_lines, paste(last_var_idx + 1, weight_var, "w"))
  dsc_lines
}

dsc_get_variables <- function(dsc_lines) {
  variables <- list()
  for (i in seq_along(dsc_lines)) {
    # first 3 rows relate to missing value handling and rows to skip
    if (i <= 3) next
    # split by spaces and update list
    parts <- unlist(strsplit(dsc_lines[i], "\\s+"))
    var_name <- parts[2]
    var_type <- parts[3]
    variables[[var_name]] <- var_type
  }
  variables[["missing_indicator"]] <- dsc_lines[2]
  variables
}

count_missing_values <- function(x, dsc_vars) {
  missing_vars <- c()
  for (col in colnames(x)) {
    if (any(is.na(x[[col]])) && dsc_vars[[col]] == "n") {
      missing_vars <- c(missing_vars, col)
    }
  }
  missing_vars
}

type_map <- c(
  constant_exhaustive = "a",
  constant_quantiles  = "a",
  poly1               = "b",
  poly2               = "b",
  stepwise            = "a"
)

guide_gb <- function(x, y, guide_path, config_path, run_folder=NULL,
                     type = c("regression", "binary_classification"),
                     complexity = c("constant_exhaustive", "constant_quantiles",
                                    "poly1", "poly2", "stepwise"),
                     eta = 0.1,
                     max_split_levels = 4,
                     min_node_size = 4,
                     iterations = 100,
                     bag_fraction = 1.0,
                     bag_seed = NULL,
                     epsilon = 1e-5,
                     val_x = NULL, val_y = NULL,
                     early_stop_rounds = NULL,
                     fit_pred_exact = FALSE) {
  type <- match.arg(type)
  complexity <- match.arg(complexity)
  # this is only required if fit_pred_exact is TRUE
  guide_pred_type <- type_map[[complexity]]
  # validate that bag_fraction is more than 0 and less than or equal to 1
  if (bag_fraction <= 0 || bag_fraction > 1) {
    stop("bag_fraction must be in the range (0, 1].")
  }
  bagging <- ifelse(bag_fraction < 1.0, TRUE, FALSE)
  # if bagging is FALSE and bag_seed is provided, warn user
  if (!bagging && !is.null(bag_seed)) {
    warning("bag_seed is provided but bagging is disabled (bag_fraction = 1.0). The bag_seed will be ignored.")
  }
  # if type is classification, validate that y contains only 0 and 1
  if (type == "binary_classification") {
    if (!all(y %in% c(0, 1))) {
      stop("For binary_classification, y must contain only 0 and 1 values.")
    }
  }
  # check if validation set is provided, raise error if only one of val_x and val_y is provided
  has_watchlist <- !is.null(val_x) && !is.null(val_y)
  has_early_stop <- !is.null(early_stop_rounds)
  if (xor(is.null(val_x), is.null(val_y))) {
    stop("Both val_x and val_y must be provided for validation set.")
  }
  # TODO: next time implement auto splitting of training and validation set
  # if early_stop_round is provided, validate that watchlist is provided
  if (!is.null(early_stop_rounds) && !has_watchlist) {
    stop("Early_stop_rounds is provided, val_x and val_y must be provided.")
  }
  # Validate validation set structure if provided
  if (has_watchlist) {
    # Check same number of columns
    if (ncol(val_x) != ncol(x)) {
      stop("val_x must have the same number of columns as x.")
    }
    # Check same column names
    if (!all(colnames(val_x) == colnames(x))) {
      stop("val_x must have the same column names as x in the same order.")
    }
    # Check val_y length matches val_x rows
    if (length(val_y) != nrow(val_x)) {
      stop("Length of val_y must match number of rows in val_x.")
    }
    # For binary classification, check val_y contains only 0 and 1
    if (type == "binary_classification" && !all(val_y %in% c(0, 1))) {
      stop("For binary_classification, val_y must contain only 0 and 1 values.")
    }
    if (nrow(val_x) == 0) {
      stop("Validation set (val_x) cannot be empty.")
    }
  }
  # Validate early_stop_rounds value
  if (has_early_stop) {
    if (!is.numeric(early_stop_rounds) || length(early_stop_rounds) != 1) {
      stop("early_stop_rounds must be a single numeric value.")
    }
    if (early_stop_rounds < 1) {
      stop("early_stop_rounds must be at least 1.")
    }
    if (early_stop_rounds >= iterations) {
      warning("early_stop_rounds is >= iterations.")
    }
  }

  # validate that guide_path exists
  if (!file.exists(guide_path)) {
    stop("guide_path does not exist.")
  }
  # validate that run_folder exists, if not, create a temporary folder
  if (is.null(run_folder)) {
    run_folder <- tempdir()
  }
  if (!dir.exists(run_folder)) {
    stop("run_folder does not exist.")
  }
  # only read .in file contained within the config_path
  in_file <- paste0("data_", complexity, ".in")
  in_file_path <- file.path(config_path, in_file)
  if (!file.exists(in_file_path)) {
    stop(paste("Configuration .in file does not exist at", in_file_path))
  }
  in_file_lines <- readLines(in_file_path)
  # replace max split levels and min node size with user input
  in_file_lines[grep("\\(max. no. split levels\\)", in_file_lines)] <-
    paste0("        ", max_split_levels, "   (max. no. split levels)")
  in_file_lines[grep("\\(min. node sample size\\)", in_file_lines)] <-
    paste0("        ", min_node_size, "   (min. node sample size)")
  # write modified .in file to run_folder and save in attributes
  writeLines(in_file_lines, con = file.path(run_folder, "data.in"))
  in_file_concat <- paste(in_file_lines, collapse = "\n")

  # copy DSC file in config_path to run_folder
  dsc_file_src <- file.path(config_path, "data.DSC")
  if (!file.exists(dsc_file_src)) {
    stop(paste("DSC file does not exist at", dsc_file_src))
  }
  dsc_lines <- dsc_clean(readLines(dsc_file_src))
  # if (bagging || has_watchlist) dsc_lines <- dsc_add_weight(dsc_lines)
  # write modified DSC file to run_folder
  writeLines(dsc_lines, con = file.path(run_folder, "data.DSC"))
  dsc_vars <- dsc_get_variables(dsc_lines)

  # keep track of missing values for subsequent processing for predictions
  missing_num_vars <- count_missing_values(x, dsc_vars)

  # if fit_pred_exact is TRUE, then need to pass in guide_pred_type,
  ## only relevant for regression as classifier calculates logodds using nodes
  if (type == "regression") {
    fit <- fit_regression(x = x, y = y,
                          guide_path = guide_path,
                          run_folder = run_folder,
                          eta = eta,
                          iterations = iterations,
                          epsilon = epsilon,
                          bagging = bagging,
                          bag_fraction = bag_fraction,
                          bag_seed = bag_seed,
                          val_x = val_x,
                          val_y = val_y,
                          early_stop_rounds = early_stop_rounds,
                          has_early_stop = has_early_stop,
                          has_watchlist = has_watchlist,
                          fit_pred_exact = fit_pred_exact,
                          guide_pred_type = guide_pred_type)
  }
  if (type == "binary_classification") {
    fit <- fit_binary_classifier(x = x, y = y,
                                 guide_path = guide_path,
                                 run_folder = run_folder,
                                 eta = eta,
                                 iterations = iterations,
                                 epsilon = epsilon,
                                 bagging = bagging,
                                 bag_fraction = bag_fraction,
                                 bag_seed = bag_seed,
                                 val_x = val_x,
                                 val_y = val_y,
                                 early_stop_rounds = early_stop_rounds,
                                 has_early_stop = has_early_stop,
                                 has_watchlist = has_watchlist)
  }
  # Add attributes
  structure(
    list(
      fit = fit,
      guide_path = guide_path,
      config_path = config_path,
      in_file = in_file_concat,
      dsc_vars = dsc_vars,
      missing_num_vars = missing_num_vars,
      guide_pred_type = guide_pred_type,
      eta = eta,
      iterations = iterations,
      epsilon = epsilon,
      type = type,
      call = match.call(),             # store the call
      nobs = nrow(x),                  # number of observations
      predictors = colnames(x)         # predictor names
    ),
    class = "guide_gb"
  )
}
