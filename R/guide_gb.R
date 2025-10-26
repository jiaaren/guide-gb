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
mse <- function(resid) {
  mean(resid^2)
}
rmse <- function(resid) {
  sqrt(mean(resid^2))
}

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
fit_regression <- function(x, y, guide_path, run_folder, eta, iterations, epsilon) {
  # keep track of current path and change path to run_folder
  curr_path <- getwd()
  setwd(run_folder)
  
  # initialise predictions with mean of y
  pred_y <- mean(y)
  y_pred <- rep(pred_y, length(y))

  # initialise return values
  eta_vec <- c()
  err_vec <- c()
  trees <- list()

  # get pred_func based on root prediction
  # pred_func <- make_prediction_tree_a

  prev_train_err <- rmse(y - y_pred)
  print(paste('train rmse:', prev_train_err))
  
  # iterate gradient boosting
  for (it in 1:iterations) {
    it_id <- paste('it_', it, sep = '')
    # compute residuals
    resid <- y - y_pred
    # write training data (features + residuals)
    write.csv(data.frame(x, resid = resid), 
              file.path(run_folder, 'data.csv'), 
              row.names = FALSE)
    # fit guide tree (external call)
    exec_out <- system(paste(guide_path, '< data.in'), intern = TRUE)
    # read fitted predictions (should be on residuals)
    code <- trim_file_at_marker('data.R')
    # code predicted will be parsed
    eval(parse(text = code))
    trees[[it]] <- predicted
    # read fitted from file vs parsed code
    fitted <- read.table('data.fitted', header = TRUE)
    # fitted <- pred_func(x, predicted) # this would return 'pred' instead of 'predicted'
    # update predictions
    y_pred <- y_pred + eta * fitted$predicted
    eta_vec[it] <- eta
    # compute training RMSE against true target
    new_train_err <- rmse(y - y_pred)
    err_vec[it] <- new_train_err
    print(paste('train rmse after iteration', it, ':', new_train_err)) 
    # stopping criterion
    if (abs(new_train_err - prev_train_err) < epsilon) break
    prev_train_err <- new_train_err
  }
  # reset path to current path after fitting
  setwd(curr_path)
  return(list(
    basepred=pred_y,
    trees=trees,
    iterations=it,
    eta=eta_vec,
    err=err_vec
  ))
}

loglik <- function(actual, pred) {
  sum(actual * log(pred) + (1 - actual) * log(1 - pred))
}

# Handles binary cases
# accepts when y is either 1 or 0, where positive class is assumed to be 1
fit_binary_classifier <- function(x, y, guide_path, run_folder, eta, iterations, epsilon) {
  # keep track of current path and change path to run_folder
  curr_path <- getwd()
  setwd(run_folder)
  
  # initialise predictions with log odds
  countPos <- sum(y)
  countNeg <- length(y) - countPos
  init.log.odds <- log(countPos / countNeg)
  log.odds <- init.log.odds
  y_pred <- rep(1/(1+exp(-log.odds)), length(y))

  # initialise return values
  eta_vec <- c()
  err_vec <- c()
  trees <- list()
  tree_maps <- list()

  prev_train_err <- loglik(y, y_pred)
  print(paste('train log likelihood:', prev_train_err))
  
  # iterate gradient boosting
  for (it in 1:iterations) {
    it_id <- paste('it_', it, sep = '')
    # compute residuals
    resid <- y - y_pred
    # write training data (features + residuals)
    write.csv(data.frame(x, resid = resid), 
              file.path(run_folder, 'data.csv'), 
              row.names = FALSE)
    # fit guide tree (external call)
    exec_out <- system(paste(guide_path, '< data.in'), intern = TRUE)
    # read fitted predictions (should be on residuals)
    fitted <- read.table('data.fitted', header = TRUE)
    code <- trim_file_at_marker('data.R')
    eval(parse(text = code))
    trees[[it]] <- predicted
    # calculate gradients using residuals
    tmp_tree_map <- list()
    for (node in unique(fitted$node)) {
      bool_node_train <- fitted$train == 'y' & fitted$node == node
      # this should be observed instead of predicted
      train.new.preds <- fitted$observed[bool_node_train]
      # resid would have the same values as fitted_observed
      numerator <- sum(resid[bool_node_train])
      denominator <- sum(y_pred[bool_node_train] * (1 - y_pred[bool_node_train]))
      predLogOdds <- numerator / denominator
      fitted[fitted$node == node, 'predLogOdds'] <- predLogOdds
      tmp_tree_map[[as.character(node)]] <- predLogOdds
    }
    tree_maps[[it]] <- tmp_tree_map
    # update predictions
    log.odds <- log.odds + fitted$predLogOdds * eta
    y_pred <- 1/(1+exp(-log.odds))
    eta_vec[it] <- eta
    # compute new train error if pred against true target
    new_train_err <- loglik(y, y_pred)
    err_vec[it] <- new_train_err
    print(paste('train loglik after iteration', it, ':', new_train_err)) 
    # stopping criterion
    if (abs(new_train_err - prev_train_err) < epsilon) break
    prev_train_err <- new_train_err
  }
  # reset path to current path after fitting
  setwd(curr_path)
  return(list(
    basepred=init.log.odds,
    trees=trees,
    tree_maps=tree_maps,
    iterations=it,
    eta=eta_vec,
    err=err_vec
  ))
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
                    complexity=c("constant_exhaustive", "constant_quantiles",
                                "poly1", "poly2", "stepwise"),
                    eta=0.1,
                    max_split_levels=4,
                    min_node_size=4,
                    iterations=100,
                    epsilon=1e-5) {
  type <- match.arg(type)
  complexity <- match.arg(complexity)
  guide_pred_type <- type_map[[complexity]]

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
  file.copy(dsc_file_src, file.path(run_folder, "data.DSC"), overwrite = TRUE)

  if (type == "regression") {
    fit <- fit_regression(x,y,guide_path,run_folder,eta,iterations,epsilon)
  }
  if (type == "binary_classification") {
    fit <- fit_binary_classifier(x,y,guide_path,run_folder,eta,iterations,epsilon)
  }

  # Add attributes
  structure(
    list(
      fit = fit,
      guide_path=guide_path,
      config_path=config_path,
      in_file=in_file_concat,
      guide_pred_type=guide_pred_type,
      eta=eta,
      iterations=iterations,
      epsilon=epsilon,
      type = type,
      call = match.call(),             # store the call
      nobs = nrow(x),                  # number of observations
      predictors = colnames(x)         # predictor names
    ),
    class = "guide_gb"
  )
}

