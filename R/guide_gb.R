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
    fitted <- read.table('data.fitted', header = TRUE)
    code <- trim_file_at_marker('data.R')
    eval(parse(text = code))
    trees[[it]] <- predicted
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

guide_gb <- function(x, y, guide_path, run_folder,
                    type = c("regression", "classification"),
                    eta=0.1,
                    iterations=100,
                    epsilon=1e-5) {
  type <- match.arg(type)

  if (type == "regression") {
    fit <- fit_regression(x,y,guide_path,run_folder,eta,iterations,epsilon)
  }

  if (type == "classification") {
    fit <- glm(y ~ ., data = data.frame(y = y, x), family = binomial)
  }

  # Add attributes
  structure(
    list(
      fit = fit,
      guide_path=guide_path,
      run_folder=run_folder,
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

