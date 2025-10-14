library(gbm)
library(xgboost)
library(catboost)
setwd("/Users/jkhong/Desktop/guide-gb/")
# Load functions for GUIDE-GB
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

data <- read.csv("/Users/jkhong/Desktop/guide-gb/data/reg_boston/boston.csv", header = TRUE)
x <- data[,1:13]
# convert all columns to numeric
for (col in colnames(x)) {
  x[[col]] <- as.numeric(x[[col]])
}
y <- as.numeric(data$y)
folds <- data$fold
# sense chcek over count of folds
table(folds)

# models' parameters
DT <- 0.5 # decision threshold
SEED <- 456
eta <- 0.05
iterations <- 5000
max_depth <- 4
min_child_weight <- 2

# define train test splits
k <- 1
train_x <- x[folds != k, ]
train_y <- y[folds != k]
test_x  <- x[folds == k, ]
test_y  <- y[folds == k]

# train and predict using guide-gb
guide_path <- '/Users/jkhong/Desktop/guide-gb/guide'
run_folder <- '/Users/jkhong/Desktop/guide-gb/guide_run'

# ...existing code...
setwd("/Users/jkhong/Desktop/guide-gb/tests/2_regression/boston2")
conn <- file("result_regression.csv", open="wt")
writeLines("model,fold,RMSE_test,RMSE_train,MAE_,R2,nrow_test,nrow_train", conn)
update_matrix_regression <- function(model_name, fold, rmse_test, rmse_train, mae_, r2, nrow_test, nrow_train, conn=conn) {
  writeLines(paste(model_name, fold, sprintf("%.6f", rmse_test), sprintf("%.6f", rmse_train), sprintf("%.6f", mae_), sprintf("%.6f", r2), sprintf("%.6f", nrow_test), sprintf("%.6f", nrow_train), sep=","), conn)
}

for (k in sort(unique(folds))) {
  # train / test split for fold k
  train_x <- x[folds != k, ]
  train_y <- y[folds != k]
  test_x  <- x[folds == k, ]
  test_y  <- y[folds == k]

  # GUIDE-GB (regression)
  set.seed(SEED)
  model <- guide_gb(train_x, train_y,
                    guide_path, run_folder, eta=eta, iterations=iterations,
                    type = "regression")
  pred_gbguide <- predict(model, test_x)
  rmse_ <- sqrt(mean((pred_gbguide - test_y)^2))
  mae_  <- mean(abs(pred_gbguide - test_y))
  r2   <- 1 - sum((pred_gbguide - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_gbguide <- predict(model, train_x)
  train_rmse_ <- sqrt(mean((train_pred_gbguide - train_y)^2))
  update_matrix_regression("guide-gb", k, rmse_, train_rmse_, mae_, r2, nrow(test_x), nrow(train_x), conn)

 # gbm (regression) with early stopping via internal validation (train.fraction)
  set.seed(SEED)
  val_fraction <- 0.2
  gbmmodel <- gbm(
    formula = y ~ .,
    data = data.frame(train_x, y=train_y),
    distribution = "gaussian",
    n.trees = iterations,
    interaction.depth = max_depth,
    shrinkage = eta,
    n.minobsinnode = min_child_weight,
    train.fraction = 1 - val_fraction
  )
  best_iter_gbm <- gbm.perf(gbmmodel, method = "test", plot.it = FALSE)
  pred_gbm <- predict(gbmmodel, test_x, n.trees = best_iter_gbm)
  rmse_ <- sqrt(mean((pred_gbm - test_y)^2))
  mae_  <- mean(abs(pred_gbm - test_y))
  r2   <- 1 - sum((pred_gbm - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_gbm <- predict(gbmmodel, train_x, n.trees = best_iter_gbm)
  train_rmse_ <- sqrt(mean((train_pred_gbm - train_y)^2))
  update_matrix_regression("gbm", k, rmse_, train_rmse_, mae_, r2, nrow(test_x), nrow(train_x), conn)

  # xgboost (regression) with early stopping
  options(na.action = "na.pass")
  train_x2 <- model.matrix(~ . - 1, data = train_x, na.action=na.pass)
  test_x2  <- model.matrix(~ . - 1, data = test_x, na.action=na.pass)
  options(na.action = "na.omit")

  dtrain <- xgb.DMatrix(data = as.matrix(train_x2), label = train_y)
  dtest  <- xgb.DMatrix(data = as.matrix(test_x2),  label = test_y)
  params <- list(objective = "reg:squarederror", eta = eta, max_depth = max_depth,
                 min_child_weight = min_child_weight, eval_metric = "rmse")
  set.seed(SEED)
  watchlist <- list(train = dtrain, eval = dtest)
  early_stop_rounds <- 20
  xgbmodel <- xgb.train(params = params, data = dtrain, nrounds = iterations,
                        watchlist = watchlist, early_stopping_rounds = early_stop_rounds,
                        verbose = 0)
  best_iter_xgb <- if (!is.null(xgbmodel$best_iteration)) xgbmodel$best_iteration else iterations
  pred_xgb <- predict(xgbmodel, dtest, ntreelimit = best_iter_xgb)
  rmse_ <- sqrt(mean((pred_xgb - test_y)^2))
  mae_  <- mean(abs(pred_xgb - test_y))
  r2   <- 1 - sum((pred_xgb - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_xgb <- predict(xgbmodel, dtrain, ntreelimit = best_iter_xgb)
  train_rmse_ <- sqrt(mean((train_pred_xgb - train_y)^2))
  update_matrix_regression("xgboost", k, rmse_, train_rmse_, mae_, r2, nrow(test_x), nrow(train_x), conn)

  # catboost (regression) with early stopping (overfitting detector) and validation pool
  train_pool <- catboost.load_pool(data = train_x, label = train_y)
  test_pool  <- catboost.load_pool(data = test_x,  label = test_y)
  catmodel <- catboost.train(train_pool, test_pool,
                             params = list(loss_function = 'RMSE',
                                           iterations = iterations,
                                           learning_rate = eta,
                                           depth = max_depth,
                                           random_seed = SEED,
                                           od_type = 'Iter',       # enable overfitting detector
                                           od_wait = 20,           # patience
                                           use_best_model = TRUE)) # return best model on validation
  pred_cat <- catboost.predict(catmodel, test_pool, prediction_type = 'RawFormulaVal')
  rmse_ <- sqrt(mean((pred_cat - test_y)^2))
  mae_  <- mean(abs(pred_cat - test_y))
  r2   <- 1 - sum((pred_cat - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_cat <- catboost.predict(catmodel, train_pool, prediction_type = 'RawFormulaVal')
  train_rmse_ <- sqrt(mean((train_pred_cat - train_y)^2))
  update_matrix_regression("catboost", k, rmse_, train_rmse_, mae_, r2, nrow(test_x), nrow(train_x), conn)

  # save models
  saveRDS(model, file = paste0("model_guide_gb_fold_", k, ".rds"))
  saveRDS(gbmmodel, file = paste0("model_gbm_fold_", k, ".rds"))
  saveRDS(xgbmodel, file = paste0("model_xgb_fold_", k, ".rds"))
  saveRDS(catmodel, file = paste0("model_catboost_fold_", k, ".rds"))

}
close(conn)

