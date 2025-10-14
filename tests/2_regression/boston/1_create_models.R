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
iterations <- 1000
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
setwd("/Users/jkhong/Desktop/guide-gb/tests/2_regression/boston")
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

  # gbm (regression)
  set.seed(SEED)
  gbmmodel <- gbm(
    formula = y ~ .,
    data = data.frame(train_x, y=train_y),
    distribution = "gaussian",
    n.trees = iterations,
    interaction.depth = max_depth,
    shrinkage = eta,
    n.minobsinnode = min_child_weight
  )
  pred_gbm <- predict(gbmmodel, test_x, n.trees = iterations)
  rmse_ <- sqrt(mean((pred_gbm - test_y)^2))
  mae_  <- mean(abs(pred_gbm - test_y))
  r2   <- 1 - sum((pred_gbm - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_gbm <- predict(gbmmodel, train_x, n.trees = iterations)
  train_rmse_ <- sqrt(mean((train_pred_gbm - train_y)^2))
  update_matrix_regression("gbm", k, rmse_, train_rmse_, mae_, r2, nrow(test_x), nrow(train_x), conn)

  # xgboost (regression)
  options(na.action = "na.pass")
  train_x2 <- model.matrix(~ . - 1, data = train_x, na.action=na.pass)
  test_x2  <- model.matrix(~ . - 1, data = test_x, na.action=na.pass)
  options(na.action = "na.omit")

  dtrain <- xgb.DMatrix(data = as.matrix(train_x2), label = train_y)
  dtest  <- xgb.DMatrix(data = as.matrix(test_x2),  label = test_y)
  params <- list(objective = "reg:squarederror", eta=eta, max_depth=max_depth,
                 min_child_weight=min_child_weight, eval_metric = "rmse")
  set.seed(SEED)
  xgbmodel <- xgb.train(params = params, data = dtrain, nrounds = iterations)
  pred_xgb <- predict(xgbmodel, dtest)
  rmse_ <- sqrt(mean((pred_xgb - test_y)^2))
  mae_  <- mean(abs(pred_xgb - test_y))
  r2   <- 1 - sum((pred_xgb - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_xgb <- predict(xgbmodel, dtrain)
  train_rmse_ <- sqrt(mean((train_pred_xgb - train_y)^2))
  update_matrix_regression("xgboost", k, rmse_, train_rmse_, mae_, r2, nrow(dtest), nrow(dtrain), conn)

  # catboost (regression)
  train_pool <- catboost.load_pool(data = train_x, label = train_y)
  test_pool  <- catboost.load_pool(data = test_x,  label = test_y)
  catmodel <- catboost.train(train_pool, NULL,
                             params = list(loss_function = 'RMSE',
                                           iterations = iterations,
                                           learning_rate = eta,
                                           depth = max_depth,
                                           random_seed = SEED))
  pred_cat <- catboost.predict(catmodel, test_pool, prediction_type = 'RawFormulaVal')
  rmse_ <- sqrt(mean((pred_cat - test_y)^2))
  mae_  <- mean(abs(pred_cat - test_y))
  r2   <- 1 - sum((pred_cat - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_cat <- catboost.predict(catmodel, train_pool, prediction_type = 'RawFormulaVal')
  train_rmse_ <- sqrt(mean((train_pred_cat - train_y)^2))
  update_matrix_regression("catboost", k, rmse_, train_rmse_, mae_, r2, nrow(test_pool), nrow(train_pool), conn)

  # save models
  saveRDS(model, file = paste0("model_guide_gb_fold_", k, ".rds"))
  saveRDS(gbmmodel, file = paste0("model_gbm_fold_", k, ".rds"))
  saveRDS(xgbmodel, file = paste0("model_xgb_fold_", k, ".rds"))
  saveRDS(catmodel, file = paste0("model_catboost_fold_", k, ".rds"))

}
close(conn)

