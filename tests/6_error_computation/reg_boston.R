library(MASS)
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

x <- Boston[,-14]
y <- Boston$medv
guide_path <- '/Users/jkhong/Desktop/guide-gb/guide'
run_folder <- '/Users/jkhong/Desktop/guide-gb/_guide_run'
config_path <- '/Users/jkhong/Desktop/guide-gb/data/reg_boston/in'
eta <- 0.05
iterations <- 250
epsilon <- 1e-5

# GUIDE-GB
model <- guide_gb(x, y, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="constant_exhaustive",
                   max_split_levels=4, min_node_size=2, iterations=iterations, bag_fraction=0.5, bag_seed=456,
                   fit_pred_exact = TRUE)

model$fit$err
pred <- predict(model, x, n_trees = 1:model$fit$iterations)
mse_vec <- rep(0, model$fit$iterations)
for (i in 1:model$fit$iterations) {
  mse_vec[i] <- mse(actual = y, pred = pred[,i])
}
round(mse_vec - model$fit$err, 6)

## sense check using utils func
(res <- sense_check_calc(model, x, y))
round(res, 6)

# GBM
library(gbm)
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(x, y), 
  distribution = "gaussian",  # for binary classification
  n.trees = iterations,               # number of boosting iterations
  interaction.depth = 4,       # tree depth
  shrinkage = 0.05,            # learning rate
  n.minobsinnode = 2,
  bag.fraction=0.5
)

gbmmodel$train.error
pred2 <- predict(gbmmodel, newdata = data.frame(x), n.trees = 1:iterations, type = "response")
# MSE
mse_vec2 <- rep(0, iterations)
for (i in 1:iterations) {
  mse_vec2[i] <- mse(actual = y, pred = pred2[,i])
}
round(mse_vec2 - gbmmodel$train.error, 6)
