library(caret)
library(pROC)
library(gbm)

data("BreastCancer", package = "mlbench")
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

x <- BreastCancer[,2:10]
y <- as.integer(BreastCancer$Class == "malignant")
guide_path <- "/Users/jkhong/Desktop/guide-gb/guide"
run_folder <- "/Users/jkhong/Desktop/guide-gb/guide_run2"
config_path = "/Users/jkhong/Desktop/guide-gb/data/clas_bin_breastcancer/in"
eta <- 0.05
iterations <- 250
epsilon <- 1e-5

# convert all x to numeric
for (col in colnames(x)) {
  x[[col]] <- as.numeric(as.character(x[[col]]))
}

# time the model fitting
model <- guide_gb(x, y, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="binary_classification",
                   max_split_levels=4, min_node_size=2, iterations=iterations, bag_fraction=0.5, bag_seed=456,
                   fit_pred_exact = TRUE)
model$fit$err
## using "response"
pred <- predict(model, newdata = data.frame(x), n_trees = 1:model$fit$iterations, type = "response")
log_loss_vec <- rep(0, model$fit$iterations)
for (i in 1:model$fit$iterations) {
  log_loss_vec[i] <- loglik(actual = y, pred = pred[,i])
}
round(log_loss_vec - model$fit$err, 6)

## using "link"
pred <- predict(model, newdata = data.frame(x), n_trees = 1:model$fit$iterations, type = "link")
log_loss_vec <- rep(0, model$fit$iterations)
for (i in 1:model$fit$iterations) {
  log_loss_vec[i] <- loglik2(actual = y, log_odds = pred[,i])
}
round(log_loss_vec - model$fit$err, 6)

## sense check using utils func
(res <- sense_check_calc(model, x, y))
round(res, 6)


# test equivalent in gbm
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(x, y), 
  distribution = "bernoulli",  # for binary classification
  n.trees = 250,               # number of boosting iterations
  interaction.depth = 4,       # tree depth
  shrinkage = 0.05,            # learning rate
  n.minobsinnode = 2,
  bag.fraction = 0.5,
)
gbmmodel$train.error
## using "response"
pred2 <- predict(gbmmodel, newdata = data.frame(x), n.trees = 1:250, type = "response")
# log likelihood
log_loss_vec2 <- rep(0, 250)
for (i in 1:250) {
  log_loss_vec2[i] <- -2 * mean(y * log(pred2[,i]) + (1 - y) * log(1 - pred2[,i]))
}
# ok
round(log_loss_vec2 - gbmmodel$train.error, 6)

## using "link"
pred2 <- predict(gbmmodel, newdata = data.frame(x), n.trees = 1:250, type = "link")
log_loss_vec2 <- rep(0, 250)
for (i in 1:250) {
  log_loss_vec2[i] <- loglik2(actual = y, log_odds = pred2[,i])
}
round(log_loss_vec2 - gbmmodel$train.error, 6)
