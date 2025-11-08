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
iterations <- 500
epsilon <- 1e-5

# test using split of train and test data
n <- nrow(x)
k <- 5
set.seed(456)
folds <- sample(rep(1:k, length.out = n))  # random fold labels

# Example: fold 1 is test, others are train
i <- 4
test  <- x[folds == i, ]
train <- x[folds != i, ]
testy <- y[folds == i]
trainy <- y[folds != i]
# time the model fitting
model <- guide_gb(x=train, y=trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="constant_exhaustive",
                   max_split_levels=4, min_node_size=2, iterations=iterations, bag_fraction=0.5, bag_seed=456,
                   val_x=test, val_y=testy, early_stop_rounds = 100,
                   fit_pred_exact = TRUE)
pred <- predict(model, test)
rmse(testy, pred)
model$fit$err_val

pred_trains <- predict(model, train, n_tree=1:model$fit$iterations)
mse_trains <- apply(pred_trains, 2, function(pr) mse(trainy, pr))




model2 <- guide_gb(x=train, y=trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="constant_exhaustive",
                   max_split_levels=4, min_node_size=2, iterations=iterations, bag_fraction=0.5, bag_seed=456,
                   fit_pred_exact = TRUE)
pred2 <- predict(model2, test)
rmse(testy, pred2)
mse(testy, pred2)

pred_trains2 <- predict(model2, train, n_tree=1:model$fit$iterations)
mse_trains2 <- apply(pred_trains2, 2, function(pr) mse(trainy, pr))

round(mse_trains - mse_trains2, 100)

