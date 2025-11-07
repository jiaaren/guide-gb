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
iterations <- 1000
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
start <- Sys.time()
model <- guide_gb(train, trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="constant_exhaustive",
                   max_split_levels=4, min_node_size=2, iterations=iterations, bag_fraction=0.5, bag_seed=123)
end <- Sys.time()
end - start
pred <- predict(model, test)
resid <- testy - pred
rmse(resid)
# x11()
plot(testy, resid); abline(h=0, lty=2)

# sense_check_calc
res <- sense_check_calc(model, train, trainy)
round(res, 6)


# output graph of fitted vs test rmse to determine if early stopping is beneficial
preds_matrix <- predict(model, newdata = train, n_trees = 1:iterations)
preds_matrix_test <- predict(model, newdata = test, n_trees = 1:iterations)
rmse_train <- apply(preds_matrix, 2, function(pred){ rmse(pred - trainy) })
rmse_test <- apply(preds_matrix_test, 2, function(pred){ rmse(pred - testy) })
plot(1:iterations, rmse_train, type='l', col='blue', ylim=range(c(rmse_train, rmse_test)), ylab='RMSE', xlab='Number of Trees', main='Train vs Test RMSE')
lines(1:iterations, rmse_test, col='red')
legend("topright", legend=c("Train RMSE", "Test RMSE"), col=c("blue", "red"), lty=1)

plot(1:iterations, rmse_test, col='red', type='l', ylim=c(median(rmse_test) - 0.1, median(rmse_test) + 0.1),
     ylab='Test RMSE', xlab='Number of Trees', main='Test RMSE with Early Stopping')

best_rmse_idx <- which.min(rmse_test)
rmse_train[best_rmse_idx]
rmse_test[best_rmse_idx]

# gbm
library(gbm)
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(train, y=trainy), 
  distribution = "gaussian",  # for binary classification
  n.trees = iterations,               # number of boosting iterations
  interaction.depth = 4,       # tree depth
  shrinkage = 0.05,            # learning rate
  n.minobsinnode = 2,
  bag.fraction=0.5
)
pred2 <- predict(gbmmodel, test)
resid2 <- testy - pred2
rmse(resid2)
plot(testy, resid2); abline(h=0, lty=2)

