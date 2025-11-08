library(MASS)
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

data <- read.csv("/Users/jkhong/Desktop/guide-gb/tests/8_handling_of_NA_stepwise/NA_boston.csv")
x <- data[,1:13]
y <- data$y
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
model <- guide_gb(x=train, y=trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="stepwise",
                   max_split_levels=4, min_node_size=2, iterations=500, bag_fraction=0.5, bag_seed=456,
                   val_x=test, val_y=testy, eta=eta,
                   fit_pred_exact = TRUE)
pred <- predict(model, test)
rmse(testy, pred)

pred_trains <- predict(model, train, n_tree=1:144)
mse_trains <- apply(pred_trains, 2, function(pr) mse(trainy, pr))

