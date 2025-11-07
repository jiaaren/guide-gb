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
model <- guide_gb(x=train, y=trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="constant_exhaustive",
                   max_split_levels=4, min_node_size=2, iterations=144, bag_fraction=0.5, bag_seed=456,
                   val_x=test, val_y=testy,
                   fit_pred_exact = TRUE)
pred <- predict(model, test)
rmse(testy, pred)

pred_trains <- predict(model, train, n_tree=1:144)
mse_trains <- apply(pred_trains, 2, function(pr) mse(trainy, pr))




model2 <- guide_gb(x=train, y=trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="regression", complexity="constant_exhaustive",
                   max_split_levels=4, min_node_size=2, iterations=144, bag_fraction=0.5, bag_seed=456,
                   fit_pred_exact = TRUE)
pred2 <- predict(model2, test)
rmse(testy, pred2)
mse(testy, pred2)

pred_trains2 <- predict(model2, train, n_tree=1:144)
mse_trains2 <- apply(pred_trains2, 2, function(pr) mse(trainy, pr))

round(mse_trains - mse_trains2, 100)


# check what columns have NA values in train data
colSums(is.na(train))
# check what columns have NA values in test data
colSums(is.na(test))


# debug fitted files
fitted1 <- read.table("/Users/jkhong/Desktop/guide-gb/tests/7_early_stopping/data1.fitted", header=TRUE)
fitted2 <- read.table("/Users/jkhong/Desktop/guide-gb/tests/7_early_stopping/data2.fitted", header=TRUE)

idx_train <- 1:405
head(fitted1)
# compare the 'train' column
all.equal(fitted1[idx_train, "train"], fitted2$train)
# compare observed column
all.equal(fitted1[idx_train, "observed"], fitted2$observed)
# compare node column
all.equal(fitted1[idx_train, "node"], fitted2$node)
# compare predicted column
all.equal(fitted1[idx_train, "predicted"], fitted2$predicted)

# debug input file
input1 <- read.csv("/Users/jkhong/Desktop/guide-gb/tests/7_early_stopping/data1.csv")
input2 <- read.csv("/Users/jkhong/Desktop/guide-gb/tests/7_early_stopping/data2.csv")

# check equality of train rows
all.equal(input1[idx_train, ], input2)
# check remaining rows in input1 (validation rows)
val_idx <- (nrow(train) + 1):nrow(input1)
val_input1 <- input1[val_idx, ]
val_input1$istrain
