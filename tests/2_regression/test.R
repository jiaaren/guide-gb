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
run_folder <- '/Users/jkhong/Desktop/guide-gb/guide_run'
eta <- 0.05
iterations <- 1000
epsilon <- 1e-5

model <- guide_gb(x, y, guide_path, run_folder, eta=0.05, iterations=1000, type = "regression")
pred <- predict(model, x)
# train rmse
rmse(pred - y)


# test using split of train and test data
n <- nrow(x)
k <- 5
set.seed(456)
folds <- sample(rep(1:k, length.out = n))  # random fold labels

# Example: fold 1 is test, others are train
i <- 5
test  <- x[folds == i, ]
train <- x[folds != i, ]
testy <- y[folds == i]
trainy <- y[folds != i]
model <- guide_gb(train, trainy, guide_path, run_folder, eta=0.05, iterations=1000, type = "regression")
pred <- predict(model, test)

resid <- testy - pred 
# x11()
plot(testy, resid); abline(h=0, lty=2)
rmse(resid)


library(gbm)
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(train, y=trainy), 
  distribution = "gaussian",  # for binary classification
  n.trees = 1000,               # number of boosting iterations
  interaction.depth = 3,       # tree depth
  shrinkage = 0.05,            # learning rate
  n.minobsinnode = 2
)
pred2 <- predict(gbmmodel, test)
resid2 <- testy - pred2
rmse(resid2)
plot(testy, resid2); abline(h=0, lty=2)
