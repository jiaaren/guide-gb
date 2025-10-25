data("BreastCancer", package = "mlbench")
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

x <- BreastCancer[,2:10]
x <- data.frame(lapply(x, function(col) as.numeric(as.character(col))))
y <- as.integer(BreastCancer$Class == 'malignant')
guide_path <- '/Users/jkhong/Desktop/guide-gb/guide'
run_folder <- '/Users/jkhong/Desktop/guide-gb/guide_run2'
eta <- 0.05
iterations <- 250
epsilon <- 1e-5

model <- guide_gb(x, y, guide_path, run_folder, eta=eta, iterations=iterations, type = "binary_classification")

start_time <- Sys.time()
pred <- predict(model, x)
end_time <- Sys.time()
end_time - start_time

# log lik
loglik(y, pred)
# should return all values close to 0
sense_check_calc(model, x)



# test using split of train and test data
n <- nrow(x)
k <- 10
# stratify splitting
split_data <- split(x, f=y)
fold_list <- list()

# make folds
# can reference list using split_data[[1]], split_data[[2]], etc, or names using split_data$`0`, split_data$`1`
set.seed(456)
for (key in names(split_data)) {
  fold_list[[key]] <- sample(rep(1:k, length.out = nrow(split_data[[key]])))
}
lapply(fold_list, table)
# Example: fold 1 is test, others are train
i <- 3
test  <- do.call(rbind, mapply( function(df, folds) df[folds == i, ], split_data, fold_list, SIMPLIFY = FALSE))
train <- do.call(rbind, mapply( function(df, folds) df[folds != i, ], split_data, fold_list, SIMPLIFY = FALSE))
row.names(test) <- NULL; row.names(train) <- NULL;
testy <- NULL; trainy <- NULL
for (key in names(split_data)) {
  testy  <- c(testy, rep(as.integer(key), sum(fold_list[[key]] == i)))
  trainy <- c(trainy, rep(as.integer(key), sum(fold_list[[key]] != i)))
}
model <- guide_gb(train, trainy, guide_path, run_folder, eta=0.05, iterations=1000, type = "binary_classification")
pred <- predict(model, test)
table(testy, pred > 0.5)


library(gbm)
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(train, y=trainy), 
  distribution = "bernoulli",  # for binary classification
  n.trees = 1000,               # number of boosting iterations
  interaction.depth = 3,       # tree depth
  shrinkage = 0.05,            # learning rate
  n.minobsinnode = 2
)
pred2 <- predict(gbmmodel, test)
pred2 <- 1/(1+exp(-pred2))
table(testy, pred2 > 0.5)


