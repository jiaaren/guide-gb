library(gbm)
library(xgboost)
library(catboost)
setwd("/Users/jkhong/Desktop/guide-gb")
# Load functions for GUIDE-GB
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

data <- read.csv("/Users/jkhong/Desktop/guide-gb/data/clas_bin_breastcancer/breastcancer.csv", header = TRUE)
x <- data[,1:9]
x <- data.frame(sapply(x, as.numeric))
y <- as.numeric(data$y)
folds <- data$fold

# models' parameters
DT <- 0.5 # decision threshold
SEED <- 456
eta <- 0.01
iterations <- 1000
max_depth <- 4
min_child_weight <- 2


setwd("/Users/jkhong/Desktop/guide-gb/tests/3_binary_classification/breastcancer2")
conn <- file("result.csv", open="at")
writeLines("model,fold,type,TN,FN,TP,FP", conn)
update_matrix <- function(tab, model, type, fold,conn=conn) {
  writeLines(paste(model,fold,type,tab[1,1],tab[2,1],tab[2,2],tab[1,2],sep=","),conn)
}

for (k in sort(unique(folds))) {
# define folds to subset train and test set, of which has been split in 0_create_cross_val.R
# k <- 3
train_x <- x[folds != k, ]
train_y <- y[folds != k]
test_x  <- x[folds == k, ]
test_y  <- y[folds == k]

# train and predict using guide-gb
guide_path <- '/Users/jkhong/Desktop/guide-gb/guide'
run_folder <- '/Users/jkhong/Desktop/guide-gb/guide_run'
set.seed(SEED) # seed is not currently used in this implementation, but may be relevant if stochastic training is implemented
model <- guide_gb(train_x, train_y, 
                  guide_path, run_folder, eta=eta, iterations=iterations,
                  type = "binary_classification")
pred <- predict(model, test_x)
tab <- table(actual=test_y, pred=as.integer(pred > DT))
update_matrix(tab, "guide-gb", "test", k, conn)
# hist(pred)


# train and predict using gbm
set.seed(SEED)
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(train_x, y=train_y), 
  distribution = "bernoulli",  # for binary classification
  n.trees = iterations,               # number of boosting iterations
  interaction.depth = max_depth,       # tree depth
  shrinkage = eta,            # learning rate
  n.minobsinnode = min_child_weight
)
pred2 <- predict(gbmmodel, test_x)
pred2 <- 1/(1+exp(-pred2))
table(test_y, as.integer(pred2 > DT))
tab2 <- table(test_y, as.integer(pred2 > DT))
update_matrix(tab2, "gbm", "test", k, conn)
# hist(pred2)


# train and predict using xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)
params <- list(objective = "binary:logistic", eta=eta, max_depth=max_depth, min_child_weight=min_child_weight, eval_metric = "logloss",
min_data_in_leaf = min_child_weight) # min_data_in_leaf is equivalent to min_child_weight in xgboost
set.seed(SEED)
xgbmodel <- xgb.train(params = params, data = dtrain, nrounds = iterations)
pred3 <- predict(xgbmodel, dtest)
tab3 <- table(test_y, as.integer(pred3 > DT))
update_matrix(tab3, "xgboost", "test", k, conn)
# hist(pred3)


# train and predict using catboost
train_pool <- catboost.load_pool(data = train_x, label = train_y)
test_pool <- catboost.load_pool(data = test_x, label = test_y)
catmodel <- catboost.train(train_pool, NULL, 
                           params = list(loss_function = 'Logloss', 
                                         iterations = iterations,
                                         learning_rate = eta,
                                         depth = max_depth,
                                         random_seed = SEED))
pred4 <- catboost.predict(catmodel, test_pool, prediction_type = 'Probability')
tab4 <- table(test_y, as.integer(pred4 > DT))
update_matrix(tab4, "catboost", "test", k, conn)
# hist(pred4)

# save models above
saveRDS(model, file = paste0("model_guide_gb_fold_", k, ".rds"))
saveRDS(gbmmodel, file = paste0("model_gbm_fold_", k, ".rds"))
saveRDS(xgbmodel, file = paste0("model_xgb_fold_", k, ".rds"))
saveRDS(catmodel, file = paste0("model_catboost_fold_", k, ".rds"))


} 
# End of for loop for k-fold cross validation
close(conn)

#       pred
# actual  0  1
#      0 43  2
#      1  1 23
