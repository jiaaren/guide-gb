library(caret)
library(pROC)

data("BreastCancer", package = "mlbench")
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

x <- BreastCancer[,2:10]
x <- data.frame(lapply(x, function(col) as.numeric(as.character(col))))
y <- as.integer(BreastCancer$Class == "malignant")
guide_path <- "/Users/jkhong/Desktop/guide-gb/guide"
run_folder <- "/Users/jkhong/Desktop/guide-gb/guide_run2"
config_path = "/Users/jkhong/Desktop/guide-gb/data/clas_bin_breastcancer/in"
eta <- 0.05
iterations <- 250
epsilon <- 1e-5

# stratify splitting
set.seed(123)
folds_list <- createFolds(y, k = 5, list = TRUE, returnTrain = FALSE)

# Example: fold 1 is test, others are train
i <- 1
test  <- x[folds_list[[i]], ]
train <- x[-folds_list[[i]], ]
testy <- y[folds_list[[i]]]
trainy <- y[-folds_list[[i]]]
# time the model fitting
start <- Sys.time()
model <- guide_gb(train, trainy, guide_path=guide_path, config_path=config_path,
                   run_folder=run_folder, type="binary_classification",
                   max_split_levels=4, min_node_size=2, iterations=iterations, bag_fraction=0.5, bag_seed=456,
                   fit_pred_exact = TRUE)
end <- Sys.time()
end - start
pred <- predict(model, test)
table(testy, pred > 0.5)
auc(testy, pred)


# compare with GBM
library(gbm)
gbmmodel <- gbm(
  formula = y ~ ., 
  data = data.frame(train, y=trainy), 
  distribution = "bernoulli",  # for binary classification
  n.trees = 250,               # number of boosting iterations
  interaction.depth = 4,       # tree depth
  shrinkage = 0.05,            # learning rate
  n.minobsinnode = 2,
  bag.fraction = 0.5         # subsampling fraction
)
pred2 <- predict(gbmmodel, test)
pred2 <- 1/(1+exp(-pred2))
table(testy, pred2 > 0.5)
auc(testy, pred2)


