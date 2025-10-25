setwd("/Users/jkhong/Desktop/guide-gb/")
# Load functions for GUIDE-GB
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

data <- read.csv("/Users/jkhong/Desktop/guide-gb/data/reg_boston/boston.csv", header = TRUE)
x <- data[,1:13]
# convert all columns to numeric
for (col in colnames(x)) {
  x[[col]] <- as.numeric(x[[col]])
}
y <- as.numeric(data$y)
folds <- data$fold

setwd("/Users/jkhong/Desktop/guide-gb/tests/2_regression/boston4")
conn <- file("result_regression.csv", open="wt")
writeLines("model,fold,RMSE_test,RMSE_train,MAE_,R2,nrow_test,nrow_train", conn)
update_matrix_regression <- function(model_name, fold, rmse_test, rmse_train, mae_, r2, nrow_test, nrow_train, conn=conn) {
  writeLines(paste(model_name, fold, sprintf("%.6f", rmse_test), sprintf("%.6f", rmse_train), sprintf("%.6f", mae_), sprintf("%.6f", r2), sprintf("%.6f", nrow_test), sprintf("%.6f", nrow_train), sep=","), conn)
}

for (k in sort(unique(folds))) {
  # train / test split for fold k
  train_x <- x[folds != k, ]
  train_y <- y[folds != k]
  test_x  <- x[folds == k, ]
  test_y  <- y[folds == k]

  # GUIDE-GB (regression)
  model <- readRDS(paste("model_guide_gb_fold_", k, ".rds", sep=""))
  pred_gbguide <- predict(model, test_x)
  rmse_ <- sqrt(mean((pred_gbguide - test_y)^2))
  mae_  <- mean(abs(pred_gbguide - test_y))
  r2   <- 1 - sum((pred_gbguide - test_y)^2) / sum((test_y - mean(test_y))^2)
  train_pred_gbguide <- predict(model, train_x)
  train_rmse_ <- sqrt(mean((train_pred_gbguide - train_y)^2))
  update_matrix_regression("guide-gb", k, rmse_, train_rmse_, mae_, r2, nrow(test_x), nrow(train_x), conn)

}
close(conn)

