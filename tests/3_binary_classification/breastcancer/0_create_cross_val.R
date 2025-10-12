data("BreastCancer", package = "mlbench")
x <- BreastCancer[,2:10]
y <- as.integer(BreastCancer$Class == 'malignant')

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
sorted_idx_list <- list()
for (key in names(fold_list)) {
  sorted_idx_list[[key]] <- sort(fold_list[[key]], index.return=TRUE)
}
for (key in names(split_data)) {
    sort_x <- sorted_idx_list[[key]]$x
    sort_ix <- sorted_idx_list[[key]]$ix
    split_data[[key]] <- split_data[[key]][sort_ix, ]
    split_data[[key]]$fold <- sort_x
    split_data[[key]]$y <- as.integer(key)
}
cv_x <- do.call(rbind, split_data)
row.names(cv_x) <- NULL;
write.csv(cv_x, file = "/Users/jkhong/Desktop/guide-gb/data/clas_bin_breastcancer/breastcancer.csv", row.names = FALSE)
