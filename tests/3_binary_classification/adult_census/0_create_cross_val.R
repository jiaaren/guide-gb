# source - https://archive.ics.uci.edu/dataset/2/adult
data <- read.csv("/Users/jkhong/Desktop/guide-gb/tests/3_binary_classification/adult_census/adult.data",
        header = FALSE, sep=",", strip.white=TRUE, na.strings = c("?"))
colnames(data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
# exclude "fnlwgt" because cardinality is too high
# exclude education_num because it is redundant to education
sapply(data, function(col) length(unique(col)))
x <- data[,c(1,2,4,6:14)]
# unique labels are either "<=50K" or ">50K"
unique(data$income)
# y relates to income higher than 50k
y <- as.integer(data$income == '>50K')

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
write.csv(cv_x, file = "/Users/jkhong/Desktop/guide-gb/data/clas_bin_aduitcensus/adultcensus.csv", row.names = FALSE)
