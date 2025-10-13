library(MASS)
# modify working directory as needed
x <- Boston[,-14]
y <- Boston$medv

# create 10 folds
n <- nrow(x)
k <- 10
set.seed(456)
folds <- sample(rep(1:k, length.out = n))

sorted_idx <- sort(folds, index.return=TRUE)
cv_x <- data.frame(cbind(x,y))[sorted_idx$ix, ]
cv_x$fold <- sorted_idx$x
row.names(cv_x) <- NULL
write.csv(cv_x, "/Users/jkhong/Desktop/guide-gb/data/reg_boston/boston.csv", row.names = FALSE)
