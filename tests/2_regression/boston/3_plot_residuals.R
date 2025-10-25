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
# sense chcek over count of folds
table(folds)

k <- 7
# read rds
model <- readRDS(paste("/Users/jkhong/Desktop/guide-gb/tests/2_regression/boston/model_guide_gb_fold_",k,".rds",sep=''))
train_x <- x[folds != k, ]
train_y <- y[folds != k]
test_x  <- x[folds == k, ]
test_y  <- y[folds == k]

pred <- predict(model, test_x)
residuals <- test_y - pred

quartz()
# Plot residuals
xlim <- c(0, 55)
ylim <- c(-15, 15)
plot(test_y, residuals,
     main = 'Residuals plot for GUIDE-gradient boosted tree\n(Test dataset)',
     xlab = 'Actual median house value',
     ylab = 'Residuals',
     pch = 3, col = 'blue',
     xlim = xlim, ylim = ylim,
     xaxt = "n", yaxt = "n")        # suppress default axes

# ticks every 10 on x, every 5 on y based on limits
axis(1, at = seq(xlim[1], xlim[2], by = 10))
axis(2, at = seq(ylim[1], ylim[2], by = 5))

sd_ <- sd(residuals)
abline(h=0, lty=2)
abline(h=c(-1.96*sd_, 1.96*sd_), lty=2, col='red')
legend(
  x = "topleft",
  lty = c(NA, 2),      # first line dashed, second none (since it's a point)
  pch = c(3, NA),      # first has no symbol, second has pch=3
  col = c("blue", "red"),
  text.col = "black",
  cex = 0.75,
  legend = c("Observation", "95% Confidence Limit")
)
