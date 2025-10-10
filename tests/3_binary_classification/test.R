data("BreastCancer", package = "mlbench")
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

x <- BreastCancer[,2:10]
y <- as.integer(BreastCancer$Class == 'malignant')
guide_path <- '/Users/jkhong/Desktop/guide-gb/guide'
run_folder <- '/Users/jkhong/Desktop/guide-gb/guide_run'
eta <- 0.05
iterations <- 1000
epsilon <- 1e-5

model <- guide_gb(x, y, guide_path, run_folder, eta=0.05, iterations=1000, type = "binary_classification")
pred <- predict(model, x)
# train rmse
rmse(pred - y)

