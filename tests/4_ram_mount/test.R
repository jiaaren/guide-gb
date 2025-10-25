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
iterations <- 250
epsilon <- 1e-5

start_time <- Sys.time()
model <- guide_gb(x, y, guide_path, run_folder, eta=0.05, iterations=250, type = "regression")
end_time <- Sys.time()

# print time difference
end_time - start_time

start_time <- Sys.time()
pred <- predict(model, x)
end_time <- Sys.time()
end_time - start_time

# train rmse
rmse(pred - y)
