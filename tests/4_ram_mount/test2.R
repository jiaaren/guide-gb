library(MASS)
# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")
# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

guide_path <- './guide'
run_folder <- '/Volumes/GUIDERUN'
# Make sure to mount the RAM disks
# copy all relevant files to run_folder
copy_files <- c('/Users/jkhong/Desktop/guide-gb/guide',
                '/Users/jkhong/Desktop/guide-gb/guide_run/data.in',
                '/Users/jkhong/Desktop/guide-gb/guide_run/data.DSC')
file.copy(copy_files, run_folder, recursive = TRUE)

x <- Boston[,-14]
y <- Boston$medv

eta <- 0.05
iterations <- 1000
epsilon <- 1e-5

start_time <- Sys.time()
model <- guide_gb(x, y, guide_path, run_folder, eta=0.05, iterations=1000, type = "regression")
end_time <- Sys.time()

# print time difference
end_time - start_time

pred <- predict(model, x)
# train rmse
rmse(pred - y)
