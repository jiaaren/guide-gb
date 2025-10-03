# modify working directory as needed
setwd("/Users/jkhong/Desktop/guide-gb")

# Load all functions into current session
for (f in list.files("R", full.names = TRUE)) {
  source(f)
}

x <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
y <- 5 + 2*x$x1 - x$x2 + rnorm(100)
model_reg <- my_model(x, y, type = "regression")
predict(model_reg, newdata = x[1:5, ])
print(model_reg)

# Classification
y_class <- rbinom(100, 1, plogis(2*x$x1 - x$x2))
model_class <- my_model(x, y_class, type = "classification")
predict(model_class, newdata = x[1:5, ])

