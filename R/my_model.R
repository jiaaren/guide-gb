#' Fit a custom regression or classification model
#'
#' @param x A data frame of predictors.
#' @param y A vector of outcomes.
#' @param type Either "regression" or "classification".
#'
#' @return An object of class "my_model".
#' @export
#'
#' @examples
#' x <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
#' y <- 2 + 3*x$x1 - x$x2 + rnorm(50)
#' model <- my_model(x, y, type = "regression")
#' predict(model, newdata = x)
my_model <- function(x, y, type = c("regression", "classification")) {
  type <- match.arg(type)

  if (type == "regression") {
    fit <- lm(y ~ ., data = data.frame(y = y, x))
  }

  if (type == "classification") {
    fit <- glm(y ~ ., data = data.frame(y = y, x), family = binomial)
  }

  structure(
    list(fit = fit, type = type),
    class = "my_model"
  )
}
