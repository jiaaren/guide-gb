# metrics
mse <- function(actual, pred) {
  mean((actual - pred)^2)
}

rmse <- function(actual, pred) {
  sqrt(mse(actual, pred))
}

loglik <- function(actual, pred) {
  -2 * mean(actual * log(pred) + (1 - actual) * log(1 - pred))
}

loglik2 <- function(actual, log_odds) {
  -2 * mean(actual * log_odds - log(1 + exp(log_odds)))
}
