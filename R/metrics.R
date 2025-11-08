#' Mean Squared Error
#' 
#' @param actual Actual values
#' @param pred Predicted values
#' @return MSE value
#' @keywords internal
mse <- function(actual, pred) {
  mean((actual - pred)^2)
}

#' Root Mean Squared Error
#' 
#' @param actual Actual values
#' @param pred Predicted values
#' @return RMSE value
#' @keywords internal
rmse <- function(actual, pred) {
  sqrt(mse(actual, pred))
}

#' Log Likelihood (for probabilities)
#' 
#' @param actual Actual values
#' @param pred Predicted probabilities
#' @return Log likelihood value
#' @keywords internal
loglik <- function(actual, pred) {
  -2 * mean(actual * log(pred) + (1 - actual) * log(1 - pred))
}

#' Log Likelihood (for log odds)
#' 
#' @param actual Actual values
#' @param log_odds Predicted log odds
#' @return Log likelihood value
#' @keywords internal
loglik2 <- function(actual, log_odds) {
  -2 * mean(actual * log_odds - log(1 + exp(log_odds)))
}
