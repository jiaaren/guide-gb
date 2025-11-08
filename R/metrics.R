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

calc_classification_metrics <- function(true, pred, threshold = 0.5) {
  # convert predicted probabilities to class labels using threshold
  pred_class <- as.integer(pred >= threshold)
  true_class <- as.integer(true)

  TP <- sum(pred_class == 1 & true_class == 1)
  TN <- sum(pred_class == 0 & true_class == 0)
  FP <- sum(pred_class == 1 & true_class == 0)
  FN <- sum(pred_class == 0 & true_class == 1)

  accuracy <- if ((TP + TN + FP + FN) > 0) (TP + TN) / (TP + TN + FP + FN) else NA_real_
  recall <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_   # sensitivity
  specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
  balanced_accuracy <- mean(c(recall, specificity), na.rm = TRUE)

  # return metrics
  c(
    accuracy = accuracy,
    recall = recall,
    specificity = specificity,
    balanced_accuracy = balanced_accuracy
  )
}
