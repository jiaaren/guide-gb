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
  library(pROC)
  # convert predicted probabilities to class labels using threshold
  pred_class <- as.integer(pred >= threshold)
  true_class <- as.integer(true)

  TP <- sum(pred_class == 1 & true_class == 1)
  TN <- sum(pred_class == 0 & true_class == 0)
  FP <- sum(pred_class == 1 & true_class == 0)
  FN <- sum(pred_class == 0 & true_class == 1)

  accuracy <- if ((TP + TN + FP + FN) > 0) (TP + TN) / (TP + TN + FP + FN) else NA_real_
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  recall <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_   # sensitivity
  specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
  auc <- as.numeric(auc(true_class, pred, levels = c(0, 1), direction = "<"))
  balanced_accuracy <- mean(c(recall, specificity), na.rm = TRUE)

  # return metrics
  c(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    balanced_accuracy = balanced_accuracy,
    auc = auc
  )
}

save_metrics_classification <- function(MODEL_OUTPUT_FOLDER, OUTPUT_FOLDER, ITERATIONS, prefix,
                                        fold_k, final_model, model_pred_func, fittings_df,
                                        best_params, history, best_iterations,
                                        x_train, y_train, x_test, y_test,
                                        threshold = 0.5) {
  # calculate train metrics - all iter
  y_train_pred <- model_pred_func(final_model, x_train)
  train_metrics <- calc_classification_metrics(y_train, y_train_pred, threshold)
  # calculate test metrics - all iter
  y_test_pred <- model_pred_func(final_model, x_test)
  test_metrics <- calc_classification_metrics(y_test, y_test_pred, threshold)

  # calculate train metrics - best iter
  y_train_pred_best <- model_pred_func(final_model, x_train, n_trees = round(best_iterations))
  train_metrics_best <- calc_classification_metrics(y_train, y_train_pred_best, threshold)
  # calculate test metrics - best iter
  y_test_pred_best <- model_pred_func(final_model, x_test, n_trees = round(best_iterations))
  test_metrics_best <- calc_classification_metrics(y_test, y_test_pred_best, threshold)

  # save history, final model
  saveRDS(final_model, file.path(MODEL_OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, ".rds")))
  write.csv(data.frame(history), file.path(OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, "_history.csv")), row.names = FALSE)
  write.csv(fittings_df, file.path(OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, "_fittings_history.csv")), row.names = FALSE)
  # save best params, along with train and test score
  params_df <- data.frame(matrix(rep(best_params, 4), nrow = 4, byrow = TRUE))
  names(params_df) <- names(best_params)
  metrics_df <- data.frame(rbind(train_metrics, test_metrics, train_metrics_best, test_metrics_best))
  metrics_df$iterations <- c(ITERATIONS, ITERATIONS, round(best_iterations), round(best_iterations))
  metrics_df <- cbind(data.frame(set = c("train", "test", "train_best", "test_best")),
                      params_df, metrics_df)
  write.csv(metrics_df, file.path(OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, "_best_params.csv")), row.names = FALSE)
}

calc_regression_metrics <- function(true, pred) {
  # MSE
  mse_value <- mse(true, pred)
  # RMSE
  rmse_value <- rmse(true, pred)
  # MAE
  mae_value <- mean(abs(true - pred))
  # R-squared
  ss_res <- sum((true - pred)^2)
  ss_tot <- sum((true - mean(true))^2)
  r_squared <- if (ss_tot > 0) 1 - (ss_res / ss_tot) else NA_real_

  # return metrics
  c(
    mse = mse_value,
    rmse = rmse_value,
    mae = mae_value,
    r_squared = r_squared
  )
}

save_metrics_regression <- function(MODEL_OUTPUT_FOLDER, OUTPUT_FOLDER, ITERATIONS, prefix,
                                   fold_k, final_model, model_pred_func, fittings_df,
                                   best_params, history, best_iterations,
                                   x_train, y_train, x_test, y_test) {
  # calculate train metrics - all iter
  y_train_pred <- model_pred_func(final_model, x_train)
  train_metrics <- calc_regression_metrics(y_train, y_train_pred)
  # calculate test metrics - all iter
  y_test_pred <- model_pred_func(final_model, x_test)
  test_metrics <- calc_regression_metrics(y_test, y_test_pred)

  # calculate train metrics - best iter
  y_train_pred_best <- model_pred_func(final_model, x_train, n_trees = round(best_iterations))
  train_metrics_best <- calc_regression_metrics(y_train, y_train_pred_best)
  # calculate test metrics - best iter
  y_test_pred_best <- model_pred_func(final_model, x_test, n_trees = round(best_iterations))
  test_metrics_best <- calc_regression_metrics(y_test, y_test_pred_best)

  # save history, final model
  saveRDS(final_model, file.path(MODEL_OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, ".rds")))
  write.csv(data.frame(history), file.path(OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, "_history.csv")), row.names = FALSE)
  write.csv(fittings_df, file.path(OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, "_fittings_history.csv")), row.names = FALSE)
  # save best params, along with train and test score
  params_df <- data.frame(matrix(rep(best_params, 4), nrow = 4, byrow = TRUE))
  names(params_df) <- names(best_params)
  metrics_df <- data.frame(rbind(train_metrics, test_metrics, train_metrics_best, test_metrics_best))
  metrics_df$iterations <- c(ITERATIONS, ITERATIONS, round(best_iterations), round(best_iterations))
  metrics_df <- cbind(data.frame(set = c("train", "test", "train_best", "test_best")),
                      params_df, metrics_df)
  write.csv(metrics_df, file.path(OUTPUT_FOLDER, paste0(prefix, "_fold_", fold_k, "_best_params.csv")), row.names = FALSE)
}