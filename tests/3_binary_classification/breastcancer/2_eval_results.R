res <- read.csv("/Users/jkhong/Desktop/guide-gb/tests/3_binary_classification/breastcancer2/result.csv")

# Accuracy
res$accuracy <- (res$TP + res$TN) / (res$TP + res$FP + res$FN + res$TN)
# Precision (Positive Predictive Value)
res$precision <- res$TP / (res$TP + res$FP)
# Recall (Sensitivity, True Positive Rate)
res$recall <- res$TP / (res$TP + res$FN)
# Specificity (True Negative Rate)
res$specificity <- res$TN / (res$TN + res$FP)
# F1 Score
res$f1 <- 2 * res$precision * res$recall / (res$precision + res$recall)
res$balacc <- (res$recall + res$specificity) / 2
metric_cols <- c("accuracy", "precision", "recall", "specificity","balacc","f1")


aov_model <- aov(specificity ~ model, data = res)
summary(aov_model)
TukeyHSD(aov_model)

# Compute means by model and type
res_summary <- aggregate(
  res[metric_cols],
  by = list(model = res$model, type = res$type),
  FUN = mean,
  na.rm = TRUE
)

res_summary2 <- res_summary
res_summary2[,3:8] <- round(res_summary2[,3:8],3)
res_summary2

res_summary_sd <- aggregate(
  res[metric_cols],
  by = list(model = res$model, type = res$type),
  FUN = sd,
  na.rm = TRUE
)
res_summary_sd2 <- res_summary_sd
res_summary_sd2[,3:8] <- round(res_summary_sd2[,3:8],3)
res_summary_sd2


guide_test <- res[res$model=='guide-gb' & res$type=='test',]
gbm_test <- res[res$model=='xgboost' & res$type=='test',]

# t.test for recall/sensitivity
t.test(gbm_test$recall, guide_test$recall)$p.value
# t.test for specificity
t.test(guide_test$specificity, gbm_test$specificity)$p.value
# t.test for balanced accuracy
t.test(guide_test$balacc, gbm_test$balacc)$p.value


# paired t-test
guide_and_gbm_diff <- guide_test[,c(8,10,11,13)] - gbm_test[,c(8,10,11,13)]
guide_and_gbm_diff
apply(guide_and_gbm_diff, MARGIN=2, function(x) {t.test(x)$p.value})
