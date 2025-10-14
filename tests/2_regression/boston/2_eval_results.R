results <- read.csv("/Users/jkhong/Desktop/guide-gb/tests/2_regression/boston/result_regression.csv", header = TRUE)
res2 <- results[,1:4]
res2$RMSE_diff <- results$RMSE_test - results$RMSE_train
# group by model and get mean and standard error of RMSE_test, RMSE_train, RMSE_diff
aggregate(cbind(RMSE_test, RMSE_train, RMSE_diff) ~ model, data = res2, 
        FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))

# perform anova over RMSE_diff
anova_res <- aov(RMSE_diff ~ model, data = res2)
summary(anova_res)
TukeyHSD(anova_res)

# perform bonferroni paired t-test between guide-gb and all models for rmse_diff
pairwise.t.test(res2$RMSE_diff, res2$model, p.adjust.method = "bonferroni", paired = TRUE)

# long form
p1 <- t.test(res2$RMSE_diff[res2$model == "guide-gb"], res2$RMSE_diff[res2$model == "gbm"], paired = TRUE)$p.value
p2 <- t.test(res2$RMSE_diff[res2$model == "guide-gb"], res2$RMSE_diff[res2$model == "xgboost"], paired = TRUE)$p.value
p3 <- t.test(res2$RMSE_diff[res2$model == "guide-gb"], res2$RMSE_diff[res2$model == "catboost"], paired = TRUE)$p.value
p_values <- c(p1, p2, p3)
p_adjusted <- p.adjust(p_values, method = "bonferroni")
names(p_adjusted) <- c("gbm", "xgboost", "catboost")
p_adjusted

data.frame(
  Comparison = names(p_adjusted),
  Raw_p = p_values,
  Bonferroni_p = p_adjusted,
  Significant = p_adjusted < 0.05
)
