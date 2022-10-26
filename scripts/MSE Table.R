library(xtable)

MSEs <- data.frame(Test_MSE = c(lm2_mse, en_1se_mse, tree_1se_mse, rf_full_mse,
                                gbm_mse, svm_mse))
rownames(MSEs) <- c("Ordinary Least Squares", "Elastic Net", "Regression Tree",
                    "Random Forest", "Gradient Boosting Machine",
                    "Support Vector Machine")

save(MSEs, file = "data-raw/MSE Table.RData")

#latex(summary(MSEs)[[1]], cdec = c(0, 2, 2, 2, 4), na.blank = TRUE,
#      booktabs = TRUE, table.env = FALSE, center = "none", file = "", title = "")

print(xtable(MSEs), floating = FALSE, latex.environments = NULL,
      booktabs = TRUE)

MSEs
