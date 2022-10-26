load("data-raw/attend.RData")

library(mltools)
library(EZtune)
library(gbm)
library(doParallel)

set.seed(6117)

attend_train_x <- data.matrix(attend_train[, c(1:4, 6:13)])
attend_train_y <- as.matrix(attend_train[, 5])

attend_train_gbm <- attend_train[, c(1:13)]
attend_train_gbm$New.Coach <- as.factor(attend_train_gbm$New.Coach)
attend_train_gbm$Tailgating <- as.factor(attend_train_gbm$Tailgating)


cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#Tune GBM
attend_gbm_tune <- eztune(attend_train_x, attend_train_y, method = "gbm",
                          fast = FALSE, cross = 10)
attend_gbm_tune
# n.trees = 1999, interaction.depth = 10, n.minobsinnode = 5, shrinkage = 0.01

attend_gbm <- gbm(Fill.Rate ~ . , distribution = "gaussian",
                 interaction.depth = 10, n.trees = 1999, shrinkage = 0.01,
                 n.minobsinnode = 5, data = attend_train_gbm)

## Predict on Test Set ##

gbm_pred <- predict(attend_gbm, newdata = attend_test[, c(1:4, 6:13)])
gbm_mse <- mse(preds = gbm_pred, actuals = as.vector(attend_test$Fill.Rate))
gbm_mse
#0.02082515


stopCluster(cl)

save(attend_gbm, file = "data-raw/GMB.RData")
