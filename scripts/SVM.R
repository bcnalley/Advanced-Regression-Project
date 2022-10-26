load("data-raw/attend.RData")

library(mltools)
library(EZtune)
library(e1071)
library(doParallel)

attend_train_x <- data.matrix(attend_train[, c(1:4, 6:13)])
attend_train_y <- as.matrix(attend_train[, 5])

attend_train_gbm <- attend_train[, c(1:13)]
attend_train_gbm$New.Coach <- as.factor(attend_train_gbm$New.Coach)
attend_train_gbm$Tailgating <- as.factor(attend_train_gbm$Tailgating)

attend_train_gbm_x <- data.matrix(attend_train_gbm[, c(1:4, 6:13)])
attend_train_gbm_y <- as.matrix(attend_train_gbm[, 5])

set.seed(6117)

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

attend_svm_tune <- eztune(attend_train_gbm_x, attend_train_gbm_y, method = "svm",
                          fast = FALSE, cross = 10)
attend_svm_tune
# gamma = 0.015625, cost = 33

attend_tuned_svm <- svm(Fill.Rate ~ . , probability = TRUE, cost = 33,
                      gamma = 0.015625, data = attend_train_gbm)


## Predict on Test Data ##

svm_pred <- predict(attend_tuned_svm, newdata = attend_test[, c(1:4, 6:13)])
svm_mse <- mse(preds = svm_pred, actuals = as.vector(attend_test$Fill.Rate))
svm_mse
#0.02254111

save(attend_tuned_svm, file = "data-raw/SVM.RData")

stopCluster(cl)
