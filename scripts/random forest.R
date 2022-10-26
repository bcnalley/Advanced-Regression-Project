library(randomForest)
library(mltools)

load("data-raw/attend.RData")

attend_train$Fill2 <- attend_train$Fill.Rate^2

attend_train2 <- attend_train[, c(1:4, 6:14)]

set.seed(6117)

attend_train_rf <- randomForest(Fill2 ~ . , importance = TRUE,
                                data = attend_train2)
attend_train_rf
# % variance explained = 55.74, mean of squared residuals = 0.04012014

#Variable Importance Plot
varImpPlot(attend_train_rf, scale = FALSE)
# Conference far and away most important variable
# big drop before Tailgating, Win_Pct and Rank

# RF with only most important variables
attend_train_trim <- randomForest(Fill2 ~ Conference + Tailgating + Win_Pct + Rank,
                                  data = attend_train2)
attend_train_trim
# % variance explained = 44.24, mean of squared residuals = 0.0505411


## Predict on Test Set ##

# Compare MSEs
rf_full_pred <- predict(attend_train_rf, newdata = attend_test[, c(1:4, 6:13)])
rf_full_mse <- mse(preds = rf_full_pred, actuals = as.vector(attend_test$Fill.Rate))
rf_full_mse
#0.03968691

rf_trim_pred <- predict(attend_train_trim, newdata = attend_test[, c(1:4, 6:13)])
rf_trim_mse <- mse(preds = rf_trim_pred, actuals = as.vector(attend_test$Fill.Rate))
rf_trim_mse
#0.0419294

# will go with full tree because it makes more accurate predictions
save(attend_train_rf, file = "data-raw/RF.RData")
