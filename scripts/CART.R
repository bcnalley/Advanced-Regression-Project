library(mltools)
library(rpart)

load("data-raw/attend.RData")

attend_train$Fill2 <- attend_train$Fill.Rate^2

attend_train2 <- attend_train[, c(1:4, 6:14)]

attend_tree_full <- rpart(Fill2 ~ . , method = "anova",
                          control = rpart.control(cp = 0.0, minsplit = 2),
                          data = attend_train2)
plot(attend_tree_full, margin = 0.1)
text(attend_tree_full, use.n = TRUE)

plotcp(attend_tree_full)  

# Find min cp value
cp <- attend_tree_full$cptable[which.min(attend_tree_full$cptable[, "xerror"]), "CP"]
attend_tree_full$cptable
cp
#0.001355095

# Find 1-SE value
cp.select <- function(big.tree) {
  min.x <- which.min(big.tree$cptable[, 4]) #column 4 is xerror
  for(i in 1:nrow(big.tree$cptable)) {
    if(big.tree$cptable[i, 4] < big.tree$cptable[min.x, 4] + big.tree$cptable[min.x, 5]) return(big.tree$cptable[i, 1]) #column 5: xstd, column 1: cp 
  }
}
cp.select(attend_tree_full)
# 0.003431024

# Min cp value tree
attend_tree_min <- rpart(Fill2 ~ . , method = "anova",
                          control = rpart.control(cp = 0.001355095, minsplit = 2),
                          data = attend_train2)
plot(attend_tree_min, margin = 0.1)
text(attend_tree_min, use.n = TRUE)

# 1-SE cp tree
attend_tree_1se <- rpart(Fill2 ~ . , method = "anova",
                         control = rpart.control(cp = 0.003431024, minsplit = 2),
                         data = attend_train2)
plot(attend_tree_1se, margin = 0.1)
text(attend_tree_1se, use.n = TRUE)

variable_tree <- rpart(Fill2 ~ . , method = "anova",
                       control = rpart.control(maxdepth = 12), data = attend_train2)
plot(variable_tree, margin = 0.1)
text(variable_tree, use.n = TRUE)

## Predict on Test Set ##

# Compare MSEs
tree_min_pred <- predict(attend_tree_min, newdata = attend_test[, c(1:4, 6:13)])
tree_min_mse <- mse(preds = tree_min_pred, actuals = as.vector(attend_test$Fill.Rate))
tree_min_mse
#0.04338522

tree_1se_pred <- predict(attend_tree_1se, newdata = attend_test[, c(1:4, 6:13)])
tree_1se_mse <- mse(preds = tree_1se_pred, actuals = as.vector(attend_test$Fill.Rate))
tree_1se_mse
#0.04457094

# Difference in prediction MSE is small so will go with 1-SE tree because 
# it's smaller, so more interpretable.
save(attend_tree_1se, file = "data-raw/CART.RData")
