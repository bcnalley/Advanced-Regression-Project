load("data-raw/attend.RData")

library(glmnet)
library(glmnetUtils)
library(mltools)

attend_train$Fill2 <- attend_train$Fill.Rate^2


attend_train_x <- data.matrix(attend_train[, c(1:4, 6:13)])
attend_train_y <- as.matrix(attend_train[, 14])

fit <- glmnetUtils::cva.glmnet(x = attend_train_x, y = attend_train_y,
                               formula = y ~ x)
plot(fit)

get_alpha <- function(fit) {
  alpha <- fit$alpha
  error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  alpha[which.min(error)]
}

# Get all parameters.
get_model_params <- function(fit) {
  alpha <- fit$alpha
  lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
  error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best <- which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}

get_alpha(fit)
#0.008
get_model_params(fit)
# lambdamin = 0.003028653, lambda1se = 0.09466708

# Lambda Min
attend_elastic_min <- glmnet::glmnet(attend_train_x, attend_train_y, alpha = 0.008,
                                 lambda = 0.003028653)
coef(attend_elastic_min)
#Time, Rank(-), Rivalry, TV, New.Coach(-), Tailgating, PRCP(-), SNOW, TMAX,
# Opponent_Rank(-), Conference(-), Win_Pct
attend_elastic_min
# 34.84 % Deviance explained

#Lambda 1SE
attend_elastic_1se <- glmnet::glmnet(attend_train_x, attend_train_y, alpha = 0.008,
                                     lambda = 0.09466708)
coef(attend_elastic_1se)
#Time, Rank(-), Rivalry, TV, New.Coach(-), Tailgating, PRCP(-), SNOW, TMAX,
# Opponent_Rank(-), Conference(-), Win_Pct

en_coef <- as.matrix(coef(attend_elastic_1se))
attend_elastic_1se
# 33.86 % Deviance explained

print(xtable(en_coef), caption = c("Elastic Net Coefficients"), floating = FALSE,
      latex.environments = NULL, booktabs = TRUE)

## Predict on Test Set ##

# Compare MSEs
en_min_pred <- predict(attend_elastic_min, newx = data.matrix(attend_test[, c(1:4, 6:13)]))
en_min_mse <- mse(preds = en_min_pred, actuals = as.vector(attend_test$Fill.Rate))
en_min_mse
#0.04945916

en_1se_pred <- predict(attend_elastic_1se, newx = data.matrix(attend_test[, c(1:4, 6:13)]))
en_1se_mse <- mse(preds = en_1se_pred, actuals = as.vector(attend_test$Fill.Rate))
en_1se_mse
#0.0470152

# Very close, go with 1SE model because it theoretically should generalize better,
# and because it has slightly lower MSE
save(attend_elastic_1se, file = "data-raw/EN.RData")
