load("data-raw/attend.RData")

library(MASS)
library(mltools)
library(olsrr)

# OLS Model 1
attend_lm <- lm(Fill.Rate ~ . , data = attend_train)
summary(attend_lm)
# adj. R^2 = .4233, Residual standard error = .1635

bc <- boxcox(attend_lm, lambda = seq(-2, 4, 1/10))
lambda_bc <- bc$x[which.max(bc$y)]
lambda_bc
# 2.36

# Collinearity Diagnostics
ols_coll_diag(attend_lm)
# seems fine

#Residual plot
plot(predict(attend_lm), resid(attend_lm), xlab = "Predicted Y",
     ylab = "Residuals", 
     main = "Predicted Y vs Residuals")

#QQ Plot
qqnorm(resid(attend_lm), main = "QQ Plot of Residuals")
qqline(resid(attend_lm))
# Pretty good

#Y vs Predicted Y
plot(predict(attend_lm), attend_train$Fill.Rate, xlab = "Predicted Y",
     ylab = "Observed Y", 
     main = "Observed Y vs Predicted Y")

# OLS Model 2
attend_lm2 <- lm(Fill.Rate^2 ~ ., data = attend_train)
summary(attend_lm2)
# higher adj. R^2 of .4531, Residual Standard Error = 0.2227

# Collinearity Diagnostics
ols_coll_diag(attend_lm2)
# seems fine

#Residual plot
plot(predict(attend_lm2), resid(attend_lm2), xlab = "Predicted Y",
     ylab = "Residuals", 
     main = "Predicted Y vs Residuals")

#QQ Plot
qqnorm(resid(attend_lm2), main = "QQ Plot of Residuals")
qqline(resid(attend_lm2))
# Better than the untransformed fill rate

#Y vs Predicted Y
plot(predict(attend_lm2), attend_train$Fill.Rate, xlab = "Predicted Y",
     ylab = "Observed Y", 
     main = "Observed Y vs Predicted Y")


# Compare MSEs
lm1_pred <- predict(attend_lm, newdata = attend_test)
lm1_mse <- mse(preds = lm1_pred, actuals = as.vector(attend_test$Fill.Rate))
lm1_mse
#0.02612295

lm2_pred <- predict(attend_lm2, newdata = attend_test)
lm2_mse <- mse(preds = lm2_pred, actuals = as.vector(attend_test$Fill.Rate))
lm2_mse
#0.0456859

# Going to keep model 2 even though MSE is a little better for model 1, 
# because the transformation makes fill rate much more linear

save(attend_lm2, file = "data-raw/lm2.RData")

qqnorm(attend$TMAX)
qqline(attend$TMAX)
hist(attend$TMAX)

qqnorm(attend_train$Win_Pct)
qqline(attend_train$Win_Pct)
