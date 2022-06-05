# My first model is the Linear Regression Model'
attach(Snails)
head(Snails)
install.packages("car")
library(car)

# Fitting a simple Linear Model
fit1 <- lm(Rings ~ ., data = Snails)
summary(fit1)
vif(fit1)
par(mfrow = c(1,1))
plot(fit1)

# Removing heteroscedasticity.
fit2 <- lm(log(Rings) ~ ., data = Snails)
summary(fit2)
plot(fit2)

# Splitting the training and testing data (After analyzing the data I am pretty sure that
# "Linear Regression will outperform all the methods".)
# That's why I have given only 5% to testing!

set.seed(1)
trainingindex<- sample(1:nrow(Snails), 95*nrow(Snails)/100,replace=F)

TrainingData<- Snails[trainingindex,]
TestingData <- Snails[-trainingindex,]

# Calculating Test Rsq
LMFitPred <- predict(fit2, TestingData )
TestRSS <- sum((LMFitPred - log(TestingData[,9]))^2)
TestTSS <- sum((mean(log(TestingData[,9]))- log(TestingData[,9]))^2)
1-TestRSS/TestTSS
plot(LMFitPred, log(TestingData[,9]))

# Performing Subset Selection
install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(log(Rings) ~ ShellWeight + ShuckedWeight +  Diameter +Height + 
                            Type*LongestShell  + WholeWeight + VisceraWeight, data = Snails, nvmax = 25)
reg.summary <- summary(regfit.full)
reg.summary
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# Plotting Adj r2, BIC, Cp
par(mfrow = c(2,2))
plot(regfit.full, scale = 'r2')
plot(regfit.full, scale = 'adjr2')
plot(regfit.full, scale = 'Cp')
plot(regfit.full, scale = 'bic')

# Performing 10-fold CV on the predictors to find the best polynomial.
library(boot)
par(mfrow = c(2,3))
set.seed(2021)
# CV for ShellWeight
cv.shellerror.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(log(Rings) ~ poly(ShellWeight,i), data = Snails)
  cv.shellerror.10[i] <- cv.glm(Snails, glm.fit, K = 10)$delta[1]
  
}
which.min(cv.shellerror.10)
cv.shellerror.10[which.min(cv.shellerror.10)]
plot(1:10, cv.shellerror.10)

# CV for ShuckWeight
cv.shuckerror.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(log(Rings) ~ poly(ShuckedWeight,i), data = Snails)
  cv.shuckerror.10[i] <- cv.glm(Snails, glm.fit, K = 10)$delta[1]
  
}
which.min(cv.shuckerror.10)
cv.shuckerror.10[which.min(cv.shuckerror.10)]
plot(1:10, cv.shuckerror.10)

# CV for VisceraWeight
cv.visceraerror.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(log(Rings) ~ poly(VisceraWeight,i), data = Snails)
  cv.visceraerror.10[i] <- cv.glm(Snails, glm.fit, K = 10)$delta[1]
  
}
which.min(cv.visceraerror.10)
cv.visceraerror.10[which.min(cv.visceraerror.10)]
plot(1:10, cv.visceraerror.10)

# CV for WholeWeight
cv.wholeerror.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(log(Rings) ~ poly(WholeWeight,i), data = Snails)
  cv.wholeerror.10[i] <- cv.glm(Snails, glm.fit, K = 10)$delta[1]
  
}
which.min(cv.wholeerror.10)
cv.wholeerror.10[which.min(cv.wholeerror.10)]
plot(1:10, cv.wholeerror.10)

head(Snails)

# Checking for Height, Diameter, and Longest Shell
# CV for Diameter
cv.diaerror.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(log(Rings) ~ poly(Diameter,i), data = Snails)
  cv.diaerror.10[i] <- cv.glm(Snails, glm.fit, K = 10)$delta[1]
  
}
which.min(cv.diaerror.10)
cv.diaerror.10[which.min(cv.diaerror.10)]
plot(1:10, cv.diaerror.10)

# CV for LongestShell
cv.lserror.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(log(Rings) ~ poly(LongestShell,i), data = Snails)
  cv.lserror.10[i] <- cv.glm(Snails, glm.fit, K = 10)$delta[1]
  
}
which.min(cv.lserror.10)
cv.lserror.10[which.min(cv.lserror.10)]
plot(1:10, cv.lserror.10)

# Final Linear Model
set.seed(1)
par(mfrow = c(2,2))
trainingindex<- sample(1:nrow(Snails), 95*nrow(Snails)/100,replace=F)

TrainingData<- Snails[trainingindex,]
TestingData <- Snails[-trainingindex,]

FinalModel_LR <- lm(log(Rings) ~ log(I(ShellWeight)^6) + poly(ShuckedWeight,6) +poly(Diameter,3) + 
                      Type*poly(LongestShell,3) + Height + poly(WholeWeight,9) + poly(VisceraWeight,4) + 
                      WholeWeight:VisceraWeight , data = TrainingData)
summary(FinalModel_LR)
# Training Adjusted R-Squared of 66%

plot(FinalModel_LR)
par(mfrow = c(1,1))
plot(FinalModel_LR$residuals, time)
FinalModel.timeseries <- ts(FinalModel_LR$residuals, start = c(1,1), frequency = 3325)
plot(FinalModel.timeseries)

# Test R-Squared for final model!
LMFinalFitPred <- exp(predict(FinalModel_LR, TestingData ))
TestRSS <- sum((LMFinalFitPred - TestingData[,9])^2)
TestTSS <- sum((mean(TestingData[,9])- TestingData[,9])^2)
1-TestRSS/TestTSS
plot(LMFinalFitPred, TestingData[,9])
abline(0,1)

# Testing R-Squared is 64.27%. !!!
# Note that I have converted log(rings) back to rings!


# Performing Ridge and Lasso. Just to see how they perform compared to the Lienar Regression.
# As I have expected their didn't outperform Linear Regression in this case.

# Performing Ridge and Lasso Regression
set.seed(1)
install.packages('glmnet')
library(glmnet)
attach(Snails)
x <- model.matrix(Rings ~., TrainingData)[, -1]
X <- model.matrix(Rings ~ ., TestingData)[,-1]
y <- Snails$Rings
Y <- true_test$Rings
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid, standardize = TRUE, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 0.2, newx = X)
plot(ridge.mod)

TestRSS <- sum((ridge.pred - Y)^2)
TestTSS <- sum((mean(Y)- Y)^2)
1-TestRSS/TestTSS
# Around 50% variability explained with best lambda!!!

# Let's perform CV on the Ridge Regression 

set.seed(1)
library(boot)
cv.out <- cv.glmnet(x, y, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam



# Lasso
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid, thresh = 1e-12)
lasso.pred <- predict(ridge.mod, s = bestlam, newx = X)
plot(lasso.mod)

TestRSS <- sum((lasso.pred - Y)^2)
TestTSS <- sum((mean(Y)- Y)^2)
1-TestRSS/TestTSS

cv.lasso <- cv.glmnet(x, y, alpha = 1)
bestlam <- cv.out$lambda.min
bestlam































