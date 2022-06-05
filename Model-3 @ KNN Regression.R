# Performing KNN Regression
set.seed(1)
trainingindex<- sample(1:nrow(Snails), 0.95*nrow(Snails) ,replace=F)

TrainingData<- Snails[trainingindex,]
TestingData <- Snails[-trainingindex,]

library(FNN)
# KNN Regression using training data.
Training.Rsquared <- c()
for(i in 1:nrow(TrainingData)) {
  KnnModel <- knn.reg(train= TrainingData[,c(2,3,4,5,6,7,8)], y = log(TrainingData[,9]), test=TrainingData[ ,c(2,3,4,5,6,7,8)], k=i)
  
  TrainRSS <- sum((KnnModel$pred - log(TrainingData[,9]))^2)
  TrainTSS <- sum((mean(log(TrainingData[,9]))-log(TrainingData[,9]))^2)
  Training.Rsquared[i] <- 1-TrainRSS/TrainTSS
  
}
which.max(Training.Rsquared)
Training.Rsquared[which.max(Training.Rsquared)]
plot(1:nrow(TrainingData), Training.Rsquared)
Training.Rsquared
# Training R-Squared of 1 at K=1.

# Calculating test R-Squared!
Testing.Rsquared <- 1:nrow(TestingData)
for(i in 1:nrow(TestingData)) {
  KnnModel <- knn.reg(train= TrainingData[,c(2:8)], y = TrainingData[,9], test=TestingData[ ,c(2,3,4,5,6,7,8)], k=i)
  
  TestRSS <- sum((KnnModel$pred - TestingData[,9])^2)
  TestTSS <- sum((mean(TestingData[,9])-TestingData[,9])^2)
  Testing.Rsquared[i] <- 1-TestRSS/TestTSS
  
}
which.max(Testing.Rsquared)
Testing.Rsquared[which.max(Testing.Rsquared)]
plot(1:1:nrow(TestingData), Testing.Rsquared)
# 62% variability explained!!!

# Tried to convert Type variable into integer. But it doen't make sense. Infact KNN is performing well 
# without Type.

# Calculating the KNN again with standardizing the predictors.
library(FNN)
set.seed(1)
Standardized.Snails <- scale(Snails[,c(-1)])
Standardized.Test <- scale(true_test[, c(-1)])
trainingindex<- sample(1:nrow(Standardized.Snails), 95*nrow(Standardized.Snails)/100,replace=F)

TrainingData<- Standardized.Snails[trainingindex,]
TestingData <- Standardized.Snails[-trainingindex,]

Testing.Rsquared <- 1:nrow(TestingData)
for(i in 1:nrow(TestingData)) {
  KnnModel <- knn.reg(train= TrainingData[,c(1,2,3,4,5,6,7)], y = TrainingData[,8], test=TestingData[ ,c(1,2,3,4,5,6,7)], k=i)
  
  TestRSS <- sum((KnnModel$pred - TestingData[,8])^2)
  TestTSS <- sum((mean(TestingData[,8])-TestingData[,8])^2)
  Testing.Rsquared[i] <- 1-TestRSS/TestTSS
  
}

which.max(Testing.Rsquared)
Testing.Rsquared[which.max(Testing.Rsquared)]
par(mfrow=c(1,1))
plot(1:1:nrow(TestingData), Testing.Rsquared)
#63.6% Variability explained!!!















