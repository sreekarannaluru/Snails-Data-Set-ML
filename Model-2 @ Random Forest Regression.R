# Performing Bagging.
install.packages("randomForest")
library(randomForest)
set.seed(1)
trainingindex<- sample(1:nrow(Snails), 95*nrow(Snails)/100,replace=F)

TrainingData<- Snails[trainingindex,]
TestingData <- Snails[-trainingindex,]
bag.Snails <- randomForest(log(Rings) ~ ShellWeight + ShuckedWeight +  Diameter + 
                             Type + LongestShell + Height + WholeWeight+VisceraWeight ,
                           data = Snails,  mtry = 8, importance = TRUE, subset = trainingindex )
bag.Snails

# Converted log(rings) to exponential!
BagPred <- exp(predict(bag.Snails, newdata = Snails[-trainingindex,]))
TestRSS <- sum((BagPred - TestingData[,9])^2)
TestTSS <- sum((mean(TestingData[,9])- TestingData[,9])^2)
1-TestRSS/TestTSS 
# 59.7% Variability explained!!!

par(mfrow = c(1,1))
plot(BagPred, TestingData[,9])
abline(0,1)

# Let's Perform Random Forest
set.seed(1)
rf.Snails <- randomForest(log(Rings) ~ ShellWeight + ShuckedWeight +  Diameter + 
                            Type + LongestShell + Height + WholeWeight +VisceraWeight ,
                          data = Snails,  mtry = 2, importance = TRUE, subset = trainingindex )

rf.Snails

RFPred <- exp(predict(rf.Snails, newdata = Snails[-trainingindex,]))
TestRSS <- sum((RFPred - TestingData[,9])^2)
TestTSS <- sum((mean(TestingData[,9])- TestingData[,9])^2)
1-TestRSS/TestTSS 
# 59 % Test R-Squared.

plot(RFPred, TestingData[,9])
abline(0,1)

importance(rf.Snails)
varImpPlot(rf.Snails)


# Performing Boosting
install.packages('gbm')
library(gbm)
set.seed(1)
boost.Snails <- gbm(log(Rings) ~ .-Type, data = TrainingData, distribution = "gaussian", n.trees = 5000, interaction.depth = 4 )
summary(boost.Snails)

BoostPred <- exp(predict(boost.Snails, newdata = TestingData, n.trees = 5000))
TestRSS <- sum((BoostPred - TestingData[,9])^2)
TestTSS <- sum((mean(TestingData[,9])- TestingData[,9])^2)
1-TestRSS/TestTSS
# Only 51% of variability explained!!!

plot(RFPred, TestingData[,9])
abline(0,1)



### Comparing the above three ensemble methods I am picking Random Forests as my third model. 
### Note that Boosting is falling pretty short of RF!!!

# Let's Perform BART! Just Playing around!
install.packages('BART')
library(BART)
x <- Snails[, 2:8]
y <- Snails[, 'Rings']
xtrain <- x[trainingindex, ]
ytrain<- y[trainingindex]
xtest <- x[-trainingindex,]
ytest <- y[-trainingindex]
set.seed(2021)
bart.fit <- gbart(xtrain, ytrain, x.test = xtest)
bart.fit$yhat.test.mean
BARTPred<- bart.fit$yhat.test.mean
TestRSS <- sum((BARTPred - ytest)^2)
TestTSS <- sum((mean(ytest)- ytest)^2)
1-TestRSS/TestTSS 
## Testing R-Squared is around 55% 









