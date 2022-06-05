library("readxl")
Snails <- as.data.frame(read_excel("Snails.xlsx"))


########## Examples:
FinalModel_LR <- lm(log(Rings) ~ log(I(ShellWeight)^6) + poly(ShuckedWeight,6) +poly(Diameter,3) + 
                      Type*poly(LongestShell,3) + Height + poly(WholeWeight,9) + poly(VisceraWeight,4) + 
                      WholeWeight:VisceraWeight , data = Snails)
#assume this is your best model
summary(FinalModel_LR)
#computes testing R squared for the data set
#TestSnails

test.pred <- exp(predict(FinalModel_LR, true_test))
test.RSS <- sum((test.pred - true_test[,"Rings"])^2)
test.TSS <- sum((mean(true_test[,"Rings"]) - true_test[,"Rings"])^2)
test.R.Squared<- 1-test.RSS/test.TSS
test.R.Squared




















########## Examples:
fit<- lm(Rings ~ LongestShell+I(LongestShell^2), data=Snails)

test.pred <- predict(fit, TestSnails)
test.RSS <- sum((test.pred - TestSnails[,"Rings"])^2)
test.TSS <- sum((mean(TestSnails[,"Rings"]) - TestSnails[,"Rings"])^2)
test.R.Squared<- 1-test.RSS/test.TSS
test.R.Squared



########## Not Okay:
attach(Snails)
LongestShellSquared<-LongestShell^2

fit<- lm(Rings ~ LongestShell+LongestShellSquared)

test.pred <- predict(fit, TestSnails)
test.RSS <- sum((test.pred - TestSnails[,"Rings"])^2)
test.TSS <- sum((mean(TestSnails[,"Rings"]) - TestSnails[,"Rings"])^2)
test.R.Squared<- 1-test.RSS/test.TSS
test.R.Squared
