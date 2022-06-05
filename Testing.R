# Set working directory and importing data.
# If the data is in the excel format (xlsx) convert it into txt format.

Snails <- read.table("Snails.txt", header = TRUE)
attach(Snails)

# Suggested Model
FinalModel_LR <- lm(log(Rings) ~ log(I(ShellWeight)^6) + poly(ShuckedWeight,6) +poly(Diameter,3) + 
                      Type*poly(LongestShell,3) + Height + poly(WholeWeight,9) + poly(VisceraWeight,4) + 
                      WholeWeight:VisceraWeight , data = Snails)

summary(FinalModel_LR)
# Prediction on un seen TestSnails data.

test.pred <- exp(predict(FinalModel_LR, TestSnails))
test.RSS <- sum((test.pred - TestSnails[,"Rings"])^2)
test.TSS <- sum((mean(TestSnails[,"Rings"]) - TestSnails[,"Rings"])^2)
test.R.Squared<- 1-test.RSS/test.TSS
test.R.Squared

