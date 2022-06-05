# Importing the data from the computer into R.
Snails <- read.table("Snails.txt", header = TRUE)
head(Snails)
attach(Snails)
View(Snails) # Viewing the table
summary(Snails)

# Performing Principal Component Analysis.
Preds <- row.names(Snails)
names(Snails)
apply(Snails[,-1], 2, mean) # All predictors have similar mean, I mean in the same range. Not widely spread.
pr.out <- prcomp(Snails[,-1], scale. =  TRUE)
pr.out$rotation
biplot(pr.out, scale = 0)


# Plotting the predictors
library(corrplot)

# Without Rings
SnailsData <- data.frame(Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight)
corrplot(cor(SnailsData))
cor(SnailsData)
pairs(Snails[,2:8], pch = 20)

#With Rings
SnailsDataRings <- data.frame(Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight, Rings)
corrplot(cor(SnailsData))
cor(SnailsData)
pairs(Snails[,2:9], pch = 20)

# Investigating Height =0
Snails[Snails$Height == 0,]

# Including Weight Difference
Snails.WDiff <- Snails$WholeWeight - (Snails$ShuckedWeight + Snails$VisceraWeight + Snails$ShellWeight)
dim(Snails[Snails.WDiff < 0,])
dim(Snails[Snails.WDiff > 0,])
dim(Snails[Snails.WDiff == 0,])
Snails[Snails.WDiff < 0,]

# This R-File is like the Pre-Processing! Somewhat similar to Exploratory Data Analysis. #
