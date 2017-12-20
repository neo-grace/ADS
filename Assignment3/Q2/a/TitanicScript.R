
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer') 

setwd("C:/Neola/ADS/Assignment3/Q2/a")

#reading train and test data
library(readr)
train <- read_csv("C:/Neola/ADS/Assignment3/Q2/a/Titanic train.csv")
test <- read_csv("C:/Neola/ADS/Assignment3/Q2/a/Titanic test.csv")

#converting trining data to decision tree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method="class")
printcp(fit)
plotcp(fit)
plot(fit,margin = 0.2)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

#plot the decision tree
fancyRpartPlot(fit)

#Pruned tree
ptree <- prune(fit,
               cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,
               main="Pruned Classification Tree")


#Predict survived
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction, PClass = test$Pclass,
                     Sex = test$Sex, Age = test$Age, SibSp = test$SibSp, Parch = test$Parch, Fare = test$Fare,
                     Embarked = test$Embarked)
submitFit <- rpart(Survived ~ PClass + Sex + Age + SibSp + Parch + Fare + Embarked,
                   data=submit, method="class")
printcp(submitFit)
plotcp(submitFit)

write.csv(submit, file = "survivedResult.csv", row.names = FALSE)









