#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer') 

setwd("C:/Neola/ADS/Assignment3/Q2/b")
library(xlsx)
energyEff <- read.xlsx("C:/Neola/ADS/Assignment3/Q2/b/Energy Efficiency ENB2012_data.xlsx",sheetIndex = 1)

library(rpart)
fit <- rpart(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,
             data=energyEff, method="anova")
printcp(fit)
plotcp(fit)
summary(fit)

par(mfrow=c(1,2))
rsq.rpart(fit)

plot(fit, uniform=TRUE, 
     main="Regression Tree for Energy Efficiency ",margin = 0.2)
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/Neola/ADS/Assignment3/Q2/b/tree2.ps", 
     title = "Regression Tree for Energy Efficiency ")

fity2 <- rpart(Y2 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,
             data=energyEff, method="anova")
printcp(fity2)
plotcp(fity2)
summary(fity2)

par(mfrow=c(1,2))
rsq.rpart(fity2)

plot(fity2, uniform=TRUE, 
     main="Regression Tree for Energy Efficiency ",margin = 0.2)
text(fity2, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fity2, file = "c:/Neola/ADS/Assignment3/Q2/b/tree1.ps", 
     title = "Regression Tree for Energy Efficiency ")


