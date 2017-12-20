library(xlsx)
chemicalData <- read.xlsx("C:/Users/Neola/Downloads/Chemical Process Data.xlsx",sheetIndex = 1)

x <- data.frame(1,chemicalData$Factor_1,chemicalData$Factor_2)
x

#Part 1
chemicalData.fit <- lm(chemicalData$Yield ~ chemicalData$Factor_1 + chemicalData$Factor_2, data = x)
summary(chemicalData.fit)

#Part 2
xmatrix <- as.matrix(x)
xmatrix

ymatrix <- matrix(chemicalData$Yield)
ymatrix

#Beta claculation
b <- solve(t(xmatrix) %*% xmatrix) %*% t(xmatrix) %*% ymatrix
b

#rows and columns
n <- 17

#Hat Matrix
H <- xmatrix %*% solve(t(xmatrix) %*% xmatrix) %*% t(xmatrix)
H

#SSR calculation
J <- matrix(1, nrow = 17, ncol = 17)
JM <- J/17
mid <- H-JM

#SSR (Regression Sum of squares)
SSR <- t(ymatrix) %*% mid %*% ymatrix
SSR

#MSR (Regression Mean Square)
#Degree of freedom = 2 since two predictors
dof <- 2
MSR <- SSR/dof
MSR

#SSE (Error Sum of squares)
I <- diag(n)
SSE <- t(ymatrix) %*% (I-H) %*% ymatrix
SSE

#MSE (Error Mean Square)
MSE <- SSE/(n-(dof+1))
MSE

F0 <- MSR/MSE
F0













