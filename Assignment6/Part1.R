
library(xlsx)
data <- read.xlsx("C:/Users/Neola/Downloads/loan.xlsx",sheetIndex = 1)

#check if any missing data
apply(data,2,function(x) sum(is.na(x)))

#index to partition data as test ad train
index <- sample(1:nrow(data),round(0.75*nrow(data)))

#Converting to numeric
Res_status = as.numeric(data$Res_status)-1
Decision = as.numeric(data$Decision)-1
Liab_ref = as.numeric(data$Liab_ref)-1
Acc_ref = as.numeric(data$Acc_ref)-1
Occupation = as.numeric(data$Occupation)-1
Job_status = as.numeric(data$Job_status)-1

data = data.frame(Res_status,Liab_ref,Acc_ref,Occupation,Job_status,Decision)
print(head(data,2))

library(neuralnet)
library(caTools)
set.seed(100)

# Split based on index
train_ <- data[index,]
test_ <- data[-index,]

#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(Decision ~ Res_status + Liab_ref + Acc_ref + Occupation + Job_status
                ,data=train_,hidden=c(2,2),linear.output=FALSE)

plot(nn)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test_[1:5])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test_$Decision,predicted.nn.values$net.result)

