#First we need to read the csv file and assign it to data.
prudentialData = read.csv('C:\\Users\\Neola\\Downloads\\train.csv',header = TRUE)
data <- prudentialData

#Removing the columns where the mssing data is dominant in the entire column
data <- data[,-c(1,38,48,53,62,70)]

#Treating missing data and filling it up with mean
data$Employment_Info_1 <- ifelse(is.na(data$Employment_Info_1), ave(data$Employment_Info_1, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Employment_Info_1)
data$Employment_Info_4 <- ifelse(is.na(data$Employment_Info_4), ave(data$Employment_Info_4, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Employment_Info_4)
data$Employment_Info_6 <- ifelse(is.na(data$Employment_Info_6), ave(data$Employment_Info_6, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Employment_Info_6)
data$Insurance_History_5 <- ifelse(is.na(data$Insurance_History_5), ave(data$Insurance_History_5,
                                                                    FUN=function(x) mean(x, na.rm = TRUE)),data$Insurance_History_5)
data$Family_Hist_2 <- ifelse(is.na(data$Family_Hist_2), ave(data$Family_Hist_2, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_2)
data$Family_Hist_3 <- ifelse(is.na(data$Family_Hist_3), ave(data$Family_Hist_3, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_3)
data$Family_Hist_4 <- ifelse(is.na(data$Family_Hist_4), ave(data$Family_Hist_4, 
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Family_Hist_4)
data$Medical_History_1 <- ifelse(is.na(data$Medical_History_1), ave(data$Medical_History_1,
                                                                    FUN=function(x) mean(x, na.rm = TRUE)), data$Medical_History_1)


#1-C conversion
#convert the categorical columns, since these categories are not just
#numeric coeffcients, we need to divide these into separate columns.
#the function is applicable for all categorical columns. Since we have large
#data, practically it is not possible to convert levels to labels for every column.

categoricalfunc <- function(data, Columnname){
  for(level in unique(data[[Columnname]])){
    data[paste(Columnname,seq = "_",level)]<- ifelse(data[[Columnname]] == level,1,0)
  }
  return(subset(data,select = -get(Columnname)))
}

#After creating the function, we need to apply this function to category columns.

data <- categoricalfunc(data, "Product_Info_1")
data <- categoricalfunc(data, "Product_Info_2")
data <- categoricalfunc(data, "Product_Info_5")
data <- categoricalfunc(data, "Product_Info_6")
data <- categoricalfunc(data, "Product_Info_7")
data <- categoricalfunc(data, "Employment_Info_3")
data <- categoricalfunc(data, "Employment_Info_5")
data <- categoricalfunc(data, "InsuredInfo_1")
data <- categoricalfunc(data, "InsuredInfo_2")
data <- categoricalfunc(data, "InsuredInfo_3")
data <- categoricalfunc(data, "InsuredInfo_4")
data <- categoricalfunc(data, "InsuredInfo_5")
data <- categoricalfunc(data, "InsuredInfo_6")
data <- categoricalfunc(data, "InsuredInfo_7")
data <- categoricalfunc(data, "Insurance_History_1")
data <- categoricalfunc(data, "Insurance_History_2")
data <- categoricalfunc(data, "Insurance_History_3")
data <- categoricalfunc(data, "Insurance_History_4")
data <- categoricalfunc(data, "Insurance_History_7")
data <- categoricalfunc(data, "Insurance_History_8")
data <- categoricalfunc(data, "Insurance_History_9")
data <- categoricalfunc(data, "Family_Hist_1")
data <- categoricalfunc(data, "Medical_History_3")
data <- categoricalfunc(data, "Medical_History_4")
data <- categoricalfunc(data, "Medical_History_5")
data <- categoricalfunc(data, "Medical_History_6")
data <- categoricalfunc(data, "Medical_History_7")
data <- categoricalfunc(data, "Medical_History_8")
data <- categoricalfunc(data, "Medical_History_9")
data <- categoricalfunc(data, "Medical_History_11")
data <- categoricalfunc(data, "Medical_History_12")
data <- categoricalfunc(data, "Medical_History_13")
data <- categoricalfunc(data, "Medical_History_14")
data <- categoricalfunc(data, "Medical_History_16")
data <- categoricalfunc(data, "Medical_History_17")
data <- categoricalfunc(data, "Medical_History_18")
data <- categoricalfunc(data, "Medical_History_19")
data <- categoricalfunc(data, "Medical_History_20")
data <- categoricalfunc(data, "Medical_History_21")
data <- categoricalfunc(data, "Medical_History_22")
data <- categoricalfunc(data, "Medical_History_23")
data <- categoricalfunc(data, "Medical_History_25")
data <- categoricalfunc(data, "Medical_History_26")
data <- categoricalfunc(data, "Medical_History_27")
data <- categoricalfunc(data, "Medical_History_28")
data <- categoricalfunc(data, "Medical_History_29")
data <- categoricalfunc(data, "Medical_History_30")
data <- categoricalfunc(data, "Medical_History_31")
data <- categoricalfunc(data, "Medical_History_33")
data <- categoricalfunc(data, "Medical_History_34")
data <- categoricalfunc(data, "Medical_History_35")
data <- categoricalfunc(data, "Medical_History_36")
data <- categoricalfunc(data, "Medical_History_37")
data <- categoricalfunc(data, "Medical_History_38")
data <- categoricalfunc(data, "Medical_History_39")
data <- categoricalfunc(data, "Medical_History_40")
data <- categoricalfunc(data, "Medical_History_41")

summary(data[,121:180])

#Taking the first 50000 rows
Finaldata <- data[-(50001:59382)]

write.csv(Finaldata, file = "C:\\Users\\Neola\\Downloads\\Prudential_Cleaned.csv", row.names = FALSE)


#Reading the data from the preprocessed file
data <- read.csv("C:\\Users\\Neola\\Downloads\\Prudential_Cleaned.csv")

##Dividing our Dataset into Train Data & Test Data
set.seed(150)

index <- sample(1:nrow(data),round(0.75*nrow(data)))

train_ <- data[1:200,]
test_ <- data[500:700,]

#1-C conversion of Response
Response1 <- as.numeric(train_$Response == '1')
Response2 <- as.numeric(train_$Response == '2')
Response3 <- as.numeric(train_$Response == '3')
Response4 <- as.numeric(train_$Response == '4')
Response5 <- as.numeric(train_$Response == '5')
Response6 <- as.numeric(train_$Response == '6')
Response7 <- as.numeric(train_$Response == '7')
Response8 <- as.numeric(train_$Response == '8')

train_ <- data.frame(train_,Response1,Response2,Response3,Response4,Response5,Response6,Response7,Response8)

write.csv(train_, file = "C:\\Users\\Neola\\Downloads\\PrudentialTrainResponse.csv", row.names = FALSE)


##Neural Networks
#install.packages("neuralnet")
library(neuralnet)

#Train the dataset for neural networks
model <- neuralnet(Response1+Response2+Response3+Response4+Response5+Response6+Response7+Response8 ~ 
                     Product_Info_4 + BMI + Medical_History_1 + Medical_Keyword_3 + Medical_Keyword_9 + Medical_Keyword_25
                   + Medical_Keyword_38 + Medical_Keyword_41 + Medical_Keyword_45 + Employment_Info_3._.1 + InsuredInfo_2._.2 
                   + InsuredInfo_5._.1 + InsuredInfo_6._.2 + InsuredInfo_7._.1 + Medical_History_4._.1 + Medical_History_5._.1 
                   + Medical_History_11._.3 + Medical_History_11._.1 + Medical_History_13._.3 + Medical_History_17._.3 
                   + Medical_History_20._.2 + Medical_History_21._.2 + Medical_History_23._.3 + Medical_History_27._.3 
                   + Medical_History_28._.1 + Medical_History_29._.3 + Medical_History_30._.2 + Medical_History_35._.1 
                   + Medical_History_39._.3 + Medical_History_40._.3, data = train_, hidden = c(8,8), linear.output = FALSE)

## plot the NN
plot(model, rep = "best")

## Test the resulting output
test_ <- subset(test_, select = c(Product_Info_4,BMI,Medical_History_1,
                                       Medical_Keyword_3,Medical_Keyword_9,Medical_Keyword_25,
                                        Medical_Keyword_38,Medical_Keyword_41,Medical_Keyword_45,
                                        Employment_Info_3._.1,InsuredInfo_2._.2,InsuredInfo_5._.1,InsuredInfo_6._.2,InsuredInfo_7._.1,
                                        Medical_History_4._.1,Medical_History_5._.1,Medical_History_11._.3,Medical_History_11._.1,
                                        Medical_History_13._.3,Medical_History_17._.3,Medical_History_20._.2,Medical_History_21._.2,
                                        Medical_History_23._.3,Medical_History_27._.3,Medical_History_28._.1,Medical_History_29._.3,
                                        Medical_History_30._.2,Medical_History_35._.1,
                                        Medical_History_39._.3,Medical_History_40._.3,Response,
                                        Response1,Response2,Response3,Response4,Response5,Response6,Response7,Response8))

#Compute Predictions of Test Set
predicted.nn.values <- compute(model,test_[,1:30])
predicted.nn.values

idx <- apply(predicted.nn.values$net.result, 1, which.max)
pred <- c(1,2,3,4,5,6,7,8)[idx]
table(pred,test_$Response)




