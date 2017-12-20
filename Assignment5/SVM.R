###########################################################
######### Please fill the ??? with proper description (atleast 130 charaters for each)
######### for SVM function try different values to achieve better results


# loading neccessary packages and dataset
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit


#Display the dataset GermanCredit with the help of str() function. This function displays the
#structure of the dataset with 1000 observations of 62 variables in a compact manner. 
#The first seven variables/fields of the dataset are scaled and the first 7 fields of the dataframe 
#are rewritten with the scaled values. Once again the str() function is applied to display the scaled 
#values alongwith the values of other fields.

str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)

#The sample function will pick up 200 random number/observations between 1 and 1000 inclusive
#and store it in sample_index. The 200 numbers picked up is selected as the test dataset 
#and the remaining 800 observations form the training dataset.
sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
train_dateset = dataset[-sample_index,]

#substituting cost as 8 and gamma as 1 to determine SVM for linear kernel. 
#The number of support vectors generated is 406.Higher the value of gamma closer the range 
model = svm(Class ~ ., kernel = "linear", cost = 150, gamma = 1, data = train_dateset, scale = F)
summary(model)

#substituting cost as 4 and gamma as 1 to determine SVM for radial kernel. 
#The number of support vectors generated is 800.
model = svm(Class ~ ., kernel = "radial", cost = 0.5, gamma = 0.25, data = train_dateset, scale = F)
summary(model)

#The predict function is used to get the predictions for the test_dataset
#display the summary of the prediction
predictions <-  predict(model, test_dateset[-10])
summary(predictions)


#display the predictions in the form of a table/matrix called confusion matrix
table(test_dateset[,10], predictions)



