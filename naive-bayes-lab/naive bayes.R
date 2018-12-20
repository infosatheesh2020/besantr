#naive Bayes

#cleaning your enviornment
rm(list = ls(all=T))

#setting working directory
setwd("C:/Users/10891/Downloads/Mukil/Besant/Codes")

#importing data
Bank = read.table("logistic.txt",sep = ";",header = T)

##Recode the levels for the target
Bank$outcome <- ifelse(Bank$y=="yes",1,0)
Bank$outcome <- as.factor(as.character(Bank$outcome))
Bank <- Bank[,-17]

##To check the number of missing values
sum(is.na(Bank))

#checking the structure of data, 
# to make sure all are categorical column, since naive can't accept numeric data_type
str(Bank)

#removing/converting data types to categorical
Bank$age<-as.factor(as.character(Bank$age))
Bank$balance<-NULL
Bank$day<-as.factor(as.character(Bank$day))
Bank$duration <- NULL
Bank$campaign<-as.factor(as.character(Bank$campaign))
Bank$pdays<-NULL
Bank$previous<-as.factor(as.character(Bank$previous))

#lets look the summary to know the complete data idea
summary(Bank)

#model building
library(naivebayes)
Naive_model = naive_bayes(x = Bank[,-14],y = Bank$outcome)

#predicting on the same data
my_predictions = predict(Naive_model,Bank[,-14])
summary(my_predictions)

#comparing results
comparison = cbind(Actual = Bank$outcome,Predicted = my_predictions)
View(comparison)

#building confusion matrix
table(Actual = Bank$outcome,Predicted = my_predictions)

#looking for error metrics
library(MLmetrics)
recall_value = Recall(y_true = Bank$outcome, y_pred = my_predictions)
recall_value

precision_value = Precision(y_true = Bank$outcome, y_pred = my_predictions)
precision_value

f1_score_value = F1_Score(y_true = Bank$outcome, y_pred = my_predictions)
f1_score_value

specificity_value = Specificity(y_true = Bank$outcome, y_pred = my_predictions)
specificity_value
