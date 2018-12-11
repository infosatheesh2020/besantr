setwd("C:/Users/10891/Downloads/Mukil/Besant")
bdata = read.csv("Bankruptcy_data.csv")
library(DMwR)

ci_data = centralImputation(bdata)
sum(is.na(ci_data))
str(ci_data)
ci_data$target<- as.factor(as.character(ci_data$target))
str(ci_data)
summary(ci_data$target)
1765/34788
log_bdata = glm(formula = target~.,data = ci_data,family = binomial)
prediction_bdata = predict(log_bdata,ci_data,type = 'response')
View(round(prediction_bdata,3))
log_output = ifelse(prediction_bdata>0.0507,1,0)
log_output<- as.factor(as.character(log_output))
summary(log_output)
library(MLmetrics)
Recall(ci_data$target,log_output)
Precision(ci_data$target,log_output)
F1_Score(ci_data$target,log_output)

log_output = ifelse(prediction_bdata>0.05,1,0)
log_output<- as.factor(as.character(log_output))
summary(log_output)
library(MLmetrics)
Recall(ci_data$target,log_output)
Precision(ci_data$target,log_output)
F1_Score(ci_data$target,log_output)
confusionMatrix(ci_data$target,log_output,positive = '1')


# Predicting on the train data
predicted <- predict(log_bdata,type="response")

#Getting the probability object that has all the information-tpr,fpr,auc etc.
library(ROCR)
prob <- prediction(predicted,ci_data$target)

# Getting the true positive rate and false negative rate
tprfpr <- performance(prob, "tpr", "fpr")

# Plotting the true positive rate and false negative rate based on the threshold value
plot(tprfpr,col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
