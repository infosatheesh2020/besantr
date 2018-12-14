# Clear global environment variables
rm(list=ls(all=TRUE))

# Set working directory
setwd("C:/Users/10891/Downloads/mukil/Besant")

# Read train and test data
train_data=read.csv(file = "Bankruptcy_data.csv",header = T)

#Check summary and convert to appropraite data types
# Drop the variable ID as it does not give any information
str(train_data)
summary(train_data)
train_data$ID=NULL

train_data$target=as.factor(as.character(train_data$target))
str(train_data)
summary(train_data$target)

# Check NA values
sum(is.na(train_data))

# KNN imputation for train data : done separately for each class
library(DMwR)

#Split data based on target variable
train_data_0=train_data[train_data$target==0,]
train_data_1=train_data[train_data$target==1,]

train_data_0_knn=centralImputation(data = train_data_0)
sum(is.na(train_data_0_knn))

train_data_1_knn=centralImputation(data = train_data_1)
sum(is.na(train_data_1_knn))

# combine imputed data
train_data_knn=rbind(train_data_0_knn,train_data_1_knn)

# Split the train data into train and validation sets
install.packages("ddalpha")
library(caret)
rows=createDataPartition(y = train_data_knn$target,p=0.7,list=FALSE)
train_split=train_data_knn[rows,]
val_split=train_data_knn[-rows,]


## Build model : RANDOM FOREST

library(randomForest)
model = randomForest(target ~ ., data=train_split, 
                     keep.forest=TRUE,do.trace = TRUE, ntree=50) 
table(train_split$target)
print(model)

# Predict on Train data 
pred_Train = predict(model,train_split[,-65],type="response",norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Train = table("actual" = train_split$target, "predicted" = pred_Train)
confusionMatrix(data = pred_Train,reference = train_split$target,positive = "1")
sensitivity_Train <- cm_Train[2, 2]/sum(cm_Train[2, ])
specificity_Train <- cm_Train[1, 1]/sum(cm_Train[1, ])
accruacy_Train = sum(diag(cm_Train))/sum(cm_Train)
precision_Train= cm_Train[2, 2]/sum(cm_Train[,2])
F1_Train=2*precision_Train*sensitivity_Train/(precision_Train+sensitivity_Train)


# Predicton on Validation Data
pred_Val = predict(model, val_split[-65],type="response",norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Val = table("actual" = val_split$target, "predicted" = pred_Val);
confusionMatrix(data = pred_Val,reference = val_split$target,positive = "1")
sensitivity_Val <- cm_Val[2, 2]/sum(cm_Val[2, ])
specificity_Val <- cm_Val[1, 1]/sum(cm_Val[1, ])
accruacy_Val = sum(diag(cm_Val))/sum(cm_Val)
precision_Val= cm_Val[2, 2]/sum(cm_Val[,2])
F1_Val=2*precision_Val*sensitivity_Val/(precision_Val+sensitivity_Val)


# Important attributes
model$importance  
round(importance(model), 2)   

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])

colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

# plot (directly prints the important attributes) 
varImpPlot(model)


# Build randorm forest using top 5 important attributes. 
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:5])

# Build the classification model using randomForest
model_Imp = randomForest(target~.,
                         data=train_split[,c(top_Imp_Attr,"target")], 
                         keep.forest=TRUE,ntree=50,do.trace = TRUE) 

# Print and understand the model
print(model_Imp)

# Important attributes
model_Imp$importance  

# Predict on Train data 
pred_Train = predict(model_Imp, 
                     train_split[,setdiff(names(train_split), "target")],
                     type="response", 
                     norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Train = table("actual" = train_split$target, "predicted" = pred_Train);
confusionMatrix(data = pred_Train,reference = train_split$target,positive = "1")
sensitivity_Train <- cm_Train[2, 2]/sum(cm_Train[2, ])
specificity_Train <- cm_Train[1, 1]/sum(cm_Train[1, ])
accruacy_Train = sum(diag(cm_Train))/sum(cm_Train)
precision_Train= cm_Train[2, 2]/sum(cm_Train[,2])
F1_Train=2*precision_Train*sensitivity_Train/(precision_Train+sensitivity_Train)


# Predicton Val Data
pred_Val = predict(model_Imp, val_split[,setdiff(names(val_split),
                                                 "target")],
                   type="response", 
                   norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Val = table("actual" = val_split$target, "predicted" = pred_Val);
confusionMatrix(data = pred_Val,reference = val_split$target,positive = "1")
sensitivity_Val <- cm_Val[2, 2]/sum(cm_Val[2, ])
specificity_Val <- cm_Val[1, 1]/sum(cm_Val[1, ])
accruacy_Val = sum(diag(cm_Val))/sum(cm_Val)
precision_Val= cm_Val[2, 2]/sum(cm_Val[,2])
F1_Val=2*precision_Val*sensitivity_Val/(precision_Val+sensitivity_Val)


install.packages("C50")
library(C50)
c5_model = C5.0(x = train_data_knn[,-65],y = train_data_knn[,65])
summary(c5_model)  

c5_pred = predict(c5_model,train_data_knn[,-65])
c5_cm = table(c5_pred,train_data_knn$target)
library(MLmetrics)
Recall(y_true = train_data_knn$target,y_pred = c5_pred)
F1_Score(y_true = train_data_knn$target,y_pred = c5_pred)

c5_pred_val = predict(object = c5_model,val_split[,-65])
c5_val_cm = table(c5_pred_val,val_split$target)
Recall(y_true = val_split$target,y_pred = c5_pred_val)
F1_Score(y_true = val_split$target,y_pred = c5_pred_val)


install.packages("rpart")
library(rpart)
cart_model = rpart(train_data_knn$target~.,data = train_data_knn)
summary(cart_model)
cart_pred = predict(cart_model,train_data_knn[,-65],type = "class")
cart_cm = table(cart_pred,train_data_knn$target)
library(MLmetrics)
Recall(y_true = train_data_knn$target,y_pred = cart_pred)
F1_Score(y_true = train_data_knn$target,y_pred = cart_pred)

cart_pred_val = predict(object = cart_model ,val_split[,-65])
cart_val_cm = table(cart_pred_val ,val_split$target)
Recall(y_true = val_split$target,y_pred = cart_pred_val)
F1_Score(y_true = val_split$target,y_pred = cart_pred_val)
