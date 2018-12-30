

## MODEL-1 : RANDOM FORESTS

# Clear global environment variables
rm(list=ls(all=TRUE))

# Set working directory
setwd("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/CUTe 4/20171118_Batch32_BLR_CUTe_CSE7305c")

# Read train and test data
train_data=read.csv(file = "traindata.csv",header = T)
test_data=read.csv(file = "testdata.csv",header = T)

#Check summary and convert to appropraite data types
# Drop the variable ID as it does not give any information
str(train_data)
summary(train_data)
train_data$ID=NULL
test_data$ID=NULL
train_data$target=as.factor(as.character(train_data$target))
str(train_data)

# Check NA values
sum(is.na(train_data))
sum(is.na(test_data))

# KNN imputation for train data : done separately for each class
library(DMwR)
#Split data based on target variable
train_data_0=train_data[train_data$target==0,]
train_data_1=train_data[train_data$target==1,]

train_data_0_knn=knnImputation(data = train_data_0,k=10)
sum(is.na(train_data_0_knn))
train_data_1_knn=knnImputation(data = train_data_1,k=10)
sum(is.na(train_data_1_knn))

# KNN imputation for test data is done using class "1" as recall is more important
test_data_knn=knnImputation(data=test_data,k = 10,distData = train_data_1_knn[,setdiff(names(train_data),"target")])

sum(is.na(test_data_knn))

# combine imputed data
train_data_knn=rbind(train_data_0_knn,train_data_1_knn)

# Check variables where standard deviation =0

sd_train=apply(train_data_knn,2,sd)
sort(sd_train,decreasing = TRUE)

# none of the variables have 0 variables, so none of them are removed

# Split the train data into train and validation sets
library(caret)
rows=createDataPartition(y = train_data_knn$target,p=0.8,list=FALSE)
train_split=train_data_knn[rows,]
val_split=train_data_knn[-rows,]
prop.table(table(train_split$target))
prop.table(table(val_split$target))


## Build model : RANDOM FOREST

library(randomForest)
model = randomForest(target ~ ., data=train_split, 
                     keep.forest=TRUE, ntree=500) 
table(train_split$target)
print(model)

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

# Predict on Train data 
pred_Train = predict(model, 
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
# Predicton Test Data
pred_Val = predict(model, val_split[,setdiff(names(val_split),
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

# Build randorm forest using top 5 important attributes. 
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:5])

# Build the classification model using randomForest
#model_Imp = randomForest(target~.,
#                         data=train_split[,c(top_Imp_Attr,"target")], 
#                         keep.forest=TRUE,ntree=500,mtry=3,cutoff=c(0.5,0.5)) 
model_Imp = randomForest(target~.,
                         data=train_split[,c(top_Imp_Attr,"target")], 
                         keep.forest=TRUE,ntree=500,cutoff=c(0.75,0.25)) 

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

# Prediction of test data
pred_Test = predict(model_Imp, test_data_knn,
                   type="response", 
                   norm.votes=TRUE)
prop.table(table(pred_Test))

write.csv(x = pred_Test,file = "sub2_RFskew.csv")

#Precision=46.7354% and Recall=42.7673%
#Your score is 44.66%
precision_Test=0.46
sensitivity_Test=0.42
F1_Test=2*precision_Test*sensitivity_Test/(precision_Test+sensitivity_Test)

## MODEL-2 : XGBOOST

# Clear global environment variables
rm(list=ls(all=TRUE))

# Set working directory
setwd("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/CUTe 4/20171118_Batch32_BLR_CUTe_CSE7305c")

# Read train and test data
train_data=read.csv(file = "traindata.csv",header = T)
test_data=read.csv(file = "testdata.csv",header = T)

#Check summary and convert to appropraite data types
# Drop the variable ID as it does not give any information
str(train_data)
summary(train_data)
train_data$ID=NULL
test_data$ID=NULL
train_data$target=as.factor(as.character(train_data$target))
str(train_data)

# Check NA values
sum(is.na(train_data))
sum(is.na(test_data))

colSums(is.na(train_data))
colSums(is.na(test_data))
train_data$target=as.factor(as.character(train_data$target))
# Split the train data into train and validation sets
library(caret)
rows=createDataPartition(y = train_data$target,p=0.8,list=FALSE)
train_split=train_data[rows,]
val_split=train_data[-rows,]
prop.table(table(train_split$target))
prop.table(table(val_split$target))

# KNN imputation
train_split_knn=knnImputation(data = train_split[,setdiff(names(train_split),"target")],k=10)
val_split_knn=knnImputation(data=val_split[,setdiff(names(train_split),"target")],k=10,distData = train_split_knn)
test_data_knn=knnImputation(data=test_data[,setdiff(names(train_split),"target")],k=10,distData = train_split_knn)

names(train_split_knn)

# fit the XGBoost model
library(xgboost)

dtrain = xgb.DMatrix(data = as.matrix(train_split_knn),
                     label = as.numeric(train_split$target)-1)

model = xgboost(data = dtrain, max.depth = 2, 
                eta = 1, nthread = 2, nround = 2, 
                objective = "binary:logistic", verbose = 1)
# objective = "binary:logistic": we will train a binary classification model ;
# max.deph = 2: the trees won't be deep, because our case is very simple ;
# nthread = 2: the number of cpu threads we are going to use;
# nround = 2: there will be two passes on the data
# eta = 1: It controls the learning rate
# verbose = 1: print evaluation metric
#Use watchlist parameter. It is a list of xgb.DMatrix, each of them tagged with a name.
dtest = xgb.DMatrix(data = as.matrix(val_split_knn),
                    label = as.numeric(val_split$target)-1)

watchlist = list(train=dtrain, test=dtest)

model = xgb.train(data=dtrain, max.depth=15,
                  eta=0.01, nthread = 2, nround=1000, 
                  watchlist=watchlist,
                  eval.metric = "auc", 
                  objective = "binary:logistic",
                  verbose=1,subsample=0.5,early_stopping_rounds=100,colsample_bytree=1)
# eval.metric allows us to monitor two new metrics for each round, logloss and error.

importance <- xgb.importance(feature_names = names(train_split_knn), model = model)
print(importance)
xgb.plot.importance(importance_matrix = importance)

# predict
prob_train_split <- data.frame(predict(model, as.matrix(train_split_knn)))
prob_val_split <- data.frame(predict(model, as.matrix(val_split_knn)))
prob_test <- data.frame(predict(model, as.matrix(test_data_knn)))

print(head(pred))

library(ROCR)
pred_train <- prediction(prob_train_split,train_split$target)
perf <- performance(pred_train,measure = "tpr", x.measure = "fpr")
plot(perf)

# The numbers we get are probabilities that a datum will be classified as 1. 
# Therefore, we will set the rule that if this probability for a 
# specific datum is > 0.5 then the observation is classified as 1 (or 0 otherwise).

prediction_Train <- as.numeric(prob_train_split > 0.14)
prediction_Val <- as.numeric(prob_val_split > 0.14)
prediction_Test <- as.numeric(prob_test > 0.14)
prop.table(table(prediction_Test))

# Build confusion matrix and find accuracy   
cm_Train = table("actual" = train_split$target, "predicted" = prediction_Train);
confusionMatrix(data = prediction_Train,reference = train_split$target,positive = "1")
sensitivity_Train <- cm_Train[2, 2]/sum(cm_Train[2, ])
specificity_Train <- cm_Train[1, 1]/sum(cm_Train[1, ])
accruacy_Train = sum(diag(cm_Train))/sum(cm_Train)
precision_Train= cm_Train[2, 2]/sum(cm_Train[,2])
F1_Train=2*precision_Train*sensitivity_Train/(precision_Train+sensitivity_Train)

cm_Val = table("actual" = val_split$target, "predicted" = prediction_Val);
confusionMatrix(data = prediction_Val,reference = val_split$target,positive = "1")
sensitivity_Val <- cm_Val[2, 2]/sum(cm_Val[2, ])
specificity_Val <- cm_Val[1, 1]/sum(cm_Val[1, ])
accruacy_Val = sum(diag(cm_Val))/sum(cm_Val)
precision_Val= cm_Val[2, 2]/sum(cm_Val[,2])
F1_Val=2*precision_Val*sensitivity_Val/(precision_Val+sensitivity_Val)

write.csv(x = prediction_Test,file = "XGB.csv")

## This model gives the best result :
## F1 = 62.15%, Precision=69.37984% and Recall=56.28931%

## MODEL-3 : Random Forest : different implementation

#Clear the workspace
rm(list=ls(all=TRUE))

setwd("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/CUTe 4/20171118_Batch32_BLR_CUTe_CSE7305c")

#-----Read train data
train_data = read.csv("traindata.csv")

#-----Understand the data
str(train_data)
summary(train_data)

#----Drop ID as this is not adding any value for modelling
train_data$ID = NULL

#-----Drop columns with more than 20* missing values
train_data1 = train_data[,!( colSums(is.na(train_data)) >  nrow(train_data) * 0.2  )]

library(DMwR)
#-----Drop rows with more than 20% missing values
train_data2 = train_data[-manyNAs(train_data, 0.2),] 

sum(is.na(train_data2))

#-----Convert target variable to categorical
train_data2$target = as.factor(as.numeric(train_data2$target))
#-----Impute missing values
imputed_train = knnImputation(data = train_data2, k = 5)

sum(is.na(imputed_train))

#-----Read test data
test_data = read.csv("testdata.csv")
sum(is.na(test_data))
imputed_test = knnImputation(data = test_data, k = 5)
#----Drop ID as this is not adding any value for modelling
test_data$ID = NULL

library(ROSE)

#Due to lot of imbalance in the class , balanced it using ovun.sample function
data_balanced_both <- ovun.sample(target ~ ., data = imputed_train, method = "both", p=0.5,N=36372, seed = 1)$data
table(data_balanced_both$target)


# Createa model using Random Forest
model = randomForest(target ~ ., data=data_balanced_both, 
                     keep.forest=TRUE, ntree=500) 
print(model)

#Plot the graph on the created model to check the important attributes
varImpPlot(model)

model$importance 
round(importance(model), 2) 

# Predict on the trained data
pred_Train = predict(model, 
                     data_balanced_both[,setdiff(names(data_balanced_both), "target")],
                     type="response", 
                     norm.votes=TRUE)

#Predict on the Test Data
pred_Test = predict(model, 
                    imputed_test[,setdiff(names(imputed_test), "target")],
                    type="response", 
                    norm.votes=TRUE)


write.csv(pred_Test, file = "predictionat4.csv")

## Result : F1_score = 38%, precision=57%, recall=28%

## MODEL - 4 : XG BOOST : different approach

rm(list = ls(all=TRUE))
library(dplyr)
setwd("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/CUTe 4/20171118_Batch32_BLR_CUTe_CSE7305c")

data1 <- read.csv("traindata.csv",header = T)
str(data1)
summary(data1)
data1$ID <- NULL
sum(is.na(data1))
colSums(is.na(data1))
# ATTR37 have more than 20% of NA values, Hence removing Attr37 from data
data1$Attr37 <- NULL
data1$target <- as.factor(as.character(data1$target))
sum(is.na(data1))


library(DMwR)
manyNAs(data = data1,nORp = 0.1)
nonadata <- data1[-manyNAs(data = data1,nORp = 0.1),]
nonadata1 <- knnImputation(data = nonadata,k = 5)
sum(is.na(nonadata1))
View(nonadata1)

write.csv(x = nonadata1,row.names = F,file = "Nonadata.csv")
data <- read.csv(file = "Nonadata.csv",header = T)
str(data)
data$target <- as.factor(as.character(data$target))

library(xgboost)
#converting factor to numeric
# data$target <- (as.numeric(data$target)-1)

dtrain = xgb.DMatrix(data = as.matrix(datawot1),label = data$target)

model = xgboost(data = dtrain, max.depth = 10, 
                eta = 0.02, nthread = 2, nround = 300, 
                objective = "binary:logistic", verbose = 1)

# datawot <- data[,-which(names(data) %in% c("target"))]
datawot1 <- data[,which(names(data) %in% c("Attr2","Attr15","Attr32","Attr33","Attr34","Attr40",
                                           "Attr42","Attr51","Attr53","Attr54","Attr56","Attr64"))]

str(datawot1)

# model <- ada(x = datawot,y = data$target, iter=10, loss="logistic")
summary(model)
prob_Train <- predict(model, as.matrix(datawot))
pred_Train <- ifelse(prob_Train>0.2,1,0)
write.csv(x = pred_Train,file = "prediction_Train.csv",row.names = F)

# Building confusion matrix and find accuracy   
cm_Train = table(data$target, pred_Train)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
View(cm_Train)
View(pred_Train)
str(pred_Train)

library(caret)
precision <- posPredValue(pred_Train, data$target, positive="1")
recall <- sensitivity(pred_Train, data$target, positive="1")
F1 <- (2 * precision * recall) / (precision + recall)


#using test data for prediction

data2 <- read.csv("testdata.csv",header = T)
str(data2)
sum(is.na(data2))
colSums(is.na(data2))
data2$Attr37 <- NULL
data2$id <- NULL
sum(is.na(data2))

# # 
# manyNAs(data = data2,nORp = 0.1)
# nonadata_test <- data2[-manyNAs(data = data2,nORp = 0.1),]
nonadata_test <- knnImputation(data = data2,k = 5)
sum(is.na(nonadata_test))
View(nonadata_test)

write.csv(x = nonadata_test,row.names = F,file = "Nonadata_test.csv")
test_data <- read.csv(file = "Nonadata_test.csv",header = T)
datawot1_test <- test_data[,which(names(test_data) %in% c("Attr2","Attr15","Attr32","Attr33","Attr34","Attr40",
                                                          "Attr42","Attr51","Attr53","Attr54","Attr56","Attr64"))]
str(datawot1_test)
dtest = xgb.DMatrix(data = as.matrix(datawot1_test))
prob_test <- predict(model, as.matrix(datawot1_test))
pred_Test  =  ifelse(prob_test>0.085,1,0) 
sum(pred_Test)
write.csv(x = pred_Test,file = "prediction_test.csv",row.names = F)
head(pred_Test)
### This model has been tuned from 8.83 F1 score to 26.82 F1 Score. 
# Performed XGBoost with 12 attributes which was positively correlated with target attribute.

## MODEL-6 : ADA BOOST
----------------------------------------------------------------------------------------------
  
  data1 <- read.csv("traindata.csv",header = T)
str(data1)
summary(data1)
data1$ID <- NULL
sum(is.na(data1))
colSums(is.na(data1))
# ATTR37 have more than 20% of NA values, Hence removing Attr37 from data
data1$Attr37 <- NULL
data1$target <- as.factor(as.character(data1$target))
sum(is.na(data1))


library(DMwR)
manyNAs(data = data1,nORp = 0.1)
nonadata <- data1[-manyNAs(data = data1,nORp = 0.1),]
nonadata1 <- knnImputation(data = nonadata,k = 5)
sum(is.na(nonadata1))
View(nonadata1)

write.csv(x = nonadata1,row.names = F,file = "Nonadata.csv")
data <- read.csv(file = "Nonadata.csv",header = T)
data$target <- as.factor(as.character(data$target))
library(ada)
datawot <- data[,-which(names(data) %in% c("target"))]
str(datawot)
model <- ada(x = datawot,y = data$target, iter=10, loss="logistic")
summary(model)

pred_Train  =  predict(model, data)  
write.csv(x = pred_Train,file = "prediction_Train.csv",row.names = F)
# Building confusion matrix and find accuracy   
cm_Train = table(data$target, pred_Train)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
View(cm_Train)
View(pred_Train)
str(pred_Train)

library(caret)
precision <- posPredValue(pred_Train, data$target, positive="1")
recall <- sensitivity(pred_Train, data$target, positive="1")
F1 <- (2 * precision * recall) / (precision + recall)


#using test data for prediction

data2 <- read.csv("testdata.csv",header = T)
str(data2)
sum(is.na(data2))
colSums(is.na(data2))
data2$Attr37 <- NULL
data2$id <- NULL
sum(is.na(data2))

# # 
# manyNAs(data = data2,nORp = 0.1)
# nonadata_test <- data2[-manyNAs(data = data2,nORp = 0.1),]
nonadata_test <- knnImputation(data = data2,k = 5)
sum(is.na(nonadata_test))
View(nonadata_test)

write.csv(x = nonadata_test,row.names = F,file = "Nonadata_test.csv")
test_data <- read.csv(file = "Nonadata_test.csv",header = T)
# test_data$target <- NA
# test_data$target <- as.factor(as.character(test_data$target))
str(test_data)
pred_Test  =  predict(model, test_data) 
View(pred_Test)
write.csv(x = pred_Test,file = "prediction.csv",row.names = F)
head(pred_Test)

## ADA Boost with standardisation resulted 11.24 F1 Score, Which is worse than without standardization.
#without Satndardizing ADA Boost resulted with 24.06 F1 Score.
## Also standardising with Preprocess resulted 3.24 F1 Score. Where,
#standardising with decostand resulted 0.

