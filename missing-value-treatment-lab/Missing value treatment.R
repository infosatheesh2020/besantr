rm(list=ls(all=TRUE))
bank_data<-read.csv("Bankruptcy_data.csv",header=TRUE)
sum(is.na(bank_data))
colSums(is.na(bank_data))

#Subsetting data by first 1000 rows
data_1000 = bank_data[1:1000,]
colSums(is.na(data_1000))

# install.packages("DMwR")
library(DMwR)

#Performing central Imputation and assigning to the new name.
imp_central_data=centralImputation(data = data_1000)
colSums(is.na(imp_central_data))
sum(is.na(imp_central_data))

#Performing KNN Imputation and assigning to the new name.
imp_knn_data <- knnImputation(data = data_1000,k = 10)
sum(is.na(imp_knn_data))
colSums(is.na(imp_knn_data))
