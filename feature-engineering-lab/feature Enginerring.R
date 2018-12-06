#removing data variables in Environment
rm(list=ls(all=TRUE))

#Setting working directory
setwd("C:/Users/10891/Downloads/Mukil/Besant")

#reading the data in to environment
data = read.csv("Toyota_SimpleReg.csv",header = T)

#getting the structure of the data
str(data)
max(data$Price)
min(data$Price)

#LOG transformed data
log_data = log(data)
str(log_data)
max(log_data$Price)
min(log_data$Price)

#Standardization
#install required packages
#install.packages("vegan")

#calling library
library(vegan)

#standardization using Range
range_std = decostand(data,method = 'range')
max(range_std$Price)
min(range_std$Price)


#standardization using Satndard Deviation
SD_data = decostand(data,method = 'standardize')
max(SD_data$Price)
min(SD_data$Price)

# aa= c(3,5)
Height= c(145,163,172,183,191)
weight= c(56000,67500,73845,97345,100000)
data1=cbind(Height,weight)
# data2= rbind(a,b)

data1_range = decostand(data1,method = 'range')
data1_sd = decostand(data1,method = 'standardize')
sd(a)

log_data1 = log(data1)