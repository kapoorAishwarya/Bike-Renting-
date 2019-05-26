# clearing R enviroment
rm(list = ls())

#Seting working directory 
setwd("C:/Users/Lenovo/Desktop/Edwisor/Project_1")

#packages and library
lib_name = c("ggplot2", "corrgram", "caret", "randomForest", "unbalanced", "c50", "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE")
#Using the lapply function we will install all the packages in one go
lapply(lib_name, require, character.only = TRUE)
rm(lib_name)
install.packages("DMwR")
library(DMwR)
install.packages("randomForest")
library(randomForest)
library(rpart)
library(MASS)
install.packages("usdm")
library(usdm)
library(caret)

# Reading data 
day = read.csv("day.csv", header = T)
View(day)
summary(day)

#removing the 2 variables from the data - instant and dtdate
dayd = day[,-1]
dayd = dayd[,-1]
View(dayd)

#the random forest
sacling <- randomForest(cnt ~. , data = dayd, ntree =100, keep.forest = FALSE, importance = TRUE)
importance(sacling, type = 1)
symnum(cor(dayd))

# creating the sample data
sample_data = sample(1:nrow(dayd), 0.80 * nrow(dayd))
test_data = dayd[sample_data,]
train_data = dayd[-sample_data,]

# regression model
fit = rpart(cnt ~ . , data = test_data, method = "anova")
summary(fit)
prediction = predict(fit, train_data [,-14])
prediction

#calculating the MAPE
mape = function(y, yhat){
  mean(abs((y-yhat)/y))
}
mape(train_data[,14],prediction)

#Calculatig the regr.eval
regr.eval(train_data[,14], prediction, stats = c('mae' ,'rmse' ,'mape' , 'mse'))

write.csv(dayd, "dayd.csv", row.names = F)
