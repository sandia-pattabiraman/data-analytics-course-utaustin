install.packages("reader")
library("readr")
IrisDataset<- read.csv("/Users/ajayvembu/Downloads/R Tutorial Data Sets/iris.csv")
attributes(IrisDataset)
summary(IrisDataset)
str(IrisDataset)
names(IrisDataset)
hist(IrisDataset$Sepal.Length)
hist(IrisDataset$Sepal.Width)
hist(IrisDataset$Petal.Length)
hist(IrisDataset$Petal.Width)
plot(IrisDataset$Sepal.Length)
plot(IrisDataset$Sepal.Width)
plot(IrisDataset$Petal.Length)
plot(IrisDataset$Petal.Width)
qqnorm(IrisDataset$Sepal.Length)
qqnorm(IrisDataset$Sepal.Width)
qqnorm(IrisDataset$Petal.Length)
qqnorm(IrisDataset$Petal.Width)
IrisDataset$Sepal.Length<- as.numeric(IrisDataset$Sepal.Length)  
IrisDataset$Sepal.Width<- as.numeric(IrisDataset$Sepal.Width)   
IrisDataset$Petal.Length<- as.numeric(IrisDataset$Petal.Length)  
IrisDataset$Petal.Width<- as.numeric(IrisDataset$Petal.Width)  
set.seed(123)
trainSize <- round(nrow(IrisDataset) * 0.2)
testSize <- nrow(IrisDataset) - trainSet
trainSize
testSize
trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]
set.seed(405)
trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]
Predicting_Petallength<-lm(Petal.Length~ Petal.Width + Sepal.Width + Sepal.Length, trainSet)
summary(Predicting_Petallength)
Predicted_Petallength <- predict(Predicting_Petallength,testSet)
Predicted_Petallength





