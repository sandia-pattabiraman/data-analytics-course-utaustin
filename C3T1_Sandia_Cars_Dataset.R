#Package installation 
install.packages("reader")
install.packages("dplyr") 
install.packages("float")

#Package installation adding dependent packages and packages suggested by maintainer
install.packages("reader", dependencies = c("Depends", "Suggests"))

#Library
library(readr)
library(dplyr)
library(float)


#read the car dataset
cars_Dataset<- read.csv("/Users/ajayvembu/Downloads/R Tutorial Data Sets/cars.csv")

#List your attributes within your data set
attributes(cars_Dataset)

#Prints the min, max, mean, median, and quartiles of each attribute
summary(cars_Dataset)

#Displays the structure of your data set
str(cars_Dataset)

#Names your attributes within your data set
names(cars_Dataset)

#Will print out the instances within that particular column in your data set
cars_Dataset$name.of.car 
cars_Dataset$speed.of.car 
cars_Dataset$distance.of.car 

#Histogram Plot
hist(cars_Dataset$name.of.car)
hist(cars_Dataset$speed.of.car)
hist(cars_Dataset$distance.of.car)

cars_Dataset %>%                                      
  group_by(name.of.car) %>%                         
  summarise_at(vars(name.of.car),              
               list(speed.of.car  = mean, distance.of.car = mean))

#Scatter box plot-Speed increases distance also increases shows linearity
plot(cars_Dataset$speed.of.car,cars_Dataset$distance.of.car)

#Normal Quantile Plot
qqnorm(cars_Dataset$speed.of.car)
qqnorm(cars_Dataset$distance.of.car)

#datatypes conversion 
cars_Dataset$speed.of.car<-as.float(cars_Dataset$speed.of.car)
cars_Dataset$speed.of.car<-as.integer(cars_Dataset$speed.of.car)

#renaming the columns
names(cars_Dataset)<-c("title.of.car","fast.of.car","miles.of.car") 
names(cars_Dataset)<-c("name.of.car","speed.of.car","distance.of.car") 

##find missing values
#Will count how many NA’s(not available) you have
summary(cars_Dataset)
#Will show your NA’s through logical data. (TRUE if it’s missing, FALSE if it’s not.)
is.na(cars_Dataset) 

#Drops any rows with missing values and omits them forever.
na.omit(cars_Dataset$name.of.car)

#Drops any rows with missing values, but keeps track of where they were.
na.exclude(cars_Dataset$name.of.car)

#Replace the missing values with the mean
cars_Dataset$name.of.car[is.na(cars_Dataset$name.of.car)]<-mean(cars_Dataset$name.of.car,na.rm = TRUE)

##Creating Testing and Training Sets
#to create your training and testing sets, you need to use the set.seed() function.
set.seed(123)

#calculate the sizes of each set but do not create the sets:
trainSize<-round(nrow(cars_Dataset)*0.7) 
testSize<-nrow(cars_Dataset)-trainSize

# to see how many instances will be in each set, you 
trainSize
testSize

#To create the training and test sets
training_indices<-sample(seq_len(nrow(cars_Dataset)),size =trainSize)
trainSet<-cars_Dataset[training_indices,]
testSet<-cars_Dataset[-training_indices,] 

#To create this model, we will be using the linear model function
Pedicting_distance<-lm(distance.of.car~ speed.of.car, trainSet)
summary(Pedicting_distance)

#Predictions
predicted_distance <- predict(Pedicting_distance,testSet)
#to view the predictions
predicted_distance



