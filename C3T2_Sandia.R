install.packages('e1071', dependencies=TRUE)
install.packages('C50', dependencies=TRUE)

library(e1071)
library(caret)
library(gbm)
library(randomForest)
library(mlbench)
library(C50)

# Load Dataset

Train_Complete_responses.Datasets<- read.csv("/Users/ajayvembu/Downloads/SurveyData/Completeresponses.csv")

# Set the SEED
set.seed(123)
trainIndex <- createDataPartition(Train_Complete_responses.Datasets$brand, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
str(trainIndex)

Train_Complete_responses.Datasets <- Train_Complete_responses.Datasets[ trainIndex,]
Test_Complete_responses.Datasets  <- Train_Complete_responses.Datasets[-trainIndex,]


Train_Complete_responses.Datasets$brand = as.factor(Train_Complete_responses.Datasets$brand)
Test_Complete_responses.Datasets$brand = as.factor(Test_Complete_responses.Datasets$brand)

str(Train_Complete_responses.Datasets)
str(Test_Complete_responses.Datasets)

# Random Forest Classifier - Total 2 models (rfTuneGridModel, rfTuneLengthModel)

# 10 fold Cross Validation for Random Forest
rfControl <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')

# Tune Grid for MTRY values from 1 to 6
rfTuneGrid <- expand.grid(.mtry=c(1:6))

# Train the Random Forest Model
rfTuneGridModel <- train(brand ~ . , 
                    data=Train_Complete_responses.Datasets,
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=rfTuneGrid, 
                    trControl=rfControl,
                    ntree=1000)
rfTuneGridModel

# Using Tune Length 

# Train the Random Forest Model
rfTuneLengthModel <- train(brand ~ . , 
                    data=Train_Complete_responses.Datasets,
                    method='rf', 
                    tuneLength=8, 
                    trControl=rfControl,
                    ntree=1000)
rfTuneLengthModel

# Algorithm Tune (tuneRF will give the mtry values byitself)
bestMTRY<- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=1000)
bestMTRY

# Get the Variable Importance
varImp(rfTuneGridModel)
varImp(rfTuneLengthModel)

# Stochastic Gradient Boosting Classifier - Total 4 models (gbmDefaultModel, gbmTuneLengthModel, gbmTuneGridModel & gbmROCModel)

gbmControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)

# Default GBM model
gbmDefaultModel <- train(brand ~ ., 
                data = Train_Complete_responses.Datasets, 
                method = "gbm", 
                trControl = gbmControl,
                verbose = FALSE)
gbmDefaultModel

# GBM Tune Length = 8  
gbmTuneLengthModel <- train(brand ~ ., data = Train_Complete_responses.Datasets, 
                 method = "gbm", 
                 tuneLength = 8,
                 trControl = gbmControl, 
                 verbose = FALSE)
gbmTuneLengthModel

# GBM Tune Grid
gbmTuneGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
gbmTuneGridModel <- train(brand ~ ., data = Train_Complete_responses.Datasets, 
                 method = "gbm", 
                 tuneGrid = gbmTuneGrid,
                 trControl = gbmControl, 
                 verbose = FALSE)
gbmTuneGridModel

# GBM using ROC

# Change levels for brand from 0,1 to X0,X1 for ROC
levels(Train_Complete_responses.Datasets$brand) <- make.names(levels(factor(Train_Complete_responses.Datasets$brand)))

#GBM Control ROC
gbmControlROC <- trainControl(method = "repeatedcv",number = 10,repeats = 10,classProbs = TRUE)

#Train_Complete_responses.Datasets$brand <- factor(Train_Complete_responses.Datasets$brand)
gbmROCModel <- train(brand ~ ., 
                 data = Train_Complete_responses.Datasets, 
                 method = "gbm", 
                 trControl = gbmControlROC, 
                 verbose = FALSE, 
                 tuneGrid = gbmTuneGrid,
                 metric = "ROC")

gbmROCModel

# Find the Best model from gbmROCModel
whichTwoPct <- tolerance(gbmROCModel$results, 
                         metric = "ROC", 
                         tol = 2, 
                         maximize = TRUE)  
cat("best model within 2 pct of best:\n")
gbmROCModel$results[whichTwoPct,1:6]

# Get the Variable Importance
varImp(gbmDefaultModel)
varImp(gbmTuneLengthModel)
varImp(gbmTuneGridModel)
varImp(gbmROCModel)



# C5.0 model - Total 2 models (c50TuneGridModel & c50TuneLengthModel)

c50Control <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, 
                           returnResamp="all")

# Choose the features and classes
x <- Train_Complete_responses.Datasets[c("salary","age","elevel","car","zipcode","credit")]
y <- Train_Complete_responses.Datasets$brand

#C5.0 Tune Grid
c50TuneGrid <- expand.grid( .winnow = c(TRUE,FALSE), 
                     .trials=c(1,5,10,15,20,25,30), 
                     .model="tree" )

c50TuneGridModel <- train(x=x,
                          y=y,
                          tuneGrid=c50TuneGrid,
                          trControl=c50Control,
                          method="C5.0",
                          verbose=FALSE)
c50TuneGridModel

# Tune Length = 8 
c50TuneLengthModel <- train(x=x,
                            y=y,
                            tuneLength=15,
                            trControl=c50Control,
                            method="C5.0",
                            verbose=FALSE)
c50TuneLengthModel

# Get the Variable Importance

varImp(c50TuneGridModel)
varImp(c50TuneLengthModel)

# Decisiontree - Total 2 models (dTreeTuneLengthModel & dTreeTuneGridModel)

dTreeControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


# Using Tune Length
dTreeTuneLengthModel <- train(brand ~., 
                              data = Train_Complete_responses.Datasets, 
                              method = "rpart",
                              parms = list(split = "information"),
                              trControl=dTreeControl,
                              tuneLength = 15,
                              preProc = c("center", "scale"))
dTreeTuneLengthModel

# Using Tune Grid
dTreeTuneGrid <- expand.grid(cp=seq(0,0.1,0.01))
dTreeTuneGridModel <- train(brand ~., 
                    data = Train_Complete_responses.Datasets, 
                    method = "rpart",
                    parms = list(split = "information"),
                    trControl=trctrl,
                    tuneGrid = rpart.grid,
                    preProc = c("center", "scale"))
dTreeTuneGridModel

# Get the Variable Importance
varImp(dTreeTuneLengthModel)
varImp(dTreeTuneGridModel)

# Compare the Predictions

# Random Forest

# Tune Grid
predictionsTuneGrid <- predict(rfTuneGridModel,Test_Complete_responses.Datasets)
cmRFTuneGrid <- confusionMatrix(predictionsTuneGrid,Test_Complete_responses.Datasets$brand)
cmRFTuneGrid
postResample(predictionsTuneGrid, Test_Complete_responses.Datasets$brand)

#Tune Length
predictionsTuneLength <- predict(rfTuneLengthModel,Test_Complete_responses.Datasets)
cmRFTuneLength <- confusionMatrix(predictionsTuneLength,Test_Complete_responses.Datasets$brand)
cmRFTuneLength
postResample(predictionsTuneLength, Test_Complete_responses.Datasets$brand)


# GBM

# Default

predictionGBMDefault <- predict(gbmDefaultModel,Test_Complete_responses.Datasets)
cmGBMDefault <- confusionMatrix(predictionGBMDefault,Test_Complete_responses.Datasets$brand)
cmGBMDefault
postResample(predictionGBMDefault, Test_Complete_responses.Datasets$brand)

# Tune Grid

predictionGBMTuneGrid <- predict(gbmTuneGridModel,Test_Complete_responses.Datasets)
cmGBMTuneGrid <- confusionMatrix(predictionGBMTuneGrid,Test_Complete_responses.Datasets$brand)
cmGBMTuneGrid
postResample(predictionGBMTuneGrid, Test_Complete_responses.Datasets$brand)

# Tune Length

predictionGBMTuneLength <- predict(gbmTuneLengthModel,Test_Complete_responses.Datasets)
cmGBMTuneLength <- confusionMatrix(predictionGBMTuneLength,Test_Complete_responses.Datasets$brand)
cmGBMTuneLength
postResample(predictionGBMTuneLength, Test_Complete_responses.Datasets$brand)

# ROC

predictionGBMROC <- predict(gbmROCModel,Test_Complete_responses.Datasets)
predictionGBMROC <- factor(predictionGBMROC, levels = c('X0', 'X1'), labels = c(0,1))
predictionGBMROC <- as.factor(predictionGBMROC)
cmGBMROC <- confusionMatrix(predictionGBMROC,Test_Complete_responses.Datasets$brand)
cmGBMROC
postResample(predictionGBMROC, Test_Complete_responses.Datasets$brand)

#C5.0

# Tune Grid
predictionC50TuneGrid <- predict(c50TuneGridModel,Test_Complete_responses.Datasets)
cmC50TuneGrid <- confusionMatrix(predictionC50TuneGrid,Test_Complete_responses.Datasets$brand)
cmC50TuneGrid
postResample(predictionC50TuneGrid, Test_Complete_responses.Datasets$brand)

# Tune Length
predictionC50TuneLength <- predict(c50TuneLengthModel,Test_Complete_responses.Datasets)
cmC50TuneLength <- confusionMatrix(predictionC50TuneLength,Test_Complete_responses.Datasets$brand)
cmC50TuneLength
postResample(predictionC50TuneLength, Test_Complete_responses.Datasets$brand)

#Decision tree

# Tune Length
predictiondTreeTuneLength <- predict(dTreeTuneLengthModel,Test_Complete_responses.Datasets)
cmTreeTuneLength <- confusionMatrix(predictiondTreeTuneLength,Test_Complete_responses.Datasets$brand)
cmTreeTuneLength
postResample(predictiondTreeTuneLength, Test_Complete_responses.Datasets$brand)

# Tune Grid
predictiondTreeTuneGrid <- predict(dTreeTuneGridModel,Test_Complete_responses.Datasets)
cmTreeTuneGrid <- confusionMatrix(predictiondTreeTuneGrid,Test_Complete_responses.Datasets$brand)
cmTreeTuneGrid
postResample(predictiondTreeTuneGrid, Test_Complete_responses.Datasets$brand)

#summary for Random forest,GBM,C5.0,Decision tree models 
summary(rfTuneGridModel)
summary(rfTuneLengthModel)
summary(gbmDefaultModel)
summary(gbmTuneGridModel)
summary(gbmROCModel)
summary(c50TuneGridModel)
summary(c50TuneLengthModel)
summary(dTreeTuneLengthModel)
summary(dTreeTuneGridModel)

# Prediction on the Incomplete Survey Data
Test_SurveyIncomplete.Datasets<- read.csv("/Users/ajayvembu/Downloads/SurveyData/SurveyIncomplete.csv")
Test_SurveyIncomplete.Datasets$brand = as.factor(Test_SurveyIncomplete.Datasets$brand)
# Random Forest

# Tune Grid
predictionsTuneGridSurveyIncomplete <- predict(rfTuneGridModel,Test_SurveyIncomplete.Datasets)
cmRFTuneGridSurveyIncomplete <- confusionMatrix(predictionsTuneGridSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmRFTuneGridSurveyIncomplete
postResample(predictionsTuneGridSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)


#Tune Length
predictionsTuneLengthSurveyIncomplete <- predict(rfTuneLengthModel,Test_SurveyIncomplete.Datasets)
cmRFTuneLengthSurveyIncomplete<- confusionMatrix(predictionsTuneLengthSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmRFTuneLengthSurveyIncomplete
postResample(predictionsTuneLengthSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)


# GBM

# Default

predictionGBMDefaultSurveyIncomplete <- predict(gbmDefaultModel,Test_SurveyIncomplete.Datasets)
cmGBMDefaultSurveyIncomplete <- confusionMatrix(predictionGBMDefaultSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmGBMDefaultSurveyIncomplete
postResample(predictionGBMDefaultSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

# Tune Grid

predictionGBMTuneGridSurveyIncomplete <- predict(gbmTuneGridModel,Test_SurveyIncomplete.Datasets)
cmGBMTuneGridSurveyIncomplete <- confusionMatrix(predictionGBMTuneGridSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmGBMTuneGridSurveyIncomplete
postResample(predictionGBMTuneGridSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

# Tune Length

predictionGBMTuneLengthSurveyIncomplete <- predict(gbmTuneLengthModel,Test_SurveyIncomplete.Datasets)
cmGBMTuneLengthSurveyIncomplete <- confusionMatrix(predictionGBMTuneLengthSurveyIncomplete ,Test_SurveyIncomplete.Datasets$brand)
cmGBMTuneLengthSurveyIncomplete
postResample(predictionGBMTuneLengthSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

# ROC

predictionGBMROCSurveyIncomplete <- predict(gbmROCModel,Test_SurveyIncomplete.Datasets)
predictionGBMROCSurveyIncomplete <- factor(predictionGBMROCSurveyIncomplete, levels = c('X0', 'X1'), labels = c(0,1))
predictionGBMROCSurveyIncomplete <- as.factor(predictionGBMROCSurveyIncomplete)
cmGBMROCSurveyIncompleteSurveyIncomplete <- confusionMatrix(predictionGBMROCSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmGBMROCSurveyIncompleteSurveyIncomplete
postResample(predictionGBMROCSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

#C5.0

# Tune Grid
predictionC50TuneGridSurveyIncomplete <- predict(c50TuneGridModel,Test_SurveyIncomplete.Datasets)
cmC50TuneGridSurveyIncomplete <- confusionMatrix(predictionC50TuneGridSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmC50TuneGridSurveyIncomplete
postResample(predictionC50TuneGridSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

# Tune Length
predictionC50TuneLengthSurveyIncomplete <- predict(c50TuneLengthModel,Test_SurveyIncomplete.Datasets)
cmC50TuneLengthSurveyIncomplete <- confusionMatrix(predictionC50TuneLengthSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmC50TuneLengthSurveyIncomplete
postResample(predictionC50TuneLengthSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

#Decision tree

# Tune Length
predictiondTreeTuneLengthSurveyIncomplete <- predict(dTreeTuneLengthModel,Test_SurveyIncomplete.Datasets)
cmTreeTuneLengthSurveyIncomplete <- confusionMatrix(predictiondTreeTuneLengthSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmTreeTuneLengthSurveyIncomplete
postResample(predictiondTreeTuneLengthSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)

# Tune Grid
predictiondTreeTuneGridSurveyIncomplete <- predict(dTreeTuneGridModel,Test_SurveyIncomplete.Datasets)
cmTreeTuneGridSurveyIncomplete <- confusionMatrix(predictiondTreeTuneGridSurveyIncomplete,Test_SurveyIncomplete.Datasets$brand)
cmTreeTuneGridSurveyIncomplete
postResample(predictiondTreeTuneGridSurveyIncomplete, Test_SurveyIncomplete.Datasets$brand)


