#Prediction Assignment Writeup
#These are the required packages I need for this assignment
library(ggplot2)
library(caret)
library(fscaret)
library(randomForest)
library(e1071)
#Below are the names of the data after I download them which is also shown
train_url <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(train_url,destfile = "pmltraining.csv" )
download.file(test_url,destfile = "pmltesting.csv")

training <- read.table("pmltraining.csv", sep = ",", header = TRUE)
testing <- read.table("pmltesting.csv", sep = ",", header = TRUE)
#Next I will set the seed to increase reproducibility
set.seed(1098)

# Now I will split the training set to two parts, the training and validataion sets
inTrain <- createDataPartition(y=training$classe, p=0.7, list=F)
training1 <- training[inTrain, ]
training2 <- training[-inTrain, ]





#This will remove non zero varaince predictions
nzv <- nearZeroVar(training)
training1 <- training1[, -nzv]
training2 <- training2[, -nzv]
#Next I will remove values that have the NA symbol
training1 <- training1[, colSums(is.na(training1)) == 0]
training2 <- training2[, colSums(is.na(training2)) == 0]
#removing columns unfit for prediction (ID, user_name, raw_timestamp_part_1 etc ...)
training1 <- training1[, -(1:5)]
training2 <- training2[, -(1:5)]



#This will help us choose which model is best

MOD1 <- train(classe ~., method = "rf", data = training1, verbose = TRUE, trControl = trainControl(method="cv"), number = 3)
PREDICT1 <- predict(MOD1, training1)
confusionMatrix(PREDICT1, training1$classe)


PREDICT2 <- predict(MOD1, training2)
confusionMatrix(PREDICT2, training2$classe)

# Now we will apply the model and test it
testing <- testing[, colSums(is.na(testing)) == 0]
testing <- testing[, -(1:5)]
nzvt <- nearZeroVar(testing)
testing <- testing[, -nzvt]


prediction <- predict(MOD1,testing)
prediction











