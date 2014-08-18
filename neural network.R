library(caret)
library(datasets)
library(ggplot2)

data(Boston, package='MASS')

help(Boston, package='MASS')

data(mtcars)

set.seed(13343)

# Impute and standarize data
preImputeObj <- preProcess(Boston, method="knnImpute")
Boston <- predict(preImputeObj, Boston)

# In function [createDataPartition]
# - y should be a vector, not a data frame
# - default value for list is TRUE 
inTrain <- createDataPartition(y = Boston$medv, p = 0.6, list = FALSE)

training_x <- Boston[inTrain, 2:length(Boston)]
testing_x <- Boston[-inTrain, 2:length(Boston)]

training_y <- Boston[inTrain, 1]
testing_y <- Boston[-inTrain, 1]

# Training
modFit <- train(x=training_x,y=training_y, method = "nnet", hidden = 30)
predicted_y <- predict(modFit, training_x)

# For visualization
visualizeResult <- data.frame(id=c(1:length(predicted_y)), value=predicted_y, label = rep(1,length(predicted_y)))
visualizeResult <- rbind(visualizeResult, data.frame(id=c(1:length(predicted_y)), value=training_y, label = rep(0,length(training_y))))

qplot(visualizeResult$id, visualizeResult$value, colour=visualizeResult$label)
