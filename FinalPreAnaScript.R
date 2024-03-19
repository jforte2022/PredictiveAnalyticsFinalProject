library(dplyr)
library(forecast)
library(caret)
library(leaps)

data <- read.csv("Spotify_Clean.csv")
data$key <- as.factor(data$key)
data$mode <- as.factor(data$mode)
data$time_signature <- as.factor(data$time_signature)
data <- data[,-1]

set.seed(1)
trainIndex <- sample(c(1:nrow(data)), nrow(data) *0.6)
trainingSpotify <- data[trainIndex, ]
validationSpotify <- data[-trainIndex, ]

model1 <- train(popularity ~ ., data = trainingSpotify, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = T))
model1Predict <- round(predict(model1, validationSpotify),digits = 0)
accuracy(model1Predict, validationSpotify$popularity)

model2 <- train(popularity ~ ., data = trainingSpotify[,-c(7, 12, 13)], method = 'lm', trControl = trainControl(method = 'cv', number = 10, verboseIter = T))
model2Predict <- round(predict(model2, validationSpotify),digits = 0)
accuracy(model2Predict, validationSpotify$popularity)

knnModel <- train(popularity ~ ., data = trainingSpotify, method = 'knn', preProcess = c("center", "scale"))
knnModel

plot(knnModel)
varImp(knnModel)

#training set test
knnModelPredict <- round(predict(knnModel, validationSpotify), digits = 0)
accuracy(knnModelPredict, validationSpotify$popularity)



#plot
knnPlot <- plot(knnModelPredict, validationSpotify$popularity, xlab = "Prediction", ylab = "Actual")
knnPlot
abline(lm(knnModelPredict ~ validationSpotify$popularity))

