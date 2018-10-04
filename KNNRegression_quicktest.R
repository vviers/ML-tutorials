library(caret)
df <- datasets::quakes
str(df)

plot(mag ~ depth, df)

split <- caret::createDataPartition(df$mag, times = 1, p = .75, list = F)
train <- df[split,]
test <- df[-split,]

boxplot(train$mag, test$mag, names = c("train", "test"))

set.seed(42)
ctrl <- trainControl(method="repeatedcv", repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(mag ~ depth, data = train, method = "knn", trControl = ctrl, tuneLength = 20)

#Output of kNN fit
knnFit
plot(knnFit) # k = 11

str(knnFit)

knnPredict <- predict(knnFit, newdata = test)

plot(test$depth, test$mag)
lines(test$depth, knnPredict)

knnFit$finalModel$k <- 7

knnPredict <- predict(knnFit, newdata = data.frame(depth = seq(0, 700, 30)))
plot(test$depth, test$mag)
lines(seq(0, 700, 30), knnPredict)


