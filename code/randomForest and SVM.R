setwd("D:\\MS Data Analytics\\Sem 2\\ADM\\ADM Project\\Git\\wildfires-adm\\Random Forest")

library(randomForest)
library(mlbench)
library(caret)
library(e1071)

wildFireData = read.csv("finalFileWithColNames.csv")
wildFireData$Date = NULL
wildFireData$lon = NULL
wildFireData$lat = NULL
wildFireData$X = NULL
wildFireData$elevation = NULL

######################### Random Forest #########################
newWildFireData = read.csv("newWildFireData.csv")
seed = 1337
set.seed(seed)
str(newWildFireData)
newWildFireData$avgTemp <- rowMeans(newWildFireData[ , c("minTemp", "maxTemp")] ) 
newWildFireData$minTemp = NULL
newWildFireData$maxTemp = NULL
newWildFireData[,-1] <-scale(newWildFireData[,-1])

#Data slicing : Creating 75/25 split train-test set

sample <- createDataPartition(newWildFireData$fire, p = .75, list = FALSE) 
train <- newWildFireData[sample, ]
test <- newWildFireData[-sample, ]

#test for class imbalance
table(newWildFireData$fire)
table(train$fire)
table(test$fire)

#basic random forest model
forest = randomForest(fire~., data = train, importance= TRUE, ntree=2000)
varImpPlot(forest)
plot(forest)
importance(forest)

newWildFireData$wind = NULL
# creating new samples withouth wind 
sample <- createDataPartition(newWildFireData$fire, p = .75, list = FALSE) 
train <- newWildFireData[sample, ]
test <- newWildFireData[-sample, ]

forest = randomForest(fire~., data = train, importance= TRUE, ntree=2000)

#Evaluate performance of basic random forest

forestPrediction = predict(forest, test, type = "class")
confusionMatrix(forestPrediction, test$fire, positive="Yes")

#########Tunning of parameters of rf
#https://rpubs.com/phamdinhkhanh/389752 --- model tunning
# Manual Search to find best possible number of trees
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(newWildFireData))))
modellist <- list()
for (ntree in c(50,100,200,1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(fire~., data=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

#after 50 there is not much significant difference in acuuracy and kappa values ...so ntree = 200 is choosed

######### Random Search for mtry
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(newWildFireData))
rf_random <- train(fire~., data=train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

#mtry = 1 is the optimal value


########final model with mtry=4 and ntree=2000
forest = randomForest(fire~., data = train, importance= TRUE, ntree=2000, mtry = 4)
#Evaluate performance of random forest

forestPrediction = predict(forest, test, type = "class")
confusionMatrix(forestPrediction, test$fire, positive="Yes")
varImpPlot(forest)
######################### End of Random Forest #########################


######################### SVM ##########################

##########svm using linear kernel
svmfit = svm(fire~., data=train , kernel ="linear", cost=10, scale=FALSE)
svmfit$index
summary(svmfit)


#what's the best value of c
#10-fold cv 
tune.out = tune(svm, fire~., data=train, kernel="linear", ranges=list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestMod = tune.out$best.model


#predicting with optimized model
svmPrediction = predict(bestMod, test)

#evualting performance
confusionMatrix(svmPrediction, test$fire, positive="Yes")



########svm with raidal kernel
svmfit = svm(fire~., data=train , kernel ="radial", cost=10, scale=FALSE)
svmfit$index
summary(svmfit)

#what's the best value of c
#k-fold cv 
tune.out = tune(svm, fire~., data=train, kernel="radial", ranges=list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestMod = tune.out$best.model

#predicting with optimized model
svmPrediction = predict(bestMod, test)

#evualting performance
confusionMatrix(svmPrediction, test$fire, positive="Yes")

#accuracy incerease when radial kernel is used as compared to linear kernel
#but random forest perfromance better than above two models of svm with hihger values of sensitivity, specificity, and accuracy.

