library(MASS)
attach(Boston)
#
#prepare data
#
#install.packages("randomForest")
library(randomForest)
set.seed (3)
train = sample (1: nrow(Boston), nrow(Boston )/2)

#all the medv values that are not in train.
boston.test=Boston[-train ,"medv"]
#
#bagging == randomforest with  #mtry=TotalNumberofPredictors (include all)
#
set.seed (1)
#mtry will be P

bag.boston =randomForest(medv~.,data=Boston,subset =train, mtry=13,importance =TRUE)
bag.boston
 
yhat.bag = predict (bag.boston ,newdata =Boston[-train ,])
 
 
#You can use table for classification
#Confusion Matrix
plot(yhat.bag , boston.test)
abline (0,1)
mean(( yhat.bag-boston.test)^2)
bag.boston$importance





#Random Forest
set.seed (1)
rf.boston =randomForest(medv~.,data=Boston   ,subset =train,mtry=6, importance =TRUE)
yhat.rf = predict (rf.boston ,newdata =Boston [-train ,])
mean(( yhat.rf -boston.test)^2)
#
#perform diagnostics, and report the final
#result mtry=p/2 got good result.
rf.boston$importance
varImpPlot(rf.boston) 

#Boosting
 

# install.packages("gbm")
library (gbm)
set.seed (1)
boost.boston =gbm(medv~.,data=Boston [train ,],distribution="gaussian",n.trees =4000 ,interaction.depth =4,shrinkage = 0.2)
# diagnosis
summary (boost.boston )
#Deviding Result Images into 4 windows.
par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

yhat.boost = predict(boost.boston,newdata=Boston[-train,], n.trees=4000)
mean((yhat.boost-boston.test)^2)

  