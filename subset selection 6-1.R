# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
summary(Hitters)
#showing number of rows having na values

sum(is.na(Hitters$Salary))
#na.omit removes the raws having na values
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
#Best subset selection
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
#print the best set of predictors for each model size;
# by default, only return results up to the best 8-predictor model

summary(regfit.full)
###to return as many predictors as specified

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2


#

#find the best model according to each criteria
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
which.min(reg.summary$rss)
points(19,reg.summary$rss[19],col="red",cex=2,pch=20)
#
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted  RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
#**************************
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
#
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)



plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

##print the coefficient estimates of the 7-predictor model
#they identified different best model by using their respective method
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models
#USING Validation set and Cross Validation
set.seed(1)
##Randomly split data into a training set and a test set
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
##Perform best subset selection
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
##building an "X" matrix from test data
test.mat=model.matrix(Salary~.,data=Hitters[test,])
?model.matrix
head(test.mat)
###Compute test MSE of the 19 models
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)


##after finding the best model, we need to fit this model using the full data set to obtain more accurate coefficient estimates

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
###Computing predictions was a little tedious, partly because there is no predict() method for regsubsets(). The following codes create a function for prediction.
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
##Note: need to perform best subset selection in each fold; need to compute test error for each model in each fold

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
cv.errors
#average errors over folds
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
##find coefficient estimates of the selected model using all data
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)

