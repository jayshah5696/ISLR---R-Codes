# Ridge Regression and LASSO
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
#
###the function glmnet() in the glmnet package

library(glmnet)

#the package invokes inputs and outputs separately unlike lm and glm
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# set vector of lambda values to study range from 10^10 to 0.01, total length=100
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
?glmnet
#
#
# let us look at a few results here

#first lambda=50
###first, the 50th value of lambda and the coefficient estimates resulted from that value

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#l2 norm
##the sum of squares of those estimates

sqrt(sum(coef(ridge.mod)[-1,50]^2))

#next, lambda=60
#
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#prediction of the coefficients for lambda=50 (play with this)
##prediction of the coefficients for new value of lambda 
##for example, lambda=25 

predict(ridge.mod,s=50,type="coefficients")[1:20,]

#prepare for training and validation set testing

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
##fit ridge regression on training data

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
#predict on test data with lamda=4

#seee the newx argument
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
#

#evaluate and compare test MSE and the spread of y.test
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
#
#test wth two other lambdas
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)

#
#compare with lm
# The following two are the same
#
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

#
#Cross validation to get the best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#now predict with the best lambda
#
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

##refit ridge regression on the full dataset
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


# Lasso

#only difference in model building is to use aloha=1
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# use CV to get best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min


#use best lambda for prediction
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
#Check the estimated coefficient
lasso.coef
lasso.coef[lasso.coef!=0]