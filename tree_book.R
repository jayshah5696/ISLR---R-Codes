

#FItting Classification tree
install.packages("tree")
library (tree)
library ( ISLR)
attach ( Carseats)

#We use the ifelse() function to create a variable, called
#High, which takes on a value of Yes if the Sales variable exceeds 8, and
#takes on a value of No otherwise.
High= ifelse (Sales <=8 ," No"," Yes ")
#Finally, we use the data.frame() function to merge High with the rest of
#the Carseats data.

Carseats =data.frame ( Carseats ,High)
summary(Carseats)
#creating a classification tree using tree function

tree.carseats = tree( High~.-Sales , Carseats )
summary (tree.carseats )
#in summary the missclasification error is given by 
#is cross entropy index
#The residual mean deviance reported is
#simply the deviance divided by n???|T0|, which in this case is 400???27 = 373.

plot( tree.carseats )
text( tree.carseats , pretty =0)

tree.carseats



#training and testing
set.seed (2)
train = sample (1: nrow( Carseats ), 200)
Carseats.test= Carseats [-train ,]
High.test=High [- train ]
tree.carseats = tree( High~.-Sales ,Carseats ,subset = train )
tree.pred= predict (tree.carseats , Carseats.test , type ="class")
table (tree.pred , High.test )
(86+57)/200



#cv.error
#We use the argument FUN=prune.misclass in order to indicate that we want the classification error
#rate to guide the cross-validation and pruning process, rather than the
#default for the cv.tree() function, which is deviance

set.seed (3)
cv.carseats =cv.tree( tree.carseats ,FUN =prune.misclass )
names (cv.carseats )
cv.carseats

#ploting error rate as size as well as k
par ( mfrow =c(1 ,2) )
plot(cv.carseats$size ,cv.carseats$dev ,type ="b")
plot(cv.carseats$k ,cv.carseats$dev ,type ="b")


#apply prune to tree
#prune.missclass() is used to prune the tree
prune.carseats =prune.misclass (tree.carseats ,best =9)
plot( prune.carseats )
text( prune.carseats , pretty =0)


#calculating test error
tree.pred= predict(prune.carseats , Carseats.test ,type ="class")
table (tree.pred , High.test )
(94+66)/200


#if we use size =15 then
#classification accuracy:
prune.carseats =prune.misclass(tree.carseats,best =15)
plot( prune.carseats )
text( prune.carseats , pretty =0)
tree.pred= predict (prune.carseats , Carseats.test ,type ="class")
table (tree.pred , High.test )



#Fitting Regression Trees
library ( MASS)
set.seed (1)
train = sample (1: nrow( Boston ), nrow( Boston )/2)
tree.boston =tree( medv~., Boston , subset =train )
summary ( tree.boston )
#here deviance will be RSS

plot( tree.boston )
text( tree.boston , pretty =0)




cv.boston =cv.tree(tree.boston )
plot(cv.boston$size ,cv.boston$dev , type="b")


# applying prune tree size to actual tree

prune.boston = prune.tree(tree.boston ,best =9)
plot( prune.boston )
text( prune.boston , pretty =0)


yhat= predict ( tree.boston , newdata = Boston[-train ,])
boston.test = Boston[-train ," medv "]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
#?????






#Bagging & Random Forest


#Bagging is special case of RandomForest m =p

library (randomForest)
set.seed (1)
bag.boston = randomForest(medv~., data=Boston , subset =train , mtry =13, importance = TRUE)
bag.boston

#Outof bag error
yhat.bag = predict (bag.boston , newdata =Boston[-train ,])
plot(yhat.bag , boston.test)
abline (0 ,1)
mean(( yhat.bag - boston.test)^2)


#We could change the number of trees grown by randomForest() using the ntree argument:  
bag.boston = randomForest(medv~., data=Boston , subset =train , mtry =13, ntree =25)
yhat.bag = predict (bag.boston , newdata =Boston [-train ,])
mean(( yhat.bag - boston.test)^2)


#For Rendom Forest
#we take m=sqrt(p) for classificatiom
#we take m=p/3 for regression
set.seed (1)
rf.boston = randomForest( medv~.,data =Boston , subset =train , mtry =6, importance = TRUE)
yhat.rf = predict (rf.boston , newdata = Boston [- train ,])
mean (( yhat.rf - boston.test )^2)
importance(rf.boston)
#here in the case of regression node purity is measured by training Rss and for classification deviance
varImpPlot(rf.boston)


#Boosting

#We run gbm() with the option
#distribution="gaussian" since this is a regression problem; if it were a binary
#classification problem, we would use distribution="bernoulli".
#the argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 limits the depth of each tree.
library (gbm )
set.seed(1)
boost.boston =gbm ( medv~., data =Boston [train ,], distribution= "gaussian",n.trees =5000 , interaction.depth =4)
summary(boost.boston)


#partial dependence plot for rm and lstat
par(mfrow =c(1 ,2))
plot( boost.boston ,i="rm")
plot( boost.boston ,i="lstat")

#predict

yhat.boost = predict ( boost.boston , newdata = Boston [-train ,], n.trees =5000)
mean(( yhat.boost - boston.test )^2)

#WE CAN perform boosting with different value of shrinkage parameter

boost.boston =gbm ( medv~., data =Boston [train ,], distribution= "gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2 , verbose =F)
yhat.boost = predict ( boost.boston , newdata = Boston [-train ,],   n.trees =5000)
mean(( yhat.boost - boston.test )^2)
