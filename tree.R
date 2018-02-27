library(ISLR)
#install.packages("tree")
library(tree)
attach(Carseats)

fix(Carseats)
dim(Carseats)

#making binary response for doing clssification
#using Boolean Expression
#if its true we call it as NO

High=ifelse(Sales <=8,"No","Yes")
#
#add this column to the table
#new table == data.frame
#we can use cbind also
detach(Carseats)

Carseats = data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
#Partition the data into trianing and test #
set.seed (2)
 
train=sample(1: nrow(Carseats), 200) #create a random sample of size 300 from Carseats
#randomly create a subset of a data set indices, that can use as training data set.
#spliting data set into training and testing data set.

Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

High.train=High[train]
High.test=High[-train]



#use tree() function to do CART classification
#
tree.carseats = tree(High~.-Sales,Carseats)
summary(tree.carseats)
#
#see the results
#
plot(tree.carseats )
text(tree.carseats ,pretty =0)
#
#tree.carseats
#
#test data
#only train with training data
tree.carseats =tree(High~.-Sales,Carseats,subset=train)
# use test to predict the result
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)



set.seed (3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats)
cv.carseats
#
# visualize the results
#
par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
#
# prune.misclass() based on cv results
#
prune.carseats = prune.misclass(tree.carseats ,best =9)
plot(prune.carseats)
text(prune.carseats,pretty =0)
#
#test pruned tree
#
tree.pred=predict(prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
#regression trees
library(MASS)
set.seed(1) 
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

#using cross validation to improve the results of tree
cv.boson=cv.tree(tree.boston)
plot(cv.boson$size,cv.boson$dev,type='b')

prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhatt=predict(tree.boston,newdata = Boston[-train,])
boston.test=Boston[-train,"medv"]

plot(yhatt,boston.test)
abline(0,1)
MSE=sqrt(mean((yhatt-boston.test)^2))
MSE
