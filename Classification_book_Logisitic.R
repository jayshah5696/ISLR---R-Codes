#Part 1#
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[ ,-9])
attach(Smarket)
plot(Volume)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[ ,4]
glm.probs=predict(glm.fit, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"
#The first command creates a vector of 1,250 Down elements.#
#The second line transforms to Up all of the elements for which the predicted probability of a market increase exceeds 0.5.#
table(glm.pred,Direction)
mean(glm.pred==Direction)

#Part 2#

train=(Year<2005)
Smarket.2005=Smarket[!train, ]
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial,data=Smarket,subset = train)
glm.probs=predict(glm.fit,Smarket.2005,type = "response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

#Part 3#

glm.fit=glm(Direction~Lag1+Lag2,family = binomial,data= Smarket,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
