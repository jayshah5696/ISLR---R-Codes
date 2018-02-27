library(ISLR)
attach(Smarket)
train = Year < 2005

test = !train
training_data = Smarket[train, -8]
testing_data = Smarket[test, -8] 

testing_y = Direction[test]
logistic_model = glm(Direction ~ ., data = training_data, family = "binomial")
logistic_probs = predict(logistic_model, testing_data, type =    "response")

head(logistic_probs)

logistic_pred_y = rep("Down", length(testing_y)) 
logistic_pred_y[logistic_probs > 0.6] = "Up"
#Confusion Matrix
table(logistic_pred_y, testing_y)
mean(logistic_pred_y != testing_y)
   