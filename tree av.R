#Bagging

install.packages("rpart")
library(rpart)
 x <- cbind(x_train,y_train)
# grow tree 
 fit <- rpart(y_train ~ ., data = x,method="class")
 summary(fit)
#Predict Output 
 predicted= predict(fit,x_test)
 
 
 #Random FOrest
 library(randomForest)
  x <- cbind(x_train,y_train)
 # Fitting model
  fit <- randomForest(Species ~ ., x,ntree=500)
  summary(fit)
 #Predict Output 
  predicted= predict(fit,x_test)