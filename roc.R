install.packages("pROC")
library(pROC)
data(aSAH)
roc1=roc(aSAH$outcome, aSAH$s100b,smooth = T)
auc(roc1)
plot(roc1)