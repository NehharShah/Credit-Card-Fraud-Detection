#build
library(randomForest)
samp <- as.integer(0.49*ntr)
creditcard.rF <- randomForest(Class~.,
                              data = creditcard.train,
                              ntree = 39,
                              samplesize = samp,
                              maxnodes = 44)

#Predictions
creditcard.pred <- predict(creditcard.rF, creditcard.test)
creditcard.testc$Pred <- creditcard.pred
View(creditcard.pred)                              

#ConfusionMatrix
library(caret)
confusionMatrix(creditcard.testc$Pred, 
                creditcard.testc$Class)
library(pROC)
creditcard.testc$Class <- ordered(creditcard.testc$Class, levels=c("0","1"))
creditcard.testc$Pred <- ordered(creditcard.testc$Pred, levels=c("0","1"))

auc(creditcard.testc$Class,creditcard.testc$Pred)

#MCC
library(MLmetrics)
Precision(creditcard.testc$Class, creditcard.testc$Pred, positive ="1")
Recall(creditcard.testc$Class, creditcard.testc$Pred, positive ="1")

F1_Score(creditcard.testc$Class, creditcard.testc$Pred, positive ="1")

library(mltools)
mcc(creditcard.testc$Class, creditcard.testc$Pred)