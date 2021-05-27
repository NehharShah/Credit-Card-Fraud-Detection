creditcard <- read.csv("creditcard.csv" , header = T ,sep = ",")
str(creditcard)
summary(creditcard)
information.gain()
creditcard.true  <- creditcard[creditcard$Class == 0, ]
creditcard.false <- creditcard[creditcard$Class == 1, ]

#Clustering
#Time
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=Time), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=Time), color="red",
               fill="red", alpah=0.12)

#Amount
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=Amount), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=Amount), color="red",
               fill="red", alpah=0.12)

#scale_x_log10() + annotation_logticks()

#V1
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=V1), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=V1), color="red",
               fill="red", alpah=0.12)

#V10
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=V10), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=V10), color="red",
               fill="red", alpah=0.12)
#V14
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=V14), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=V14), color="red",
               fill="red", alpah=0.12)
#V15
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=V15), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=V15), color="red",
               fill="red", alpah=0.12)


#V24
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=V24), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=V24), color="red",
               fill="red", alpah=0.12)



#V25
library(ggplot2)
ggplot() +
  geom_density(data = creditcard.true,
               aes(x=V25), color="blue",
               fill="blue", alpah=0.12) +
  geom_density(data = creditcard.false,
               aes(x=V25), color="red",
               fill="red", alpah=0.12)

#Preprocessing
colnames(creditcard)[colSums(is.na(creditcard)) > 0]
nrow(creditcard[!complete.cases(creditcard), ])

#Balancing
creditcard$Class <- as.factor(creditcard$Class)
summary(creditcard$Class)

#Decision Tree
#Shuffle
rows <- nrow(creditcard)
cols <- ncol(creditcard)
set.seed(39)
creditcard <- creditcard[sample(rows),1:cols]
#Train
ntr <- as.integer(round(0.8*rows))
creditcard.train <- creditcard[1:ntr, 1:cols]
creditcard.test <- creditcard[(ntr+1):rows, -cols]
creditcard.testc <-creditcard[(ntr+1):rows, cols]

creditcard.testc <- as.data.frame(creditcard.testc)
colnames(creditcard.testc)[1] <- c("Class")

#BuildTree(traning)
library(rpart)
Start.time <- Sys.time()
creditcard.tree <- rpart(Class ~.,
                         data = creditcard.train,
                         method = "class")
print(Sys.time() - Start.time)


#Prediction(test)
creditcard.pred <- predict(creditcard.tree, creditcard.test)
creditcard.testc$Pred <-0L
creditcard.testc$Pred[creditcard.pred[,2] > 0.5] <- 1L
creditcard.testc$Pred <- factor(creditcard.testc$Pred)

#confusion Matrix
library(caret)
confusionMatrix(creditcard.testc$Pred, 
                creditcard.testc$Class)
library(pROC)
creditcard.testc$Class <- ordered(creditcard.testc$Class, levels=c("0","1"))
creditcard.testc$Pred <- ordered(creditcard.testc$Pred, levels=c("0","1"))

auc(creditcard.testc$Class,creditcard.testc$Pred)

#Visualization
library(rpart.plot)
rpart.plot(creditcard.tree, cex = 0.66, extra = 3,
           type = 5, box.palette = "BuRd")
           


#IG & Entropy


summary(info)
str(info)

entropy <- function(target){
  freq <- data.frame(prop.table(table(target)))
  freq$entropy <- -log2(freq$Freq)*freq$Freq
  sum(freq$entropy)
}
bino <- c(rep(1,25),rep(1,25))
entropy(bino)

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