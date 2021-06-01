library(readxl)
library(e1071)
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(nnet)
library(ROCR)
library(devtools)
library(reshape)
library(keras)
require(randomForest)
require(pROC)
library(devtools)
library(ROCR)
rm(list=ls())

ht<- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/예습자료/8주/Heart.csv",header=TRUE, fileEncoding = "UTF-8")
ht <- na.omit(ht[,-1])
ht
str(ht)
sex <- as.numeric(ht$Sex)
cp <- factor(ht$ChestPain, levels = c("typical", "nontypical" ,
                                      "asymptomatic", "nonanginal"))
ca <- as.numeric(ht$Ca)
thal <- factor(ht$Thal, levels = c("fixed", "normal", "reversable"))
AHD <- factor(ht$AHD, levels = c("No", "Yes"))
ht <- data.frame(sex, cp, ca, thal, AHD)
str(ht)

preProcValues <- preProcess(ht)
ht <- predict(preProcValues, ht)
summary(ht)


set.seed(1)
indexes <- createDataPartition(y = ht$AHD, p = .75, list = FALSE)
train <- ht[indexes, ]
test <- ht[-indexes, ]

#nnet모델
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 2,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
nnet <- train(AHD~.,
              data = train,
              method = "nnet",
              metric = "ROC",
              preProcess = c("center","scale"),
              maxit = 1000,
              trControl = fitControl,
              trace = FALSE,
              tuneGrid = expand.grid(size=c(1), decay=c(0.001, 0.01, 0.1, 0.2, 0.3))
)
library(devtools)
source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nnet)

confusionMatrix(predict(nnet, newdata = test, type="raw"), test$AHD)
nn_pred1 <- ROCR::prediction(as.numeric(predict(nnet, newdata = test, type="raw")), as.numeric(test$AHD))
nn_model1.roc <- performance(nn_pred1, "tpr", "fpr")
win.graph() 
plot(nn_model1.roc, main='ROC of Test Data')
performance(nn_pred1, measure = "auc")


#결정나무
dt_model <- train(AHD~.,
                  data = train,
                  method = "rpart2",
                  metric = "ROC",
                  tuneLength = 10,
                  trControl = fitControl)
bestdepth <- dt_model$results[which.max(dt_model$results[,"ROC"]),"maxdepth"]
bestdepth

dt <- rpart(AHD~., data = train, maxdepth = bestdepth)
confusionMatrix(predict(dt, newdata = test,type="class"),test$AHD)
nn_pred2 <- ROCR::prediction(as.numeric(predict(dt, newdata = test, type="class")),as.numeric(test$AHD))
nn_pred2
nn_model2.roc <- performance(nn_pred2, "tpr", "fpr") 
win.graph() 
plot(nn_model2.roc, main='ROC of Test Data')
performance(nn_pred2, measure = "auc")

#randomforest
heartforest<-randomForest(AHD ~ ., data=train, ntree=100, importance = T)
heartforest
heartforest$importance
windows()
varImpPlot(heartforest)
heartforest$votes
confusionMatrix(predict(heartforest,newdata = test,type = "class"),test$AHD)
heartforest.roc<-roc(train$AHD, heartforest$votes[,2], direction = "<",levels = c(control = "No", case = "Yes"))
levels(train$AHD)
plot(heartforest.roc,colorize=TRUE)
auc(heartforest.roc,)
