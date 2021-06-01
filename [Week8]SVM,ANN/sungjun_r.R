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


##########
## nnet ##
##########
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
              maxit = 10000,
              trControl = fitControl,
              trace = FALSE,
              tuneGrid = expand.grid(size=c(1), decay=c(0.001, 0.01, 0.1, 0.2, 0.3))
              )


# plot
windows()
plot(nnet)
dev.off()
nnet$bestTune


# nnet plot
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
windows()
plot.nnet(nnet)
dev.off()


test$nnet <- predict(nnet, newdata = test)
conf_nnet <- confusionMatrix(test$AHD, test$nnet, positive = 'Yes')
acc_nnet <- 100 * conf_nnet[["overall"]][["Accuracy"]]
sens_nnet <- 100 * conf_nnet[["byClass"]][["Sensitivity"]]
spec_nnet <- 100 * conf_nnet[["byClass"]][["Specificity"]]
acc_nnet
sens_nnet
spec_nnet
###################
## decision tree ##
###################
dt_model <- train(AHD~.,
                  data = train,
                  method = "rpart2",
                  metric = "ROC",
                  tuneLength = 10,
                  trControl = fitControl)
# depth plot
windows()
plot(dt_model)
dev.off()


# 최적 깊이 찾기
bestdepth <- dt_model$results[which.max(dt_model$results[,"ROC"]),"maxdepth"]
bestdepth

dt <- rpart(AHD~., data = train, maxdepth = bestdepth)


windows()
fancyRpartPlot(dt)
dev.off()


test$dt <- predict(dt, newdata = test, type = "class")
conf_dt <- confusionMatrix(test$AHD, test$dt, positive = 'Yes')
acc_dt <- 100 * conf_dt[["overall"]][["Accuracy"]]
sens_dt <- 100 * conf_dt[["byClass"]][["Sensitivity"]]
spec_dt <- 100 * conf_dt[["byClass"]][["Specificity"]]


#########
## svm ##
#########

svm <- train(AHD~., 
             data = train, 
             method = "svmLinear2",
             metric = "ROC",
             preProcess = c("center","scale"),
             trControl = fitControl
             #tuneGrid = expand.grid(C = seq(0, 2, length = 20))
             )


plot(svm)
svm$bestTune


test$svm <- predict(svm, newdata = test)
conf_svm <- confusionMatrix(test$AHD, test$svm, positive = 'Yes')
acc_svm <- 100 * conf_svm[["overall"]][["Accuracy"]]
sens_svm <- 100 * conf_svm[["byClass"]][["Sensitivity"]]
spec_svm <- 100 * conf_svm[["byClass"]][["Specificity"]]
acc_svm
sens_svm
spec_svm
conf_svm
tuneGrid = expand.grid(cost = 0.25)
svm <- train(AHD~., 
             data = train, 
             method = "svmLinear2",
             metric = "ROC",
             preProcess = c("center","scale"),
             trControl = fitControl,
             tuneGrid = tuneGrid)

test$svm <- predict(svm, newdata = test)
conf_svm <- confusionMatrix(test$AHD, test$svm, positive = 'Yes')
acc_svm <- 100 * conf_svm[["overall"]][["Accuracy"]]
sens_svm <- 100 * conf_svm[["byClass"]][["Sensitivity"]]
spec_svm <- 100 * conf_svm[["byClass"]][["Specificity"]]
acc_svm
sens_svm
spec_svm
conf_svm
pred <- predict(svm, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc2 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc2
acc_svm2 <- 100 * acc2[["overall"]][["Accuracy"]]
sens_svm2 <- 100 * acc2[["byClass"]][["Sensitivity"]]
spec_svm2 <- 100 * acc2[["byClass"]][["Specificity"]]
acc_svm2
sens_svm2
spec_svm2
conf_svm
svm <- train(AHD~., 
             data = train, 
             method = "svmRadial",
             metric = "ROC",
             preProcess = c("center","scale"),
             trControl = fitControl
             #tuneGrid = expand.grid(C = seq(0, 2, length = 20))
)
pred <- predict(svm, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc3 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc3
svm$bestTune
svm <- train(AHD~., 
             data = train, 
             method = "svmPoly",
             metric = "ROC",
             preProcess = c("center","scale"),
             trControl = fitControl
)
pred <- predict(svm, test) 
svm$bestTune
pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc4 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc4

result <- tibble(Model = c('SVM Linear2', 
                           'SVM Linear2 add cost',
                           'SVM Radial',
                           'SVM POly'
                           ),
                 Accuracy = c(acc_svm, 
                              acc2*100, 
                              acc3*100,
                              acc4*100)
)
result
