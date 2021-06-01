library(tidyverse)
library(rpart)
library(e1071) # svmLinear
library(caret)
library(kernlab)
set.seed(100)
names(getModelInfo("svm"))
heart<- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/예습자료/8주/Heart.csv",header=TRUE, fileEncoding = "UTF-8")
str(heart)
data <- heart
data
data %>% filter(is.na(Slope))
data %>% filter(is.na(Ca))
data <- na.omit(data)
is.na(data)
str(data)
ChestPain<- factor(data$ChestPain, levels = c("typical", "asymptomatic", "nontypical", "pier","nonanginal" ))
Age <- as.numeric(data$Age)
Sex <- as.numeric(data$Sex)
RestBp <- as.numeric(data$RestBP)
Chol <- as.numeric(data$Chol)
Fbs <- as.numeric(data$Fbs)
RestECG <- as.numeric(data$RestECG)
MaxHR <- as.numeric(data$MaxHR)
Oldpeak <- as.numeric(data$Oldpeak)
Slope <- as.numeric(data$Slope)
Ca <- as.numeric(data$Ca)
Thal <- factor(data$Thal,levels = c("fixed","normal","reversable"))
AHD <- factor(data$AHD,levels=c("Yes","No"))

ds<- data.frame(ChestPain,Age,Sex,RestBp,Chol,Fbs,RestECG,MaxHR,Oldpeak,Slope,Ca,Thal,AHD)
str(ds)

preProcValues = preProcess(ds) 
ds <- predict(preProcValues, ds)
summary(ds)

set.seed(4) # random seed
indexes = createDataPartition(ds$AHD, p = .7, list = F)
train = ds[indexes, ]
test = ds[-indexes, ]
train
test
#의사결정나무 생성
fit = rpart(AHD~., 
            data = train
)
printcp(fit)
library(rattle)
fancyRpartPlot(fit)

pred = predict(fit, test, type="class")
print(data.frame(test, pred))
confusionMatrix(pred, test$AHD)
acc_rpart <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc_rpart
#여기까지 의사결정나무 생성 및 정확도 추출
names(getModelInfo("svm"))
#svm 최적화 함수
#=================================================================================
#svm 최적화 - 1
modelLookup("svmLinear2")
#1 - svmLinear2로 최적화
trControl <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model <- train(AHD ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                tuneLength = 10,
                trControl = trControl
)
model
plot(model)

model$finalModel

pred <- predict(model, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc1 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc1
#grid를 활용하여 더 최적화
tuneGrid = expand.grid(cost = 10**(-4:2))
trControl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 2, 
                          returnResamp = 'final')
model <- train(AHD ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                trControl = trControl,
                tuneGrid = tuneGrid
)
model
plot(model)

model$finalModel

pred <- predict(model, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc2 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc2
#=================================================================================
#svm 최적화 - 2
modelLookup("svmRadial")
#2 - svmRadial로 최적화
model <- train(AHD ~.,
               data = train,
               method = 'svmRadial',
               metric = 'Accuracy',
               tuneLength = 10,
               trControl = trControl
)
model
plot(model)

model$finalModel

pred <- predict(model, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc3 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc3
#grid를 활용하여 더 최적화
tuneGrid = expand.grid(cost = 10**(-4:2))
trControl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 2, 
                          returnResamp = 'final')
model <- train(AHD ~.,
               data = train,
               method = 'svmRadial',
               metric = 'Accuracy',
               trControl = trControl,
               tuneGrid = tuneGrid
)
model
plot(model)

model$finalModel

pred <- predict(model, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc
#=================================================================================
#svm 최적화 - 3
modelLookup("svmPoly")
#2 - svmRadial로 최적화
model <- train(AHD ~.,
               data = train,
               method = 'svmPoly',
               metric = 'Accuracy',
               tuneLength = 10,
               trControl = trControl
)
model
plot(model)

model$finalModel

pred <- predict(model, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc4 <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc4
#grid를 활용하여 더 최적화
tuneGrid = expand.grid(cost = 10**(-4:2))
trControl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 2, 
                          returnResamp = 'final')
model <- train(AHD ~.,
               data = train,
               method = 'svmRadial',
               metric = 'Accuracy',
               trControl = trControl,
               tuneGrid = tuneGrid
)
model
plot(model)

model$finalModel

pred <- predict(model, test) 

pred
print(data.frame(test$AHD, pred))

confusionMatrix(pred, test$AHD)
acc <- confusionMatrix(pred, test$AHD)$overall['Accuracy']
acc


result <- tibble(Model = c('SVM Linear', 
                           'SVM Linear w/choice of cost',
                           'SVM Radial',
                           'SVM POly',
                           'RPART'),
                 Accuracy = c(acc1, 
                              acc2, 
                              acc3,
                              acc4,
                              acc_rpart)
)
result
