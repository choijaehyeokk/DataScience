library(tidyverse)
library(rpart)
library(e1071)
library(caret)
library(nnet)
library(neuralnet)
set.seed(100) 

getModelInfo()
modelLookup("neuralnet")

getwd()
heart<- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/예습자료/8주/Heart.csv",header=TRUE, fileEncoding = "UTF-8")
heart2<- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/7주/heart.csv",header=TRUE, fileEncoding = "UCS-2LE")

str(heart)
data <- heart
data

data %>% filter(is.na(Slope))
data %>% filter(is.na(Ca))
data <- na.omit(data)
is.na(data)
str(data)

#회귀분석을 통해서 유의한 변수 추출
model0 <- lm(target~.,data = heart)
summary(model0)
#sex,cp,ca,thal만 유의함.

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

# split the data into tr and ts sets 
set.seed(4) # random seed
indexes = createDataPartition(ds$AHD, p = .7, list = F)
train = ds[indexes, ]
test = ds[-indexes, ]
train
test


# base model rpart


fit = rpart(AHD~., 
            data = train
)

printcp(fit)
library(rattle)
fancyRpartPlot(fit)

pred = predict(fit, test, type = "class" )
print(data.frame(test, pred))
confusionMatrix(pred, test$AHD)

# caret train method

#여기 아래부터는 변수 4가지로 확인해보겠음.
#=====================================================================================
ds2<- data.frame(ChestPain,Sex,Ca,Thal,AHD)
str(ds2)


preProcValues = preProcess(ds2) 
ds2 <- predict(preProcValues, ds2)
summary(ds2)

set.seed(4) # random seed
indexes = createDataPartition(ds2$AHD, p = .7, list = F)
train2 = ds2[indexes, ]
test2 = ds2[-indexes, ]
train2
test2


# base model rpart


fit2 = rpart(AHD~., 
            data = train2
)

printcp(fit2)
fancyRpartPlot(fit2)

pred2 = predict(fit2, test, type = "class" )
print(data.frame(test2, pred2))
confusionMatrix(pred2, test2$AHD)
#=====================================================================================

# optimize model - rough
modelLookup("nnet")

# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(AHD ~.,
              data = train2,
              method = 'nnet',
              maxit = 100,
              metric = 'Accuracy',
              trControl = trControl,
              tuneLength = 3,
              na.action=na.exclude
)

model
model$finalModel
model$finalModel$convergence 

# Model Evaluation
# Predict testing set
pred <- predict(model, test, type = "raw") 
pred <- factor(pred, levels = c("Yes","No" ))
pred
print(data.frame(test$AHD, pred2))
confusionMatrix(pred, test$AHD)
#=====================================================================================
#4개의 유효데이터로만
model2 = train(AHD ~.,
              data = train2,
              method = 'nnet',
              maxit = 500,
              metric = 'Accuracy',
              # preProcess = c('center', 'scale'), # data normalization
              # We dont need to this, because the data is already scaled
              
              trControl = trControl,
              #tuneGrid=grid
              tuneLength = 3,
              na.action=na.exclude
)
model2
model2$finalModel
model2$finalModel$convergence 

pred2 <- predict(model2, test2) 
pred2 <- factor(pred2, levels = c("Yes","No" ))
pred2
print(data.frame(test2$AHD, pred2))
confusionMatrix(pred2, test2$AHD)


#=====================================================================================

# grid optimization
tuneGrid = expand.grid(size = 16:18, decay = 10 ** (-5:-3))
tuneGrid
trControl = trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
model = train(AHD ~.,
              data = train2,
              method = 'nnet',
              maxit = 1000,
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid=tuneGrid
)
model

pred3 <- predict(model, test2) 
pred3
print(data.frame(test2$AHD, pred3))
confusionMatrix(pred3, test2$AHD)

#=====================================================================================
#Plot Variable performance
X <- varImp(model$finalModel)
X
plot(X)

# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(AHD ~.,
              data = train2,
              method = 'nnet',
              maxit = 500,
              metric = 'Accuracy',
              trControl = trControl,
              #tuneGrid=grid
              tuneLength = 3
)

model

model$finalModel

model$finalModel$convergence 

pred4 <- predict(model, test2) 
pred4
print(data.frame(test2$AHD, pred4))
confusionMatrix(pred4, test2$AHD)

# grid  optimization
tuneGrid = expand.grid(size = 16:18, decay = 10 ** (-5:-3))
tuneGrid
trControl = trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
model = train(AHD ~.,
              data = train2,
              method = 'nnet',
              maxit = 1000,
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid=tuneGrid
)
model

model$finalModel

model$finalModel$convergence 

pred5 <- predict(model, test2) 
pred5
print(data.frame(test2$AHD, pred5))
confusionMatrix(pred5, test2$AHD)



ds3<- data.frame(Sex,Ca,AHD)
ds3
preProcValues = preProcess(ds3) 
ds3 <- predict(preProcValues, ds3)
summary(ds3)

set.seed(4) # random seed
indexes = createDataPartition(ds3$AHD, p = .7, list = F)
train3 = ds3[indexes, ]
test3 = ds3[-indexes, ]
train3
test3
nnet_model1 <- neuralnet(AHD~., data=train3, hidden=1, threshold=0.01,stepmax = 1000)
# threshold : 에러의 감소분이 threshold 값보다 작으면 stop
# hidden : hidden node 수. 
# hidden=c(2,2) : hidden layer 2개가 각각 hidden node 2개를 가짐
# linear.output: 활성함수('logistic' or 'tanh')가 출력 뉴런에 적용되지 않아야 하는 경우(즉, 회귀) TRUE로 설정(default)
# stepmax: 훈련 수행 최대 횟수
plot(nnet_model1)
