library(caret)
library(rpart)
library(rattle) 
library(parallel)
library(rpart.plot)
df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/예습자료/2주/titanic3.csv",header=TRUE, fileEncoding = "UCS-2LE")
df <- data.frame(df$survived,df$pclass,df$age,df$sex,df$fare)
names(df)=c("survived","pclass","age","sex","fare")
df <- na.omit(df)
df

intrain<-createDataPartition(y=df$survived, p=0.7, list=FALSE) 

train<-df[intrain, ]
test<-df[-intrain, ]
train
test
#==================================================================
survivpred <- as.factor(test$survived)

fit <- rpart(survived~pclass+age+sex+fare,data=train,cp=-1,minsplit=2,minbucket=1,method = "class")
fancyRpartPlot(fit)
#일단 full tree에서의 정확도를 측정
#==================================================================
rpartpred<-predict(fit, test, type='class')
rpartpred <- as.factor(rpartpred)
confusionMatrix(rpartpred,survivpred)
#==================================================================
#여기는 cp최적화를 위한 그래프찍어봄
printcp(fit)
plotcp(fit)
#cp 중에 최적화된 값을 찾음
fit_prune1=prune(fit,cp=0.0014)
fancyRpartPlot(fit_prune1)
#==================================================================
#cp 최적화 (사후가지치기)로 정확도 향상
rpartpred2<-predict(fit_prune1, test, type='class')
rpartpred2 <- as.factor(rpartpred2)
confusionMatrix(rpartpred2,survivpred)
#==================================================================
#MaxDepth 으로 사전가지치기
train$survived<- factor(train$survived, 
                  levels = c(0, 1), 
                  labels = c("zero", "one"))
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
system.time (postprun <- train(survived~., data=train, 
                                method = "rpart2", 
                                tuneLength = 6,
                                trControl = ctrl,
                                metric = "ROC"))

plot(postprun)
fancyRpartPlot(postprun$finalModel)
#==================================================================

rtree_model <- rpart(survived~pclass+age+sex+fare,data=train,method = "class",control = rpart.control(maxdepth = 7))
rpartpred4<-predict(rtree_model, test, type='class')
rpartpre4 <- as.factor(rpartpred4)
confusionMatrix(rpartpred4,survivpred)



#==================================================================
rtree_model2 <- rpart(survived~pclass+age+sex+fare, data=test, control=rpart.control(maxdepth=6))
rtree_model2
rpart.plot(rtree_model2)
rpart.rules(rtree_model2)
fancyRpartPlot(rtree_model2)
#정확도 검증

