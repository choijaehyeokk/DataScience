library(caret)# 층화추출
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/예습자료/2주/titanic3-prep.csv",header=TRUE, fileEncoding = "UTF-8")

#데이터 프레임 생성
df <- data.frame(df$survived,df$sex,df$sibsp,df$parch,df$groupedage,df$groupedfare)  



#항목 정해줌 기존의 age&fare는 그룹화된 groupedage&fare로 대체되기에 뺐다 
names(df)=c("survived","sex","sibsp","parch","groupedage","groupedfare")  




#null값 있는 행 삭제
df <- na.omit(df)
length(df$survived)


#모든 항목의 선형회귀 분석으로 최적화된 변수 찾기
model <- lm(survived~.,data=df)  
summary(model)

#층화추출

set.seed(100) #reproducability setting
intrain<-createDataPartition(y=df$survived, p=0.5, list=FALSE)



# 트레이닝 셋 설정
train<-df[intrain, ]



# 테스트셋 설정
test<-df[-intrain, ]
train
test

survivpred <- as.factor(test$survived)

# 최적 변수로 fit생성
fit <- rpart(survived~sex+sibsp+groupedfare,data=train,cp=-1,minsplit=2,minbucket=1,method = "class")
plot(fit)
text(fit)
fancyRpartPlot(fit)
rpartpred<-predict(fit, test, type='class')
rpartpred <- as.factor(rpartpred)
confusionMatrix(rpartpred,survivpred)



fit2 <- rpart(survived~. ,data=train,cp=-1,minsplit=2,minbucket=1,method = "class") #모든 변수 포함
plot(fit2)
text(fit2)
fancyRpartPlot(fit2)
rpartpred2<-predict(fit2, test, type='class')
rpartpred2 <- as.factor(rpartpred2)
confusionMatrix(rpartpred2,survivpred)


plotcp(fit)
printcp(fit)
ptree<-prune(fit, cp= fit$cptable[which.min(fit2$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)
fancyRpartPlot(ptree)
rpartpred3<-predict(ptree, test, type='class')
rpartpred3 <- as.factor(rpartpred3)
confusionMatrix(rpartpred3,survivpred)

fitinfo <- rpart(survived~sex+sibsp+groupedfare,data=train,cp=-1,minsplit=2,minbucket=1,method = "class",parms = list(split='information'))
plot(fitinfo)
text(fitinfo)
fancyRpartPlot(fitinfo)
rpartpred4<-predict(fitinfo, test, type='class')
rpartpred4 <- as.factor(rpartpred4)
confusionMatrix(rpartpred4,survivpred)

