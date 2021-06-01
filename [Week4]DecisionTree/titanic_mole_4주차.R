df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/예습자료/2주/titanic3.csv",header=TRUE, fileEncoding = "UCS-2LE")
df <- data.frame(df$survived,df$sex,df$age,df$sibsp,df$parch,df$fare)
names(df)=c("survived","sex","age","sibsp","parch","fare")
df <- na.omit(df)
df
model <- lm(sex~.,data=df)
summary(model)
library(caret)
set.seed(100) #reproducability setting
intrain<-createDataPartition(y=df$sex, p=0.7, list=FALSE) 
train<-df[intrain, ]
test<-df[-intrain, ]
train
test
library(rpart)
fit <- rpart(sex~sibsp+parch,data=train,cp=-1,minsplit=2,minbucket=1,method = "class")
fit
plot(fit)
text(fit)
plotcp(fit)
printcp(fit)
ptree<-prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(ptree)
rpartpred<-predict(ptree, test, type='class')
rpartpred <- as.factor(rpartpred)
sexpred <- as.factor(test$sex)
library(e1071)
confusionMatrix(rpartpred,sexpred)

