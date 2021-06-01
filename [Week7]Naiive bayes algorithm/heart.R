library(e1071)
library(caret)
library(ggplot2)
df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/7주/Heart.csv",header=TRUE, fileEncoding = "UCS-2LE")
df
cov(df) #공분산
cor(df) #상관계수

#여기는 주성분분석
heart.pin <- princomp(df,cor=TRUE)
summary(heart.pin)
screeplot(heart.pin,npcs = 4,type = "lines")
loadings(heart.pin)

#회귀분석
model0 <- lm(target~age+sex+cp+trestbps+chol+fbs+restecg+exang+slope+ca+thal,data = df)
summary(model0)
model0 <- step(model0,direction = "backward")
model_lmnew <- lm(target~sex + cp + trestbps + restecg + thalach + exang + oldpeak + 
                     slope + ca + thal,data = df)
summary(model_lmnew)

#목표변수 변환
df$target <- factor(df$target, levels = c(0,1), labels = c("zero", "one"))
str(df)
summary(df)

#나이에 따른 그래프
ggplot(df, aes(age, colour = target)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")


#데이터 분할
set.seed(1234) 
intrain<-createDataPartition(y=df$target, p=0.7, list=FALSE) 
train<-df[intrain, ]
test<-df[-intrain, ]
print(table(train$target))
print(table(test$target))


#나이브 베이즈 모델 생성
model <- naiveBayes(target~sex + age+sex+cp+trestbps+chol+fbs+restecg+exang+oldpeak+thalach+ca+thal,data = train)
model
summary(model)

#예측
pred <- predict(model, test, type='class')
pred
confusionMatrix(pred, test$target)

#판별별
target <- data.frame(age=24,sex=1, cp=0, trestbps=130,chol=120,fbs=0,restecg=0,thalach=170,exang=1,oldpeak=0.2,slope=1,ca=0,thal=2)
target2 <- data.frame(age=43,sex=1, cp=0, trestbps=120,chol=177,fbs=0,restecg=0,thalach=120,exang=1,oldpeak=2.5,slope=1,ca=0,thal=3)

predict(model, newdata=target)
predict(model, newdata=target2)


