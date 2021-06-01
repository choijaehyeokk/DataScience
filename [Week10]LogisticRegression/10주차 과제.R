library(rpart)
library(e1071)
library(caret)
library(olsrr)
library(digest)
library(DAAG)

df_train <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/10주/boston_data.csv",header=TRUE, fileEncoding = "UTF-8")
str(df_train)

model <- lm(medv~.,data = df_train)
summary(model)
plot(model)

#======================================
#점진선택법
model_forward <- lm(medv ~ ., data = df_train)
k <- ols_step_forward_p(model_forward)
plot(k)
ols_step_forward_p(model_forward, details = TRUE)

start_forward = lm(medv~1,data=train)
simple_forward = step(start_forward, scope = medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat+age+indus,  direction = "forward")

#후진선택법
model_backward <- lm(medv ~ ., data = df_train)
k <- ols_step_backward_p(model_backward)
plot(k)
ols_step_backward_p(model_backward, details = TRUE)
simple_backward = step(model, direction = "backward")
#======================================
model <- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat,data = df_train)
summary(model)
#설명모델생성끝



#예측모델생성

indexes <- createDataPartition(y = df_train$medv, p = .75, list = FALSE)
train <- df_train[indexes, ]
test <- df_train[-indexes, ]

set.seed(100)
model <- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat,data = train)
pred <- predict(model, test)
pred
summary (model)

actuals_preds <- data.frame(cbind(actuals=test$medv, predicteds=pred))
actuals_preds
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy



head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape
#평균 백분율 오차

windows()
cvResults <- suppressWarnings(
  CVlm(data = df_train, 
       form.lm=medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, 
       m=5, 
       dots=FALSE, 
       seed=100, 
       legend.pos="topleft",  
       printit=TRUE
       ));  

cvResults
attr(cvResults, 'ms')
#평균 제곱 오차(MSE)
sqrt(attr(cvResults,'ms'))
#평균 제곱근 오차(RMSE)

actuals_preds <- data.frame(cbind(actuals=cvResults$medv, predicteds=cvResults$Predicted))
actuals_preds
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape
