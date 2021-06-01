df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/training1.csv",header=TRUE, fileEncoding = "UCS-2LE")
colnames(df) <- c("mode","score","tem","cov","bon","ans")
df
summary(df)
str(df)
model <- lm(ans~mode+score+tem+cov+bon,data = df)
summary(model)
model1 <- step(model,direction = "backward")
model2 <- lm(ans~mode+score+bon,data = df)
summary(model2)
coef(model2)
pre_dat <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/testing1.csv",header=TRUE, fileEncoding = "UCS-2LE")
pre_dat
colnames(pre_dat) = c("mode","score","bon","ans")
pre_dat
library(sqldf)
pre_dat2 <- sqldf("select mode,score,bon from pre_dat")
pre_dat2
pre <- predict(model2,pre_dat2)
answer <- cbind(pre,pre_dat$ans)
answer
predict(model2,newdata = data.frame(mode=1,score=87,bon=130))
predict(model2,newdata = data.frame(mode=-1,score=100,bon=300))
predict(model2,newdata = data.frame(mode=0,score=60,bon=150))
