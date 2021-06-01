df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스
               /과제/3주/아프리카코끼리무게.csv",header=TRUE, fileEncoding = "UCS-2LE")
df
head(df)
summary(df)
value_m <- df$수
value_f <- df$암
mhist<-hist(value_m, rnorm(49,5179,1),main="아프리카 코끼리 무게",
            xlab="무게",ylab="마리수",col=rgb(0,0,1,0.5),breaks=c(seq(2800,6100,by=100)),xaxt="n")
fhist<-hist(value_f, xaxt='n',rnorm(49,3590,1),main="아프리카 코끼리 무게",
            xlab="무게",ylab="마리수",col=rgb(1,0,0,0.5),breaks=c(seq(2800,6100,by=100)),xaxt="n",add=TRUE)
axis(side=1, at=seq(2800,6100,100), labels=seq(2800,6100,100))
