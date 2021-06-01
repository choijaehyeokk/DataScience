sample <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/3주/iris.csv",
                   header=TRUE, encoding = "UTF-8")

plot(iris$Petal.Length, iris$Petal.Width,xlab = "Petal길이",ylab = "Petal너비", pch=21, 
     bg=c("red","green3","blue")[unclass(iris$Species)], main="Iris Petal Data")
abline(a=3,b=-1,col="black",lty=50)
abline(a=4.2,b=-0.5,col="black",lty=50)
