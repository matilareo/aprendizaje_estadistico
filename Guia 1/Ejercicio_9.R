library(ggplot2)

eje_y <- rnorm(1000)
eje_x <- seq(1,1000)
data <- as.data.frame(cbind(eje_x,eje_y))

ggplot(data,aes(x=eje_x,y=eje_y))+geom_point(fill="green",col="black",pch=1)
ggplot(data,aes(sample=eje_y))+stat_qq()+stat_qq_line()
ggplot(data,aes(x=eje_y,y=eje_x))+geom_boxplot(bins = 100,fill="chocolate",col="black")
boxplot(data)
qqplot(eje_x,eje_y,plot=T)


       
eje_y <- rbinom(1000,10,0.4)
eje_x <- seq(1,1000)
data <- as.data.frame(cbind(eje_x,eje_y))
ggplot(data,aes(x=eje_x,y=eje_y))+geom_point(fill="green",col="black",pch=1)
ggplot(data,aes(sample=eje_y))+stat_qq()+stat_qq_line()
ggplot(data,aes(x=eje_y))+geom_histogram(bins = 100,fill="chocolate",col="black")
boxplot(data)


eje_y <- rchisq(1000,50)
eje_x <- seq(1,1000)
data <- as.data.frame(cbind(eje_x,eje_y))
ggplot(data,aes(x=eje_x,y=eje_y))+geom_point(fill="green",col="black",pch=1)
ggplot(data,aes(x=eje_y))+geom_histogram(bins = 100,fill="chocolate",col="black")
       
boxplot(data$eje_y~data$eje_x)
       
   
eje_y <- rf(1000,90,40)
eje_x <- seq(1,1000)
data <- as.data.frame(cbind(eje_x,eje_y))
ggplot(data,aes(x=eje_x,y=eje_y))+geom_point(fill="green",col="black",pch=1)
ggplot(data,aes(x=eje_y))+geom_histogram(bins = 100,fill="chocolate",col="black")
          
boxplot(data$eje_y)
              
              
eje_y <- rgamma(1000,0,7)
eje_x <- seq(1,1000)
data <- as.data.frame(cbind(eje_x,eje_y))

ggplot(data,aes(x=eje_x,y=eje_y))+geom_point(fill="green",col="black",pch=1)
ggplot(data,aes(x=eje_y))+geom_histogram(bins = 100,fill="chocolate",col="black")
boxplot(data$eje_y)

