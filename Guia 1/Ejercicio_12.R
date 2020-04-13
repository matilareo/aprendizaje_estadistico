#Ejercicio 12 
library(ggplot2)

estimar_beta <- function(x,y){
  unos<-rep(1,length(x))
  X<-cbind(unos,x)
  beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%y)
}

df<-read.csv("Guia 1/data_frames/girasol.txt",sep = " ")

x<-df$inversion
y<-df$rinde

plot<-ggplot(df,aes(x,y))+geom_point()+labs(x='Inversion',y='Rinde')

beta<-estimar_beta(x,y)


y_sombrero<-beta[1]+beta[2]*x

plot + geom_line(aes(x,y_sombrero),col='red')

ajuste<-lm(y~x)
names(ajuste)
summary(ajuste)

ajuste$coefficients
