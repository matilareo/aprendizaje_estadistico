library(ggplot2)

#FUNCION PARA ESTIMAR BETAS
estimar_beta <- function(y,...){
  unos<-rep(1,length(y))
  X<-cbind(unos,...)
  beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%y)
}

#DEFINO LOS VECTORES 
x<-rnorm(100)
epsilon<-rnorm(100,0,0.1) #PARA LOS ULTIMOS PUNTOS MODIFICAR LA VARIANZA 

#DEFINO RECTA
y<-(-1)+0.5*x+epsilon
p<-ggplot(data=NULL)+geom_point(aes(x,y))
b0<- -1
b1<- 0.5

#AJUSTE LINEAL
ajuste<-lm(y~x)
ajuste$coefficients
y_lineal<-ajuste$coefficients[1]+ajuste$coefficients[2]*x
p<-p + geom_line(aes(x,y_lineal),col='red')+
  geom_line(aes(x,-1+0.5*x),col='blue')

#AJUSTE CUADRATICO
beta_cuadratico<-estimar_beta(y,x,x*x)
y_cuadratico<- beta_cuadratico[1]+beta_cuadratico[2]*x+beta_cuadratico[3]*x*x

#PLOT DE LOS DATOS CON LAS REGRESIONES
p + geom_line(aes(x,y_cuadratico), col="green")

#CALCULO ERRORES CUADRATICOS MEDIOS Y ME FIJO CUAL ES MAYOR 
mse_lineal<-sum((y-y_lineal)*(y-y_lineal))/length(y)
mse_cuadratico<-sum((y-y_cuadratico)*(y-y_cuadratico))/length(y)
is_bigger<-mse_lineal>mse_cuadratico

#VARIANZA DE LO ESTIMADO 
sd(y)
sd(y_lineal)
sd(y_cuadratico)
