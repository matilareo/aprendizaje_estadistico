#Ejercicio 15

#FUNCION PARA ESTIMAR BETAS
estimar_beta <- function(y,...){
  unos<-rep(1,length(y))
  X<-cbind(unos,...)
  beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%y)
}

#DEFINO MIS VARIABLES
x1<-runif(100)
x2<-0.5*x1+rnorm(100)/10
e<-rnorm(100)

y<-2+2*x1+0.3*x2+e

#PLOTEO X1 EN FUNCION DE X2
p<-ggplot(data=NULL)+geom_point(aes(x1,x2))

#CALCULO CORRELACION ENTRE X1 Y X2
c_x1_x2<-cor(x1,x2)

#ARMO UN MODELO DE AJUSTE PARA Y EN FUNCION DE X1 Y X2

beta<-estimar_beta(y,x1,x2)

y_estimado<-beta[1]+beta[2]*x1+beta[3]*x2

beta[1]

beta[2]

beta[3]

p