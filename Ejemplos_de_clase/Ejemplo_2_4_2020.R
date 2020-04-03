#Cargar los datos del paquete cars en el objeto autos

datos <- cars
x <- datos$speed
y <- datos$dist

#grafico 

plot(x,y,pch=20)

#Cargo el ggplot2 y grafico

library(ggplot2)
ggplot(data=datos)+
  geom_point(aes(x=speed, y=dist))

#Estimador de la esperanza de X

x_raya<-mean(x) #promedio de los elementos de un vector

s_x<-sd(x) #desvio estandard del vector

#quiero estimar beta0 y beta1

X<-matrix(c(rep(1,50),x),byrow=FALSE,ncol=2)
unos<-rep(1,50)
X<-cbind(unos,x)#otra opcion combino columnas column bind 

t(X) #transpuesta de X
X%*%t(X)#producto matricial
#solve(X) #te da la inversa

beta_sombrero<-solve(t(X)%*%X)%*%t(X)%*%y

beta_sombrero[1] #elemento uno del vector 
#X[1,3]# elemento de la fila 1 coluna 3 de la matriz X 

#grafico 
plot(x,y,pch=20)
y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*x

lines(x,y_sombrero,col="chocolate")#agrego lineas al grafico 


abline(beta_sombrero[1],beta_sombrero[2],col="Forestgreen")#otra forma de hacer la recta

datos2<-data.frame(cbind(datos,y_sombrero))
ggplot(data=datos2)+
  geom_point(aes(x=speed, y=dist))+
  geom_line(aes(x=speed,y=y_sombrero),col="Firebrick")

#ahora la version linda 

ajuste<-lm(y~x) #modelo lineal 

names(ajuste) #metodos del objeto
summary(ajuste) # Resumen copado del objeto 

ajuste$coefficients #atributos del objeto 
