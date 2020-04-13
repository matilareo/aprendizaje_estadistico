#Ejercicio 13 

df<- read.csv("Guia 1/data_frames/abalone.txt",sep=",",header = FALSE)

names(df)<- c('Sexo(M,F,I)','Longitud(mm)','Diametro(mm)','Altura(mm)','Peso completo(gr)',
              'Peso de la carne(gr)','Peso de las visceras(gr)','Peso del caparazon(gr)'
              ,'Anillos(int)')
#a)
x<-df$`Diametro(mm)`
y<-df$`Longitud(mm)`

ajuste<-lm(y~x)
A<-ajuste$coefficients[2]
B<-ajuste$coefficients[1]

p<- ggplot(df)
p + geom_point(aes(x,y)) + geom_line(aes(x,A*x+B), col= 'red')

#b)

estimar_beta <- function(y,...){
  unos<-rep(1,length(y))
  X<-cbind(unos,...)
  beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%y)
}

z<-df$`Peso de las visceras(gr)`
y<-df$`Peso completo(gr)`
x1<-df$`Peso de la carne(gr)`
x2<-df$`Peso de las visceras(gr)`
x3<-df$`Peso del caparazon(gr)`

betas<- estimar_beta(y,x1,x2,x3)

y_estimado<- betas[1]+betas[2]*x1+betas[3]*x2+betas[4]*x3

ggplot(df)+geom_point(aes(z,y),col='red')+geom_point(aes(z,y_estimado),col='green')

#c)

P<-df$`Peso completo(gr)`
D<-df$`Diametro(mm)`

p<- ggplot(df)+geom_point(aes(D,P))

ajuste<-lm(P~D)
A<-ajuste$coefficients[2]
B<-ajuste$coefficients[1]

p +geom_line(aes(x,A*x+B), col= 'red')

unos<-rep(1,length(P))
X<-cbind(unos,D,D*D)
beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%P)

P_estimado<- beta_sombrero[1]+beta_sombrero[2]*D+beta_sombrero[3]*D*D

p<-p +geom_line(aes(x,A*x+B), col= 'red') + geom_point(aes(x,P_estimado), col= 'red')

X<-cbind(D*D*D)
X
beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%P)
beta_sombrero
P_estimado2<- beta_sombrero[1]*X

p +geom_point(aes(x,P_estimado2),col='green')
