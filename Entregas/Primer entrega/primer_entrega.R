# Primer entrega - Guia 2 - Ejercicio 7 

library(GGally)

#a)

df<-read.csv("Entregas/Primer entrega/Cemento.csv",header = TRUE,sep = ";")

attach(df)
cemento<-data.frame(cbind(x1,x2,x3,x4,x5,y))


cor(cemento)
ggpairs(cemento)
# EN PRINCIPIO SE PODRIA DECIR QUE LAS VARIABLES MAS SIGNIFICATIVAS 
# SON X2 X3 X4 SIENDO ESTAS LAS DE MAYOR CORRELACION


ajuste <- lm(y~x1+x2+x3+x4+x5,data=cemento)
summary(ajuste)
# LA REGRESION ES SIGNIFICATIVA, YA QUE EL P VALOR ES MUCHO MENOR QUE 0.05
# SE OBSERVA UNA CONTRADICCION ENTRE LOS TEST DE HIPOTESIS POR CADA VARIABLE
# Y LA SIGNIFICACION DE LA REGRESION, NO SE PUEDE ASEGURAR QUE ALGUNA DE LAS 
# VARIABLES SEA DISTINTA DE 0 EN EL CASO DE LOS TEST INDIVIDUALES

#SE PODRIA ELEGIR CIERTAS VARIABLES.

ajuste <- lm(y~x1+x2+x3+x4+x5+0,data=cemento)
summary(ajuste)

#En este caso la regresion es significativa 
#y hay 3 variables que son sifnificativamente distintas de 0 

ajuste <- lm(y~x2+x3+x4+0,data=cemento)
summary(ajuste)

b2<-ajuste$coefficients[1]
b3<-ajuste$coefficients[2]
b4<-ajuste$coefficients[3]
ggpairs(data.frame(cbind(x2,x3,x4,y)))
ya<-b2*x2+b3*x3+b4*x4
ggplot(data=NULL,aes(x2,ya))+geom_point(aes(x2,y),color='red')+
  geom_point(color='black')




