#Ejercicio 10 
library(ggplot2)

df<- read.csv("Guia 1/data_frames/graduados.txt")
names(df)<-c("graduados")
df$index<-c(1:length(x))


x<-df$graduados
y<-df$index


mean_x<-mean(x)
median_x<-median(x) #el 50% de los valores son menores o iguales a este valor
mean_10_x<-mean(x,trim=0.10)

s_x<-sd(x)
cuartil_x<-IQR(x) #rango intercuartil, diferencia entre el primero y el tercero 
mad_x<-mad(x)

ggplot(df, aes(x=names(df)[1],y=graduados))+geom_boxplot()

df$estimacion<- rnorm(29,mean =mean_x,sd=s_x)

boxplot(df$graduados,df$estimacion)

