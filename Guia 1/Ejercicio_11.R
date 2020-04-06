#Ejercicio 11

estimar_beta <- function(x,y){
  unos<-rep(1,length(x))
  X<-cbind(unos,x)
  beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%y)
}



