#Ejercicio 11

estimar_beta <- function(y,...){
  unos<-rep(1,length(y))
  X<-cbind(unos,...)
  beta_sombrero<-c(solve(t(X)%*%X)%*%t(X)%*%y)
}



