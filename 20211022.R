f <- function(x){
  sqrt(x^2+(8/x)^2)
}

x<- seq(2,3,0.1)
plot(x, 8/x,xlim=c(-10,10),ylim = c(-10,10),type ='l')

plot(x,f(x),type ='l')
sqrt(8)


x<- seq(-10,10,0.1)
plot(x,f(x),type ='l')

derf <- function(x){
  1/2*(x^2+64*x^(-2))^(-1/2)*(2*x-128*x^(-3))
}
uniroot(derf,c(2,3))$root



