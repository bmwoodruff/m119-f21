#Brain Gains 1
library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

sum((y-100)*t)/sum(t^2)


c.11 <- sum(t^2)
c.12 <-  sum(t^3)
b.1 <-  sum((y-100)*t)
c.21 <-  sum(t^3)
c.22 <-  sum(t^4)
b.2 <- sum((y-100)*t^2)
  
y <- (c.11*b.2 - c.21*b.1)/(c.11*c.22 - c.21*c.12)
x <- (b.1-c.12*y)/c.11

y
x



rm(list=ls())
# Florida Tropical Storm Data (2000-2020)
storms <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,3,4,5,7,4,7,13)

L <- function(lambda,x){
  # Remember x must be a whole number.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

logL <- function(lambda,x){
  # Remember x must be a whole number.
  sum(log((lambda^x/factorial(x))*exp(-lambda)))
}


parm.l <- seq(0,10,0.001)
best.l <- sum(storms)/21

y.L <- as.numeric(lapply(parm.l,FUN=L,x=storms))
y.logL <- as.numeric(lapply(parm.l,FUN=logL,x=storms))

par(mfrow = c(1,2), mar=c(2.5,2.5,3,0.25))
plot(parm.l,y.logL,type='l',main='logLikelihood',ylim=c(-100,-40))
abline(v=best.l,col=2)
plot(parm.l,y.L,type='l',main='Likelihood')
abline(v=best.l,col=2)

best.l
mean(storms)
