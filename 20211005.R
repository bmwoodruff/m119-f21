f <- function(x){
  5+3*exp(2*x)-7
}
uniroot(f,c(-10,10))$root



f1 <- function(x,n=20,p=0.5){
  # x must be an whole number between 0 and n, endpoints included
  factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)
}

x<- seq(0,30,1)
plot(x,f1(x,n=30,p=0.4))

f2 <- function(x,lambda=1){
  # x must be positive
  lambda*exp(-lambda*x)
}

#Here the domain is not just integers. 
#So we'll use 0.1 for our jumps, and use type = "l"
x<- seq(0,20,0.1)
plot(x,f2(x,lambda = 0.8),type="l")


f3 <- function(x,mu=0,s=1){
  (1/sqrt(2*pi*s^2))*exp(-(x-mu)^2/(2*s^2))
}

x<- seq(0,20,0.1)
plot(x,f3(x,mu=5,s=2),type="l")


f4 <- function(x,lambda=1){
  
  out <- rep(0,length(x))
  out[(x > 0)] <- 1 - exp(-lambda*x[(x > 0)])
  
  return(out)
}

x<- seq(0,20,0.1)
plot(x,f4(x,lambda=0.3),type="l")


f5 <- function(x,a=0,b=1){
  
  out <- rep(0,length(x))
  out[(a <= x) & (x <= b)] <- (x[(a <= x) & (x <= b)]-a)/(b-a)
  out[(x > b)] <- 1
  
  return(out)
}

x<- seq(0,20,0.1)
plot(x,f5(x,a=7,b=9),type="l")



library(data4led)
bulb <- led_bulb(1,seed = 1234)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)

f1 <- function(x,a0=0,a1=0){ a0 + a1*x }

x <- seq(-10,800001,2)
yM <- f1(x,a0=101,a1=0.00025)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)


f4 <- function(x,a0=100,a1=0,a2=0){ a0 + a1*x + a2*log(0.005*x+1)}

x <- seq(-10,800001,2)
yM <- f4(x,a0=100,a1=-0.0002,a2=0.9)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)





f.c1 <- function(x){
  4
}

f.c1(-2)
f.c1(-1)
f.c1(0)
f.c1(1)
f.c1(2)

x <- seq(-6,6,2)
f.c1(x)



f.c2 <- function(x){
  4 + 0*x
}

f.c2(-2)
f.c2(-1)
f.c2(0)
f.c2(1)
f.c2(2)

f.c2(x)

x <- seq(-10,10,1)
length(x)
length(f.c1(x)) #only returns 1 thing.
length(f.c2(x))

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c1(x),type='l')

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c2(x),type='l')





