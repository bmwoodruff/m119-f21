library(data4led)
bulb <- led_bulb(1,seed=123) #Remember to use your assigned seed!

t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t^3)
c.22 <- sum(t^4)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*t^2)
  
best.a2 <- (c.11*b.2 - c.12*b.1)/(c.11*c.22 - c.12^2) 
best.a1 <- (b.1 - c.12*best.a2)/c.11 

best.a1
best.a2

fxx <- -c.11
fyy <- -c.22
fxy <- -c.12

D <- (fxx)*(fyy)-(fxy)^2
D
fxx 

f2 <- function(x,a0=0,a1=0,a2=1){
  a0 + a1*x + a2*x^2
}

a0 <- 100
a1 <- best.a1
a2 <- best.a2

x <- seq(-10,800001,2)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f2(x,a0,a1,a2),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x,a0,a1,a2),col=2)

