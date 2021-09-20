f <- function(x){x^2}

rm(list=ls())

f1 <- function(x){
  sqrt(3-x)
}

f1(3)
f1(0)
f1(-100)
f1(10)

x <- seq(-10,3,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f1(x),type='l')
plot(x,f1(x))

f.c1 <- function(x){
  4
}
f.c1(seq(1,100,1))
f.c1(-2)
f.c1(-1)
f.c1(0)
f.c1(1)
f.c1(2)

x <- seq(-6,6,2)
x
f.c1(x)

f.c2 <- function(x){
  4 + 0*x
}
f.c2(seq(1,100,1))
f.c2(-2)
f.c2(-1)
f.c2(0)
f.c2(1)
f.c2(2)

f.c2(x)
f.c1(x)

x <- seq(-10,10,1)
x
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c1(x),type='l')

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c2(x),type='l')


f.quad <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quad(-2)
f.quad(-2,c=3,a=2,b=-3)
f.quad(-2,a=3,b=2,c=-3)
f.quad(-2,3,2,-3)
f.quad(-1)
f.quad(0)
f.quad(1)
f.quad(2)

x <- seq(-2,2,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.quad(x),type='l')



f.quad1 <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quad2 <- function(x,a,b,c){
  a*x^2 + b*x + c
}

f.quad1(1/2)
f.quad2(1/2)

f.quad1(0,1,2,7)
f.quad2(0,1,2,7)

f.quad1(-1/3)
f.quad2(-1/3,1,0,0)



x <- seq(-2,2,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.quad1(x,1,2,3),type='l')
plot(x,f.quad2(x,1,2,3),type='l')
