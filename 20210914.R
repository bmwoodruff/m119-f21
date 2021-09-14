x <- seq(-2,5)
x
y <- x^2

plot(x,y,type = "l")

#Here we can do one value at a time
x <- 3
y <- 3*x-2
y
#Here we have several
x <- c(3,-1,6)
x
y <- 3*x-2
y
#How about this
f <- function(x){3*x-2}
f(3)
f(-1)
f(6)
f(c(3,-1,6))

f2 <- function(x,y){4*x^2-5*y*sqrt(x+1)}
f2(3,2)
f2(0,-2.1)
f2(-2.1,5)

f5 <- function(x){
  if(x<0){-x}
  else if(x>0){x}
  else NaN
}
f5(1)
f5(-1)
f5(0)

