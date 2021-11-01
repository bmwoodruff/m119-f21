i <- 1:3
b11 <- pi
b12 <- log(2)
b21 <- sum(i^2)
b22 <- sum(i-1)
c1 <- 7
c2 <- sum(i-i^2)

x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)
x
y

#Check
b11*x+b12*y == c1
b21*x+b22*y == c2

#Check
b11*x+b12*y - c1
b21*x+b22*y - c2

all.equal(b11*x+b12*y , c1)




n <- 1:44
b11 <- sum(n)
b12 <- sum(3+0*n)
b12
c1 <- sum(7+0*n)
b21 <- sum(5+0*n)
b22 <- sum(n)
c2 <- sum(n^2)


x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)
x
y

#Check
b11*x+b12*y - c1
b21*x+b22*y - c2




library(data4led)
bulb <- led_bulb(1,seed=123)
ti <- bulb$hours
yi <- bulb$percent_intensity


a1 <- .0005254877



f1 <- function(t,a1){100+a1*t}
plot(ti,yi)
lines(ti,f1(ti,a1))
