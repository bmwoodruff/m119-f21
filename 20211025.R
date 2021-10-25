V <- function(x){x^2*(6-x^2)/(4*x)}
x <- seq(0,sqrt(6),0.1)
plot(x,V(x),type = "l")

a <- 1.41421
b <- 1.41423 
width <- (b-a)/100
x <- seq(a,b,width)
plot(x,V(x),type = "l")

sqrt(2)

DV <- function(x){3/2 - 3/4*x^2}
uniroot(DV,c(0,2))$root
