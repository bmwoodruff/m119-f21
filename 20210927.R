x <- seq(0,100,0.1)
Q <- function(x){3*(1/2)^x+2}
f <- function(x){(1/2)^x}
 
plot(x,Q(x),ylim = c(0,5), type='l')
abline(h=2,col = 'red')
abline(v=0,col = 'gray')
abline(h=0,col = 'grey')
points(x,f(x),type='l')


log(100)
exp(1)
log(100,10)
log(10)
