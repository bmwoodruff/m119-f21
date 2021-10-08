f <- function(t){
  100 - 7*t +0.0003*log(0.005*t+1)
}
uniroot(f,c(-10,20.1))$root


f <- function(t){
  100 - 7*t +0.0003*log(0.005*t+1)-80
}
uniroot(f,c(-10,20.1))$root


f <- function(t){
  100 - 7*t +0.0003*log(0.005*t+1)-80
}
uniroot(f,c(-10,20.1))$root


f <- function(y){
  -2*log(7*y,5)-2
}
uniroot(f,c(0.002,3))$root

f <- function(s){
  (100+s)*exp(-0.05*s)-80
}
uniroot(f,c(0,30))$root

#Yesterday's stuff
#Define Poisson probabilty mass function
p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}
#Now let's use several data points. 
L <- function(lambda,x1=4,x2=4,x3=8){
  # each element of x must be a whole number
  (lambda^(x1+x2+x3))/(factorial(x1)*factorial(x2)*factorial(x3))*exp(-3*lambda)
}

lambda <- seq(5.33333,5.33334,0.000001)
plot(lambda, L(lambda),type = "l")


L2 <- function(lambda,x){
  # each element of x must be a whole number
  prod(lambda^(x))/(factorial(x))*exp(-lambda)
}
L2(4,c(4,4,8))

#This doesn't work because of a vector issue.  
#you can solve it by using lapply, or sapply, or map, or a for loop. 
lambda <- seq(1,10,0.1)
plot(lambda, L2(lambda,c(4,4,8)),type = "l")



rm(list=ls())
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

f2 <- function(x,lambda=1){
  # x must be positive
  lambda*exp(-lambda*x)
}

f3 <- function(x,mu=0,s=1){
  (1/sqrt(2*pi*s^2))*exp(-(x-mu)^2/(2*s^2))
}




rm(list=ls())
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

LE <- function(lambda,x){
  # The elements of x must be positive.
  prod(lambda*exp(-lambda*x))
}

# For simplicity assume sigma is 1.
LN <- function(mu,x){
  prod((1/sqrt(2*pi))*exp(-(x-mu)^2/2))
}


###Parameter Values###
p1 <- seq(0,10,0.1)
p2 <- seq(-10,10,0.001)



###Data Values###
# Florida Hurricane Data (2000-2020)
FL <- c(4,4,8,8,6,8,2,8,7,4,8,6,4,3,3,4,5,7,4,7,13)

# Some Exponential Data
dE <- c(0.45729967, 0.47156107, 1.21461705, 0.20539769, 1.78975399, 0.09095850, 0.64675475, 1.60109333, 1.57752679, 0.01238945)

# Some Normal Data
dN <- c(-3.77117676, -2.91429587, -2.02774901, -0.23984575, -1.41960740, -3.17490528, -3.21755276, -0.06442566, -1.92134953, -0.93160739)


y.LP <- as.numeric(lapply(p1,FUN=LP,x=FL))
y.LP <- sapply(p1,FUN=LP,x=FL)
y.LP
y.LE <- as.numeric(lapply(p1,FUN=LE,x=dE))
y.LN <- as.vector(lapply(p2,FUN=LN,x=dN))

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LP,type='l',main='Poisson Likelihood')

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LE,type='l',main='Exponential Likelihood')

par(mar=c(2.5,2.5,3,0.25))
plot(p2,y.LN,type='l',main='Normal Likelihood')
