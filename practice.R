#This portion of code plots bot the likelihood and loglikelihood for the model f(x,a0,a1)=a0+a1*x. 
library(data4led)
bulb <- led_bulb(1,seed=123)
x <- bulb$hours
y <- bulb$percent_intensity

f <- function(x,a0=100,a1){a0+a1*x}

likelihood <-function(a0=100,a1,seed=123){
  bulb <- led_bulb(1,seed)
  x <- bulb$hours
  y <- bulb$percent_intensity
  residual_dist <- function(x,a0,a1){1/sqrt(2*pi)*exp(-(y-f(x,a0,a1))^2/2)}
  #We use sapply to make sure if a1 is a list of values, then the function returns a list the same length as a1.   
  sapply(a1,function(a1){prod(residual_dist(x,a0,a1))})
}



a0 <- 100
a1 <- seq(0,0.001,0.000001)
approx_cv <- a1[which(likelihood(a0=100,a1)==max(likelihood(a0=100,a1)))]
par(mfrow = c(2,1), mar=c(2.5,2.5,3,0.25))
plot(a1,likelihood(a0=100,a1), type = "l",main='Likelihood')
abline(v=approx_cv,col=2)
plot(a1,log(likelihood(a0=100,a1)),type = "l",main='logLikelihood')
abline(v=approx_cv,col=2)


par(mfrow = c(1,2), mar=c(2.5,2.5,3,0.25))
plot(parm.l,y.logL,type='l',main='logLikelihood',ylim=c(-100,-40))
abline(v=best.l,col=2)
plot(parm.l,y.L,type='l',main='Likelihood')
abline(v=best.l,col=2)
