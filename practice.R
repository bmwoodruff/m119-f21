#This portion of code plots bot the likelihood and loglikelihood for the model f(x,a0,a1)=a0+a1*x. 
library(data4led)
bulb <- led_bulb(1,123)

f <- function(x,a0=100,a1){a0+a1*x}

likelihood <-function(a0=100,a1){
  x <- bulb$hours
  y <- bulb$percent_intensity
  residual_dist <- function(x,a0,a1){1/sqrt(2*pi)*exp(-(y-f(x,a0,a1))^2/2)}
  #We use sapply to make sure if a1 is a list of values, then the function returns a list the same length as a1.   
  sapply(a1,function(a1){prod(residual_dist(x,a0,a1))})
}

a0 <- 100
a1 <- seq(0,0.001,0.000001)
plot(a1,likelihood(a0,a1), type = "l")
plot(a1,log(likelihood(a0,a1)),type = "l")

