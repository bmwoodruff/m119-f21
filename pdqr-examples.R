#devtools::install_github("echasnovski/pdqr")  #Run this line once to install the pdqr package. 
library(pdqr)

f <- function(x,a0=2,a1=0.1,a2=-0.2){a0+a1*x+a2*x^2}
g <- function(r,mu=0,sigma=1){1/sqrt(2*pi*sigma)*exp(-1/2*((r-mu)/sigma)^2)}

x = seq(1,10,0.1)
set.seed(123)
sample_r <-as_r(as_d(g,mu=0,sigma=1))(length(x))

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x,a0=2,a1=0.1,a2=-0.2),type = "l")
points(x,f(x,a0=2,a1=0.1,a2=-0.2)+sample_r,pch=1)

plot(x,f(x,a0=2,a1=2,a2=0),type = "l")
points(x,f(x,a0=2,a1=2,a2=0)+sample_r,pch=1)

library(data4led)
x <- led_bulb(1,seed=123)$hours
sample_r <- as_r(as_d(g,mu=0,sigma=.1))(length(x))
plot(x,f(x,a0=100,a1=0.001190918,a2=-1.743522e-07),type = "l")
points(x,f(x,a0=100,a1=0.001190918,a2=-1.743522e-07)+sample_r)




simulate_deterministic_with_stochastic <- function(x,f,g){
  sample_r <- as_r(as_d(g))(length(x))
  plot(x,f(x),type = "l")
  points(x,f(x)+sample_r)
}

simulate_deterministic_with_stochastic(
  x = seq(1,100),
  function(x){f(x,a0=2,a1=0.1,a2=0)},
  function(x){g(x,mu=0,sigma=2)}
)


simulate_deterministic_with_stochastic(
  x = led_bulb(1,seed=123)$hours,
  function(x){f(x,a0=1,a1=0.0003,a2=-0.0000000924)},
  function(x){g(x,mu=0,sigma=0.1)}
)

bulb <- led_bulb(1,seed=123)
x <- bulb$hours
length(x)
sample_r <- as_r(as_d(g,mu=0,sigma=1))(length(x))
plot(x,f(x,a0=100,a1=0.000524,a2=0),type = "l")
points(x,f(x,a0=100,a1=0.000524,a2=0)+sample_r)

#r <- seq(-3,3,0.001)
#plot(r,g_stochastic(r))
#n <- 100
#set.seed(123)
#g_sample <- rnorm(n,mean=0,sd=1)
#points(x,f_deterministic(x)+g_sample)

#random_g <-as_r(as_d(function(x){g_stochastic(x,mu=0,sigma=1)}))



# Custom functions
my_d <- function(x) {
  ifelse(x >= -1 & x <= 1, 0.75 * (1 - x^2), 0)
}
x <- seq(-1,1,0.1)
plot(x,my_d(x))
hist(as_r(as_d(my_d))(10000))

r_g <- as_r(as_d(my_d))
hist(as_r(as_d(my_d))(1000000))
r_g <-as_r(as_d(function(x){g(x,mu=3,sigma=1)}))
hist(r_g(1000))





#How to use this in class. 
#Given a deterministic model y=f(x), and stochastic model g(r) for the residuals, we should be able to produce something similar to what we observe by sampling residuals (using g), and adding them f(x) for each observed x.  
#If our model choices are reasonable, then sampling in this fashion should provide us with something reasonably similar to what we observed. 
library(pdqr)
library(data4led)

f <- function(x,a0=2,a1=0.1,a2=-0.2){a0+a1*x+a2*x^2}
g <- function(r,mu=0,sigma=1){1/sqrt(2*pi*sigma)*exp(-1/2*((r-mu)/sigma)^2)}

#We can set a seed to make the sampling reproducible. 
set.seed(123)
x = seq(1,10,0.1)
sample_r <- as_r(as_d(g,mu=0,sigma=1))(length(x))

plot(x,f(x,a0=2,a1=0.1,a2=-0.2),type = "l")
points(x,f(x,a0=2,a1=0.1,a2=-0.2)+sample_r,pch=1)

plot(x,f(x,a0=2,a1=2,a2=0),type = "l")
points(x,f(x,a0=2,a1=2,a2=0)+sample_r,pch=1)

#Now let's actually grab the data from a bulb 
library(pdqr)
library(data4led)
bulb <- led_bulb(1,seed = 123)
t <- bulb$hours
y <- bulb$percent_intensity
f <- function(x,a0 = 100, a1 = 0.001190918, a2 = -1.743522e-07){a0+a1*x+a2*x^2}
g <- function(r,mu=0,sigma=1){1/sqrt(2*pi*sigma)*exp(-1/2*((r-mu)/sigma)^2)}
sample_r <- as_r(as_d(g,mu = 0, sigma = 1))(length(t))
par(mar=c(2.5,2.5,0.25,0.25))
plot(t, y, pch = 16)
lines(t, f(t), type = "l")
points(t, f(t) + sample_r,col = "red", pch = 3)
#Run the last 4 lines of code over and over again a few times. 
#Or, we put the last 4 lines of code in a function, and then just call that function several times. 
draw_simulation <- function(mu = 0, sigma = 1, a0=100,a1=0.001190918,a2=-1.743522e-07){
  sample_r <- as_r(as_d(g,mu = mu, sigma = sigma))(length(t))
  plot(t,f(t,a0 = a0,a1 = a1,a2 = a2),type = "l")
  points(t,y,pch = 16)
  points(t,f(t,a0 = a0,a1 = a1,a2 = a2) + sample_r,col = "red", pch = 3)
}
draw_simulation(mu = 0,sigma = 1)
draw_simulation(mu = 0,sigma = 0.5)
draw_simulation(mu = 0,sigma = 0.2)
#What do you notice?







#In class stuff
library(data4led)
bulb <- led_bulb(1,seed=123)
ti <- bulb$hours
yi <- bulb$percent_intensity

sum(ti*(yi-100))
sum(ti^2)

sum(ti*(yi-100))/sum(ti^2)


f1 <- function(t,a1){100+a1*t}
a1 <- 0.0002
plot(ti,yi)
lines(ti,f1(ti,a1))




