f2 <- function(x,a0=0,a1=0,a2=1){
  a0 + a1*x + a2*x^2
}

a0 <- 100
a1 <- 0.001190918
a2 <- -1.743522e-07

#We compute f2 at t = 25000
f2(25000,a0,a1,a2)

#We need to solve f(t)=80
#Sounds like uniroot is our friend.

#Define a function we want to find the zero of
temp <- function(x){f2(x,a0,a1,a2)-80}
#now use uniroot. 
#we know the answer is between 0 and 25000
uniroot(temp,c(0,25000))$root





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


library(pdqr)
library(data4led)
bulb <- led_bulb(1,seed = 123)
t <- bulb$hours
y <- bulb$percent_intensity
f <- function(x,a0 = 100, a1 = 0.001190918, a2 = -1.743522e-07){a0+a1*x+a2*x^2}
g <- function(r,mu=0,sigma=1){1/sqrt(2*pi*sigma)*exp(-1/2*((r-mu)/sigma)^2)}
par(mar=c(2.5,2.5,0.25,0.25))

sample_r <- as_r(as_d(g,mu = 0, sigma = 1))(length(t))
plot(t, y, pch = 16)
lines(t, f(t), type = "l")
points(t, f(t) + sample_r,col = "red", pch = 3)

draw_simulation <- function(mu = 0, sigma = 1, a0=100,a1=0.001190918,a2=-1.743522e-07){
  sample_r <- as_r(as_d(g,mu = mu, sigma = sigma))(length(t))
  plot(t,y,pch = 16)
  lines(t,f(t,a0 = a0,a1 = a1,a2 = a2),type = "l")
  points(t,f(t,a0 = a0,a1 = a1,a2 = a2) + sample_r,col = "red", pch = 3)
}
draw_simulation(mu = 0,sigma = 1)


draw_simulation(mu = 0,sigma = 0.5)


draw_simulation(mu = 0,sigma = .5)

draw_simulation(mu = 0,sigma = .45)

