#devtools::install_github("echasnovski/pdqr")
library(pdqr)


f_deterministic <- function(x,a0=2,a1=0.1){a0+a1*x}
g_stochastic <- function(r,mu=0,sigma=1){1/sqrt(2*pi*sigma)*exp(-1/2*((r-mu)/sigma)^2)}

r <- seq(-3,3,0.001)
plot(r,g_stochastic(r))

n <- 100
set.seed(123)
g_sample <- rnorm(n,mean=0,sd=1)


x<- seq(1,n,1)
plot(x,f_deterministic(x,a0=2,a1=0.1),type = "l")
points(x,f_deterministic(x)+g_sample)




# Custom functions
my_d <- function(x) {
  ifelse(x >= -1 & x <= 1, 0.75 * (1 - x^2), 0)
}
x <- seq(-1,1,0.1)
plot(x,my_d(x))
as_d(my_d)
