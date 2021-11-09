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


