n=10000
set.seed(123456)
x_rand <- runif(n=n, min=0, max=1)
data1 <- data.frame(unif=x_rand)
# the inverse laplace with mu = 0.5
laplace_distribution = function(a, b, p){
  # result <- mu-(1/alpha)*sign(p-0.5)*log(1-2*abs(p-0.5))
  result <- b*((1-p)^(-1/a))
  return(result)
}
a=laplace_distribution(3,1,data1)
hist(a[,1])


f=function(x){
  
  a0+a1*x+a2*x**2
}

optim(fn=f)