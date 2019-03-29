rand <- function(nmax,x0,a,m){

  re = c()
  x=x0
  for (i in 1:nmax) {
    temp = (a*x)%%m
    x = temp
    re[i] = temp/m
  }
  re
}
re=rand(1000, a=69069, m=2**32, x0=1)
hist(re)



rm(list=ls())
fn = function(as, xs, f_target){
  fhat = function(x){
    as[1]+as[2]*x+as[3]*x**2
  }
  
  (fhat(xs[1])-f_target(xs[1]))**2+(fhat(xs[2])-f_target(xs[2]))**2+(fhat(xs[3])-f_target(xs[3]))**2
}

f_target = function(x){
  # -x*(1-x)
  -x*sin(10*pi*x)
}

pred = function(as,x){
  as[1]+as[2]*x+as[3]*x**2
}
re = optim(fn=fn, par=c(0.1, 0.5, 0.9),
      xs=c(0.1, 0.5, 0.9),
      f_target=f_target)

i = seq(0,1,0.01)
plot(i,f_target(i))
plot(i,pred(re$par,i))
