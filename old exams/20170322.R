library(ggplot2)
rm(list=ls())
#Q1.1####################################
f <- function(x){
  1-(sin(5*x)+cos(7*x))^2-5/(sqrt(2*pi*0.05))*exp(-(x^2)/(2*0.05))
}
x=seq(-10,10,0.001)
y=f(x)
dt <- data.frame(x,y)
ggplot(dt,aes(x=x,y=y))+
  geom_line()

x[which.min(y)]
# It is not at 0, the minimun is 0.039 (-9.123996)

#Q1.2###################################
m <- c("Nelder-Mead","BFGS","CG","SANN")
p <- c(-10,0,10)

result <- as.data.frame(cbind(rep(m,3),p))
result <- result[order(result$V1,result$p),]

set.seed(123)
for(i in 1:4){
  for (j in 1:3) {
    opt <- optim(par=p[j],
                 fn=f,
                 method=m[i])
    result[j+(i-1)*3,3] = opt$par
    result[j+(i-1)*3,4] = opt$value
    result[j+(i-1)*3,5] = opt$counts[1]
  }
}
colnames(result) <- c("method","par_init","par_final","value_final","iter")
# result <- result[order(result$value_final,result$iter),]
result

# Based on the complexity of target function, all the methods work badly.

# However, we initial value closes to 0, all methods can close to optimal value.
# BFGS is the best (24 iters), then CG (33 iters).

#Q1.3###########################################
g <- function(x){
  -2*(sin(5*x)+cos(7*x))*(5*cos(5*x)-7*sin(7*x))-5/sqrt(0.1*pi)*exp(-(x^2)/0.1)*(-1/0.1)*2*x
}

result <- as.data.frame(cbind(rep(m,3),p))
result <- result[order(result$V1,result$p),]
set.seed(123)
for(i in 1:4){
  for (j in 1:3) {
    opt <- optim(par=p[j],
                 fn=f,
                 gr=g,
                 method=m[i])
    result[j+(i-1)*3,3] = opt$par
    result[j+(i-1)*3,4] = opt$value
    result[j+(i-1)*3,5] = opt$counts[1]
  }
}
colnames(result) <- c("method","par_init","par_final","value_final","iter")
result  # SANN is FxxxING good
#           method par_init    par_final value_final  iter
# 10        BFGS      -10 -10.84432983  -1.5573066    37
# 2         BFGS        0   0.03945313  -9.1240259    24
# 6         BFGS       10  11.14678955  -1.5573066    35
# 7           CG      -10  12.66391864  -0.5485736    22
# 11          CG        0   0.03947210  -9.1240260    20
# 3           CG       10   9.52232600  -0.5485736    12
# 1  Nelder-Mead      -10  -0.91260685  -2.9358449   140
# 5  Nelder-Mead        0   0.03947210  -9.1240260   169
# 9  Nelder-Mead       10   5.37044348  -2.9336929   163
# 4         SANN      -10   0.04923084  -9.1109561 10000
# 8         SANN        0   0.06726112  -9.0173565 10000
# 12        SANN       10   0.05287848  -9.0993148 10000

#Q1.4#################################################
crossover = function(x,y){
  kid = (x+y)/2
  return(kid)
}
mutate = function(x){
  return(x^2 %%30)
}


set.seed(1234567)
func4 <- function(pars, animation = F){
  maxiter = pars$maxiter
  mutprob = pars$mutprob
  tX <- X
  for(i in 1:maxiter) {
    samples <- sample(tX, 2, replace = F)
    id <- which.max(f(tX))
    kid <- crossover(samples[1],samples[2])
    if(runif(1)<mutprob){
      kid <- mutate(kid)
    }
    tX[id] <- kid
    tX <- sort(tX)
    if(animation){ # Here we can see the change step by step
      plot(tX,f(tX),type = "b",xlim=c(-10,10), ylim = c(-10,1),col="Blue")
      # print(tX)
      lines(x=x,y=y)
      Sys.sleep(0.1)
    }
  }
  tX
}
X <- seq(-10,10)[-11]
pars <- data.frame(maxiter=100,mutprob=0.8)
func4(pars,FALSE)


#Q2.1###########################################################################
rm(list=ls())
# set.seed(12345)

f <- function(n){
  es = c()
  for (i in 1:n) {
    # browser()
    theta = runif(1,0,2*pi)
    D = runif(1)
    X_1 = sqrt(-2* log(D))*cos(theta)    # ln(x)
    X_2 = sqrt(-2* log(D))*sin(theta)    # epsilon
    
    x = exp(X_1)
    y = exp(0.5+1.5*X_1+X_2)
    es[i] = y/x
  }
  c(mean(es),var(es))
}
x=seq(100,20000,100)
y=sapply(x, f)
z=rep(exp(9/8),length(x))
dt = data.frame(x,y=y[1,],z) # mean
dt = data.frame(x,y=y[2,],z) # var

ggplot(dt,aes(x=x,y=y))+
  geom_line()+
  geom_smooth()
# the mean is convergent to 3.08
# the var is convergent to 20


#Q2.3###########################################################################
t.test(y, alternative=c("two.sided"))
# p<0.05
# we cannot reject null hypothesis. so it is unbias