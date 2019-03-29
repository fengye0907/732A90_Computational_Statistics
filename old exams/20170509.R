#1.3######################
n=200
k=199
f1=function(n,k){
  prod(1:n)/(prod(1:k)*prod(1:(n-k)))  
}
# n large k small, prod(1:n) overflow
# Na
f2=function(n,k){
  prod((k+1):n)/prod(1:(n-k))
}
# n large k small, prod((k+1):n) overflow
# Na
f3=function(n,k){
  prod(((k+1):n)/(1:(n-k)))
}
# n large k small, k
# if k large, underflow

#1.2#####################
n=2:200
r1 = sapply(n,f1,k=1)
r2 = sapply(n,f2,k=1)
r3 = sapply(n,f3,k=1)
plot(r3,col="Red",type = "l")
lines(r2,col="Blue")  #171
lines(r1,col="Green")  ##171

#1.1######################
f1(100,0) #  denominator is 0
f1(100,100)#  denominator is 0
f2(100,100)#  denominator is 0
f3(100,100)#  denominator is 0


#2.1#####################################
rm(list=ls())

n=20000
set.seed(123456)
x_rand <- runif(n=n)
data <- data.frame(unif=x_rand)

f = function(y){
  # browser()
  if(y<=1/3){
    result=log(3*y)-1/2
  }else if(y>2/3){
    result=1/2-log(3-3*y)
  }else{
    result=3*y-3/2
  }
  return(result)
}
data$nd=sapply(x_rand,f)
ggplot(data = data, aes(x=nd)) +
  geom_histogram(aes(y=..density..),
                 colour="black",
                 fill="white",
                 bins=100)+
  geom_density(alpha=.2, fill="#FF6666")
# So the data$nd is the sample of F_g distribution

#2.2#####################################

pdf_norm = function(x){
  exp(-(x**2)/2)/(sqrt(2*pi))
}
pdf_g = function(x){
  if(x<=-1/2){
    re = exp(x+1/2)/sqrt(2*pi)
  }else if(x>1/2){
    re = exp(-x*1/2)/sqrt(2*pi)
  }else{
    re = 1/sqrt(2*pi)
  }
}
c <- 1   #by plot pdf_g, the maximum is between (-0.5,0.5), we assume x=1 gets optimal
n2 <- 10000
accept <- c()
for (i in 1:n) {
  if(length(accept)>=n2){
    break()
  }
  # Here we use the sample from F_g to simulate a sample of normal distribution
  Y <- data[i,2]
  u <- runif(1)
  g <- pdf_g(Y)
  f <- pdf_norm(Y)
  if(u<=f/(c*g)){
    accept <- c(accept,Y)
  }
}
data2 <- data.frame(val=accept, id="esti")

data3 <- data.frame(val=rnorm(n=n2,0,1),id ="real")
data3 <- rbind(data2,data3)
ggplot(data = data3, aes(x=val)) +
  geom_histogram(aes(y=..density..),
                 colour="black",
                 fill="white",
                 bins=30)+
  geom_density(alpha=.2, fill="#FF6666")+
  facet_grid(id~.)

reject_rate <- 1-n2/i 
reject_rate   # nearly 0.1

mean(data2$val)  # close to 0
var(data2$val)   # colsoe to 1


t.test(data3[data3$id=="esti",1],data3[data3$id=="real",1])
#  p <0.05
var.test(data3[data3$id=="esti",1],data3[data3$id=="real",1])
#  p <0.05