library(ggplot2)
rm(list=ls())

#1.1##############################
nd <- function(n){
  theta = runif(n/2,min=0, max=2*pi)
  D = runif(n/2, 0, 1)
  X1 = sqrt(-2*log(D))*cos(theta)
  X2 = sqrt(-2*log(D))*sin(theta)
  return(c(X1,X2))
}
# hist(nd(10000))
# hist(rnorm(10000)) 

gd <- function(n,p){
  re = c()
  for (i in 1:n) {
    g=0
    t=0
    while(g==0){
      g=sample(c(0,1),1,replace=FALSE,prob=c(1-p,p))
      t=t+1
    }
    re[i]=t
  }
  re
}
# hist(gd(10000,0.1))
# hist(rgeom(10000,0.1)) 

#1.2##############################
dt1= data.frame(U=nd(10),V=gd(10,1/3))
n=100
dt2= data.frame(U=nd(n),V=gd(n,1/3))

d1 <- function(a,b){
  dist <- abs(a[1]-b[,1])+abs(a[2]-b[,2])
  dist
}

d2 <- function(a,b){
  dist <- (a[1]-b[,1])^2+(a[2]-b[,2])^2
  dist
}

goal_minimum <- function(y , X, d){
  sum = sum(d(y,X))
  return(sum)
}

p1 = optim(fn=goal_minimum, par = colMeans(dt1), d=d1, X=dt1)$par
optim(fn=goal_minimum, par = colMeans(dt1), d=d2, X=dt1)

p2 = optim(fn=goal_minimum, par = colMeans(dt2), d=d1, X=dt2)$par
optim(fn=goal_minimum, par = colMeans(dt2), d=d2, X=dt2)

#1.3############################
ggplot(dt1,aes(x=U,y=V))+
  geom_point()+
  geom_point(data=as.data.frame(t(p1)),aes(x=U,y=V,shape="b"),size=5)+
  geom_point(data=data.frame(U=median(dt1[,1]),V=median(dt1[,2])),aes(x=U,y=V,shape="a"),size=5)+
  geom_point(data=as.data.frame(t(colMeans(dt1))),aes(x=U,y=V,shape="c"),size=5)

plot(x=dt2$U,y=dt2$V,type = "p")
points(p1[1],p1[2], pch=2,col="Red")
points(median(dt2[,1]),median(dt2[,2]), pch=5,col="DarkBlue")
points(colMeans(dt2)[1],colMeans(dt2)[2],pch=6,col="Orange")

# f-mean is quite like median, especially for V

#2.1###########################
rm(list=ls())
unifn = function(n,min=1,max){
  re = c()
  for(i in 1:n){
    rand = runif(1)
    re[i]=(max-min)*rand/min
  }
  return(re)
}
unifn(100,max=10)

#2.2###########################




