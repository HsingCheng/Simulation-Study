rm(list=ls())
graphics.off()
########
# HW8-(a)
t1<-proc.time()
t<-0
wt<-numeric(100000)
k<-0
n<-0
situation<-0
arrivaltime<-0
customer<-0
servicetime<-0
X<-c()
seed<-46
LLG<-function(z0,n){
  u<-numeric(n)
  for(i in 1:n)
  {
    zi<-z0
    z0<-(97336*zi)%%(2^31-1)
    zn<-z0
    u[i]<-zn/(2^31-1)
  }
  return(list('u'=u,'z0'=zn))
}

service<-function(z0,n){
  lamda<-5
  seed<-z0
  u1<-LLG(seed,n)$u
  return(-log(u1)/lamda)
}

arrival<-function(z0,n){
  lamda<-4
  seed<-z0
  u1<-LLG(seed,n)$u
  return(-log(u1)/lamda)
}


conv<-FALSE
while(!conv){
  seed<-LLG(seed,sample(10,1))$z0
# cat("now time is:",t,"now n is:",n,"now customer in system is:",customer,"now X is:",X,"\n")
# cat("now arrivaltime is:",arrivaltime,"servicetime is:",servicetime,"\n")
if(n==0){
    n<-n+1
    customer<-customer+1
    servicetime1<-service(seed,1)
    X<-c(X,servicetime1)
    seed<-LLG(seed,sample(10,1))$z0
    arrivaltime<-arrival(seed,1)
} else if(n<10000){
if(length(X)==0){servicetime<-1000}else{servicetime<-X[1]}
   if(servicetime<arrivaltime){
    customer<-customer-1
    mt<-servicetime
    arrivaltime<-arrivaltime-mt
    X<-X-mt
    X<-X[-1]                                                                  
}else {
    n<-n+1
    customer<-customer+1
    mt<-arrivaltime
    arrivaltime<-arrival(seed,1)
    seed<-LLG(seed,sample(10,1))$z0
    X<-X-mt
    if(length(X)==0){b<-0}else{b<-X[length(X)]}
    servicetime1<-b+service(seed,1)
    X<-c(X,servicetime1)
   }
} else if(n==10000){
if(length(X)==0){servicetime<-1000}else{servicetime<-X[1]}
  if(servicetime<arrivaltime){
    customer<-customer-1
    mt<-servicetime
    arrivaltime<-arrivaltime-mt
    X<-X-mt
    X<-X[-1]
  }else {
    n<-n+1
    k<-k+1
    customer<-customer+1
    mt<-arrivaltime
    arrivaltime<-arrival(seed,1)
    seed<-LLG(seed,sample(10,1))$z0
    X<-X-mt
    if(length(X)==0){b<-0}else{b<-X[length(X)]}
    servicetime1<-b+service(seed,1)
    X<-c(X,servicetime1)
    wt[k]<-servicetime1
  }
}else if(n<110000){
  if(length(X)==0){servicetime<-1000}else{servicetime<-X[1]}
  if(servicetime<arrivaltime){
    customer<-customer-1
    mt<-servicetime
    t<-t+mt
    arrivaltime<-arrivaltime-mt
    X<-X-mt
    X<-X[-1]
  }else {
    n<-n+1
    k<-k+1
    customer<-customer+1
    mt<-arrivaltime
    t<-t+mt
    arrivaltime<-arrival(seed,1)
    seed<-LLG(seed,sample(10,1))$z0
    X<-X-mt
    if(length(X)==0){b<-0}else{b<-X[length(X)]}
    servicetime1<-b+service(seed,1)
    X<-c(X,servicetime1)
    wt[k]<-servicetime1
  }
} else if(n==110000){
  mt<-X[1]
  customer<-customer-1
  t<-t+mt
  X<-X[-1]
}
# cat("now time is:",t,"now n is:",n,"now customer in system is:",customer,"now X is:",X,"\n")
# cat("now arrivaltime is:",arrivaltime,"servicetime is:",servicetime,"\n")
if(n==110000 && customer==0)(conv<-TRUE)
}
cat("the mean of waiting time in system is:",mean(wt),"\n")
t2<-proc.time()-t1
t2

########
#HW8-(b)
nwt<-numeric(100000/10)
d<-0
l<-1
for(i in 1:100000){
  d<-d+wt[i]
  if(i%%10==0){
    nwt[l]<-d/10
    l<-l+1
    d<-0
  }
}
lower.tail<-mean(nwt)-qnorm(0.025,mean=0,sd=1,lower.tail = FALSE)*sqrt(var(nwt))/sqrt(10000)
upper.tail<-mean(nwt)+qnorm(0.025,mean=0,sd=1,lower.tail = FALSE)*sqrt(var(nwt))/sqrt(10000)
cat("the 95% confidence interval of mean of waiting time is [",lower.tail,upper.tail,"]","\n")
