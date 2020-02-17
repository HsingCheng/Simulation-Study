rm(list=ls())
graphics.off()
########
# HW8-(c)
t1<-proc.time()
t<-numeric(20000)
wt<-numeric(20000)
seed<-46
for(l in 1:20000){
  ct<-0
  rwt<-0
  n<-0
  arrivaltime<-0
  customer<-0
  servicetime<-0
  X<-c()
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
  nextevent<-"initial"
  conv<-FALSE
  while(!conv){
    # cat("X",X,"\n")
    # cat(arrivaltime,servicetime,"\n")
    # cat("ct is",ct,"rwt",rwt,"\n")
    seed<-LLG(seed,sample(10,1))$z0
    if(nextevent=="initial"){
      n<-n+1
      customer<-customer+1
      arrivaltime<-arrival(seed,1)
      seed<-LLG(seed,sample(10,1))$z0
      servicetime1<-service(seed,1)
      X<-c(X,servicetime1)
      rwt<-rwt+servicetime1
    } else if(nextevent=="arrival"){      
      n<-n+1
      customer<-customer+1
      mt<-arrivaltime
      ct<-ct+mt
      arrivaltime<-arrival(seed,1)
      X<-X-mt
      seed<-LLG(seed,sample(10,1))$z0
      if(length(X)==0){b<-0}else{b<-X[length(X)]}
      servicetime1<-b+service(seed,1)
      X<-c(X,servicetime1)
      rwt<-rwt+servicetime1
    } else if(nextevent=="service"){
      customer<-customer-1
      mt<-servicetime
      ct<-ct+mt
      arrivaltime<-arrivaltime-mt
      X<-X-mt
      X<-X[-1]
    }
    if(length(X)==0){servicetime<-0}else{servicetime<-X[1]}
    if(servicetime<arrivaltime && servicetime!=0){nextevent<-"service"} else{nextevent<-"arrival"}
    # cat("X",X,"\n")
    # cat(arrivaltime,servicetime,"\n")
    # cat("ct is",ct,"rwt",rwt,"\n")
    if(length(X)==0 && customer==0) (conv<-TRUE)
  }
  wt[l]<-rwt
  t[l]<-n
}
N<-20000

jack<-numeric(N)
for(i in 1:N){
  jack[i]<-(sum(wt[-i])/sum(t[-i]))
}
Jni<-N*mean(wt)/mean(t)-(N-1)*jack
alphanJ<-1/N*sum(Jni)
VnJ<-1/(N-1)*sum((Jni-alphanJ)^2)
snj<-sqrt(VnJ)

cat("the standard deviation is:",snj,"\n")
lower<-alphanJ-snj*qnorm(0.025,0,1,lower.tail = FALSE)/sqrt(N)
upper<-alphanJ+snj*qnorm(0.025,0,1,lower.tail = FALSE)/sqrt(N)
cat("the 95% C.I of Jacknife estimators is:[",lower,upper,"]","\n")
