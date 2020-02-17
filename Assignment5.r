rm(list=ls())
graphics.off()
########
t1<-proc.time()
m<-10000
before.time<-0
mean.time<-0
after.time<-0
before.delay<-0
mean.delay<-0
after.delay<-0
before.time.var<-0
mean.time.var<-0
after.time.var<-0
before.delay.var<-0
mean.delay.var<-0
after.delay.var<-0
seed=46
conv1<-FALSE
n<-1
while(!conv1){
  N<-150
  mu<-N/2
  c<-1/N
  k<-1
  t<-0
  delay<-numeric(N)
  X<-numeric(N)
  X2<-numeric(N)
  situation<-c(N,N,0)
  event<-c()
  passenger<-0
  suitcase<-0
  #######
  
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
  ########
  passenger.arrivaltime<-function(z0,k)
  {
    lamda<-N-k+1  #k=1,...,N
    u1<-LLG(z0,1)$u
    # u1<-runif(1,0,1)
    return(-log(u1)/lamda)
  }
  ##
  shuffling<-function(z0,N)
  {
    seed<-z0
    set<-c(1:N)
    for(m in N:2){
      seed<-LLG(seed,sample(1:10,1))$z0
      I <- LLG(seed,1)$u*m+1
      # I<-runif(1,0,1)*m+1
      I<-floor(I)
      a<-set[I]
      set[I]<-set[m]
      set[m]<-a
    }
    return(set)
  }
  shuff<-shuffling(seed,N)
  ##
  seed<-LLG(seed,sample(1:10,1))$z0
  Z<-function(z0)
  {
    lamda<-mu
    u1<-LLG(z0,1)$u
    # u1<-runif(1,0,1)
    return(-log(u1)/lamda)
  }
  ##
  
  conv<-FALSE
  while(!conv){
    seed<-LLG(seed,sample(1:10,1))$z0
    if(situation[1]==150 && situation[2]==150){
      passenger<-passenger.arrivaltime(seed,k)
      k<-k+1
      event<-c(event,1)
      
      seed<-LLG(seed,sample(1:10,1))$z0
      suitcase<-Z(seed)+c
      event<-c(event,2)
      if(passenger<suitcase){
        situation[1]<-situation[1]-1
        mt<-passenger
        passenger<-0
        suitcase<-suitcase-mt
        event<-event[-which(event==1)]
        delay[which(X==1)]<-delay[which(X==1)]+mt 
        t<-t+mt
        X[min(which(X==0))]<-1
        passenger<-passenger.arrivaltime(seed,k)
        k<-k+1
        event<-c(event,1)
      } else{
        situation[2]<-situation[2]-1
        mt<-suitcase
        suitcase<-0
        event<-event[-which(event==2)]
        passenger<-passenger-mt
        delay[which(X==1)]<-delay[which(X==1)]+mt  
        t<-t+mt
        X2[shuff[1]]<-1  
        shuff<-shuff[-1]
        suitcase<-c
        event<-c(event,2)
      }
    } else if(situation[1]>0 && situation[2]>0){
      if(passenger<suitcase){
        situation[1]<-situation[1]-1
        mt<-passenger
        passenger<-0
        suitcase<-suitcase-mt
        event<-event[-which(event==1)]
        delay[which(X==1)]<-delay[which(X==1)]+mt 
        t<-t+mt
        X[min(which(X==0))]<-1
        passenger<-passenger.arrivaltime(seed,k)
        k<-k+1
        event<-c(event,1)
      } else{
        situation[2]<-situation[2]-1
        mt<-suitcase
        suitcase<-0
        event<-event[-which(event==2)]
        passenger<-passenger-mt
        delay[which(X==1)]<-delay[which(X==1)]+mt  
        t<-t+mt
        X2[shuff[1]]<-1  
        shuff<-shuff[-1]
        suitcase<-c
        event<-c(event,2)
      }
    } else if(situation[2]==0 && situation[1]>0){
      situation[1]<-situation[1]-1
      mt<-passenger
      passenger<-0
      event<-event[-which(event==1)]
      delay[which(X==1)]<-delay[which(X==1)]+mt  
      t<-t+mt
      X[min(which(X==0))]<-1
      passenger<-passenger.arrivaltime(seed,k)
      k<-k+1
      event<-c(event,1)
    } else if(situation[1]==0 && situation[2]>0){
      situation[2]<-situation[2]-1
      mt<-suitcase
      suitcase<-0
      event<-event[-which(event==2)]
      passenger<-passenger-mt
      delay[which(X==1)]<-delay[which(X==1)]+mt  
      t<-t+mt
      X2[shuff[1]]<-1  
      shuff<-shuff[-1]
      suitcase<-c
      event<-c(event,2)
    }
    for(l in 1:N){
      if(X[l]==1 && X2[l]==1){
        X[l]<-2
        X2[l]<-2
        situation[3]<-situation[3]+1
      }
    }
    if(situation[3]==150){conv<-TRUE}
  }

  
  ## compute time mean and var
  before.time<-after.time
  mean.time<-before.time+(t-before.time)/n
  after.time<-mean.time
  ## compute the max of delay of mean and var
  before.delay<-after.delay
  mean.delay<-before.delay+(max(delay)-before.delay)/n
  after.delay<-mean.delay
  
  if(n>1){
    before.time.var<-after.time.var
    mean.time.var<-(1-1/(n-1))*before.time.var+n*(after.time-before.time)^2
    after.time.var<-mean.time.var
    
    before.delay.var<-after.delay.var
    mean.delay.var<-(1-1/(n-1))*before.delay.var+n*(after.delay-before.delay)^2
    after.delay.var<-mean.delay.var
  }
  if(n>2 && n>=(after.time.var)*(1.65)/((0.01^2)*(after.time^2))) {b <- n ; conv1<-TRUE }
  n<-n+1
}
cat("the mean of total time is:",after.time,"\n")
lower.time<-after.time-1.96*sqrt(after.time.var)/sqrt(m)
upper.time<-after.time+1.96*sqrt(after.time.var)/sqrt(m)
cat("95% confidence interval of mean of total time is:[",lower.time,upper.time,"]","\n")
cat("length of 95% confidence interval of mean of total time:",upper.time-lower.time,"\n")
lower.time<-after.time-1.65*sqrt(after.time.var)/sqrt(m)
upper.time<-after.time+1.65*sqrt(after.time.var)/sqrt(m)
cat("90% confidence interval of mean of total time :[",lower.time,upper.time,"]","\n")
cat("length of 90% confidence interval of mean of total time:",upper.time-lower.time,"\n")
cat("\n")
cat("the mean of max of delay time is:",after.delay,"\n")
lower.delay<-after.delay-1.96*sqrt(after.delay.var)/sqrt(m)
upper.delay<-after.delay+1.96*sqrt(after.delay.var)/sqrt(m)
cat("95% confidence interval of mean of max delay is:[",lower.delay,upper.delay,"]","\n")
cat("length of 95% confidence interval of mean max delay time:",upper.delay-lower.delay,"\n")

lower.delay<-after.delay-1.65*sqrt(after.delay.var)/sqrt(m)
upper.delay<-after.delay+1.65*sqrt(after.delay.var)/sqrt(m)
cat("90% confidence interval of mean of max delay is:[",lower.delay,upper.delay,"]","\n")
cat("length of 90% confidence interval of mean max delay time:",upper.delay-lower.delay,"\n")

t2<-proc.time()-t1
cat("the total time of running code is:","\n")
t2
