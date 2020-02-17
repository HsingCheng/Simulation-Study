rm(list=ls())
graphics.off()
####
t1<-proc.time()
n<-10000
busy.v<-numeric(n)
block.v<-numeric(n)
complete.v<-numeric(n)
totalcall.v<-numeric(n)
alpha<-0.05
seed <- 46
for(l in 1:n){
####
N<-10
M<-4
t<-0
totalcall<-0
completecall<-0
busycall<-0
blockcall<-0
X<-numeric(N)
xt<-numeric(N)
XM<-numeric(M)
ttime<-numeric(N)
pj<-numeric(N)
P<-matrix(1/9,nrow = N,ncol = N)
diag(P)<-0
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

calltime<-function(seed){
  lamda<-2
  u1<-LLG(seed,1)$u
  return(-log(u1)/lamda)
}
phonetime<-function(seed,n){
  a<-1/4
  b<-(exp(1)+a)/exp(1)
  c<-1/a
  u1<-numeric(n)
  conv<-FALSE
  k<-1
while(!conv){
  seed<-LLG(seed,sample(10,1))$z0
  U<-LLG(seed,1)$u
  # U<-runif(1,0,1)
  seed<-LLG(seed,sample(10,1))$z0
  W<-LLG(seed,1)$u
  # W<-runif(1,0,1)
  V<-b*U
if(V<=1){
  x<-V^c
if(W<=exp(-x)){
  u1[k]<-x
  k<-k+1
}} else{
   x<- -log(c*(b-V))
  if(W <= x^(a-1)){
   u1[k]<-x
   k<-k+1
}
}
  if(min(u1)!=0){conv<-TRUE}
}
  # return(rgamma(1,shape = 1/4,rate = 1))
  return(u1)
}

selectj<-function(seed,i){
  seed<-LLG(seed,sample(1:10,1))$z0
  u1<-LLG(seed,1)$u
  F<-0
for(j in 1:dim(P)[1]){
  F<-F+P[i,j]
if(u1<=F){
  return(j)
  break
}}}

for(i in 1:N){
    seed<-LLG(seed,sample(10,1))$z0
    pj[i]<-selectj(seed,i)
    seed<-LLG(seed,sample(10,1))$z0
    ttime[i]<-calltime(seed)
}
#####
conv<-FALSE
while(!conv){
  seed<-LLG(seed,sample(10,1))$z0
if(max(XM)==0)          #initial case
{
   mt<-min(ttime) 
   t<-t+mt
   ith<-order(ttime)[1]            # order(ttime)[1] is ith
   jth<-pj[order(ttime)[1]]        # pj[order(ttime)[1]] is jth 
   pj[c(ith,jth)]<-0
   X[c(ith,jth)] <- min(which(XM==0))  
   XM[min(which(XM==0))]<- 1
   ttime<-ttime-mt
   Li<-phonetime(seed,1)
   ttime[c(ith,jth)] <- Li
   completecall<-completecall+1
   totalcall<-totalcall+1
} else if(max(XM)!=0){      #had link be used
     if(X[order(ttime)[1]]>0){    #end of a call
       mt<-min(ttime)
       t<-t+mt
       ith<-which(ttime==min(ttime))[1]
       jth<-which(ttime==min(ttime))[2]
       ttime<-ttime-mt
       pj[ith]<-selectj(seed,ith)
       seed<-LLG(seed,sample(10,1))$z0
       pj[jth]<-selectj(seed,jth)
       seed<-LLG(seed,sample(10,1))$z0
       ttime[ith]<-calltime(seed)
       seed<-LLG(seed,sample(10,1))$z0
       ttime[jth]<-calltime(seed)
       XM[X[ith]]<-0
       X[c(ith,jth)]<-0
} else{                      #someone call phone
   if(min(XM)!=0){           #no link case
    totalcall<-totalcall+1
    blockcall<-blockcall+1
    mt<-min(ttime)
    t<-t+mt
    ith<-order(ttime)[1]
    ttime<-ttime-mt
    ttime[ith]<-calltime(seed)
    seed<-LLG(seed,sample(10,1))$z0
    pj[ith]<-selectj(seed,ith)
} else{                      #had link
     ith<-order(ttime)[1]
     jth<-pj[order(ttime)[1]]                      
       if(X[jth]>0){             #but jth is calling  
        totalcall<-totalcall+1
        busycall<-busycall+1
        mt<-min(ttime)
        t<-t+mt
        ttime<-ttime-mt
        ttime[ith]<-calltime(seed)
        seed<-LLG(seed,sample(10,1))$z0
        pj[ith]<-selectj(seed,ith)
} else{                    # jth is idle
   totalcall<-totalcall+1
   completecall<-completecall+1
   mt<-min(ttime)
   t<-t+mt
   ttime<-ttime-mt
   Li<-phonetime(seed,1)
   ttime[c(ith,jth)]<-Li
   seed<-LLG(seed,sample(10,1))$z0
   pj[c(ith,jth)]<-0
   X[c(ith,jth)] <- min(which(XM==0))  
   XM[min(which(XM==0))]<- 1
}}}}
# cat("now t is:",t,"\n")
# cat("state pij is :",pj,"\n")
# cat("state X is:",X,"XM state is:",XM,"\n")
# cat("event time is:",ttime,"\n\n")
if(max(XM)==0) {conv<-TRUE}
}
totalcall.v[l]<-totalcall
busy.v[l]<-busycall
block.v[l]<-blockcall
complete.v[l]<-completecall
}
cat("the proportion of successfully completed :",mean(complete.v)/mean(totalcall.v),"\n")
mean.succ<-mean(complete.v)/mean(totalcall.v)
sn.succ<-sqrt(sum((complete.v-mean.succ*totalcall.v)^2))/(sqrt(n)*mean(totalcall.v))
lower.succ<-mean.succ-qnorm(alpha/2,0,1,lower.tail = FALSE)*sn.succ/sqrt(n)
upper.succ<-mean.succ+qnorm(alpha/2,0,1,lower.tail = FALSE)*sn.succ/sqrt(n)
cat("95% confidence interval of limiting probability of successfully completed :[",lower.succ,upper.succ,"]","\n")

cat("the proportion of busy call :",mean(busy.v)/mean(totalcall.v),"\n")
mean.busy<-mean(busy.v)/mean(totalcall.v)
sn.busy<-sqrt(sum((busy.v-mean.busy*totalcall.v)^2))/(sqrt(n)*mean(totalcall.v))
lower.busy<-mean.busy-qnorm(alpha/2,0,1,lower.tail = FALSE)*sn.busy/sqrt(n)
upper.busy<-mean.busy+qnorm(alpha/2,0,1,lower.tail = FALSE)*sn.busy/sqrt(n)
cat("95% confidence interval of limiting probability of busycall :[",lower.busy,upper.busy,"]","\n")

cat("the proportion of blockcall:",mean(block.v)/mean(totalcall.v),"\n")
mean.block<-mean(block.v)/mean(totalcall.v)
sn.block<-sqrt(sum((block.v-mean.block*totalcall.v)^2))/(sqrt(n)*mean(totalcall.v))
lower.block<-mean.block-qnorm(alpha/2,0,1,lower.tail = FALSE)*sn.block/sqrt(n)
upper.block<-mean.block+qnorm(alpha/2,0,1,lower.tail = FALSE)*sn.block/sqrt(n)
cat("95% confidence interval of limiting probability of blockcall :[",lower.block,upper.block,"]","\n")

t2<-proc.time()-t1
t2
