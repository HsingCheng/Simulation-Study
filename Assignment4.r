#case1- exp(1/48)
rm(list=ls())
graphics.off()
t1<-proc.time()
t<-0 #time
st<-0 #sytem operation time
con<-c(8,0,2)
la<-1/48
finish<-24*365*10
re<-0
seed=2
LLG<-function(z0,n){
  u<-numeric(n)
  for(i in 1:n)
  {
    zi<-z0
    z0<-(16807*zi)%%(2^31-1)
    zn<-z0
    u[i]<-zn/(2^31-1)
  }
  zn<-zn
  return(list('u'=u,'z0'=zn))
}
repair<-function(z0)
{
  u1<-LLG(z0,1)$u
  u2<-LLG(u1*(2^31-1),1)$u
  # u1<-runif(1,0,1)
  # u2<-runif(1,0,1)
  if(u1<=0.2)
  {return(-log(u2)*12)} else{return(-log(u2)*4)}
}
lifetime<-function(lamda,z0)
{
  u<-LLG(z0,1)$u
  return(-log(u)/lamda)
  # return(-log(runif(1,0,1))/lamda)
}
sit<-numeric(8)
for(i in 1:8){
  seed<-LLG(seed,1)$z0
  sit[i]<-lifetime(la,seed)
}
c<-c()
while(t<finish){
  seed<-LLG(seed,10)$z0
  # cat("old situation:",con,"  system time:",st,"  t =:",t,"\n")
  # cat("seed is :",z,"\n")
  # cat("old lifetime :",sit,"\n")
  # cat("re :",re,"\n")
  if(con[2]==0){
    fail<-min(sit)
    sit<-sit-fail
    t<-t+fail
    st<-st+fail
    con[2]<-con[2]+1
    con[3]<-con[3]-1
    sit[which(sit==min(sit))]<-lifetime(la,seed)
    seed<-LLG(seed,1)$z0
    re<-repair(seed)
  } else if(con[2]==1){
    if(re<min(sit)){
      con[2]<-con[2]-1
      con[3]<-con[3]+1
      t<-t+re
      st<-st+re
      sit<-sit-re
      re<-0
    } else{
      con[2]<-con[2]+1
      con[3]<-con[3]-1
      fail<-min(sit)
      t<-t+fail
      st<-st+fail
      sit<-sit-fail
      sit[which(sit==min(sit))]<-lifetime(la,seed)
      re<-re-fail
    }
  } else if(con[2]==2){
    if(re<min(sit)){
      con[2]<-con[2]-1
      con[3]<-con[3]+1
      t<-t+re
      st<-st+re
      sit<-sit-re
      re<-repair(seed)
    } else{
      con[2]<-con[2]+1
      con[1]<-con[1]-1
      fail<-min(sit)
      t<-t+fail
      st<-st+fail
      sit<-sit-fail
      re<-re-fail
    }
  } else if(con[2]==3){
    con[1]<-con[1]+1
    con[2]<-con[2]-1
    sit[which(sit==min(sit))]<-lifetime(la,seed)
    seed<-LLG(seed,1)$z0
    t<-t+re
    st<-st
    re<-repair(seed)
  }
  c<-c(c,con[2])
  #cat("new situation:",con,"system time:",st,"  t =:",t,"\n")
  # cat(" new lifetime :",sit,"\n")
  # cat("re :",re,"\n\n")
}
cat("under case1 system operation time :",st,"total time:",t,"\n")
cat("systemtime shutdown time :",t-st,"proportion of system shutdown:",(t-st)/t,"\n")
t2<-proc.time()-t1
cat("time of running code:","\n")
t2
