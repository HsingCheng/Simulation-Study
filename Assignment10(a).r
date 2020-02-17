rm(list=ls())
graphics.off()
####
compare.policy<-function(n){
t1<-proc.time()
initial.n<-n
alpha<-0.05
p<-c(0.06,0.01,0.12,0.14,0.08,0.18,0.04,0.10,0.22,0.05)
conv1<-FALSE
while(!conv1){
list.orA<-c(1:10)
list.orB<-c(1:10)
list.orC<-c(1:10)
collect.A<-numeric(n)
collect.B<-numeric(n)
collect.C<-numeric(n)
loop<-1
seed<-46
LLG<-function(z0,n){
  u<-numeric(n)
  for(i in 1:n){
    zi<-z0
    z0<-(97336*zi)%%(2^31-1)
    zn<-z0
    u[i]<-zn/(2^31-1)
  }
  return(list("u"=u,"z0"=zn))
}
seed<-LLG(seed,sample(10,1))$z0
request<-function(z0){
  u<-LLG(z0,1)$u
  k<-0
  for(i in 1:length(p)){
    if(u<=k+p[i]){
      F<-i
      break
    } else{k<-k+p[i]}
  }
  return(F)
}

moveA<-function(i,list.or){
  a<-list.or[i]
  if(i ==1){list.or<-list.or} else{
  for(l in i:2){
    list.or[l]<-list.or[l-1]
  }
}
  list.or[1]<-a
  return(list.or)
}

moveB<-function(i,list.or){
  a<-list.or[i]
  if(i == 1){list.or<-list.or} else{
    list.or[i]<-list.or[i-1]
  }
  list.or[i-1]<-a
  return(list.or)
}
moveC<-function(i,list.or){
  a<-list.or[i]
  if(i == 1){
    list.or[i]<-list.or[i+1]
    list.or[i+1]<-a
  } else if(i==2){
    list.or[i]<-list.or[i-1]
    list.or[i-1]<-a
  } else{
    for(l in i:3){
      list.or[l]<-list.or[l-1]
    }
    list.or[2]<-a
  }
  return(list.or)
}

conv<-FALSE
while(!conv){
  seed<-LLG(seed,sample(10,1))$z0
  if(loop<10001){
  ith<-request(seed)
  iA<-which(list.orA==ith)
  iB<-which(list.orB==ith)
  iC<-which(list.orC==ith)
  list.orA<-moveA(iA,list.orA)
  list.orB<-moveB(iB,list.orB)
  list.orC<-moveC(iC,list.orC)
  } else if(loop>10000){
  ith<-request(seed)
  iA<-which(list.orA==ith)
  iB<-which(list.orB==ith)
  iC<-which(list.orC==ith)
  list.orA<-moveA(iA,list.orA)
  list.orB<-moveB(iB,list.orB)
  list.orC<-moveC(iC,list.orC)
  collect.A[loop-10000]<-iA
  collect.B[loop-10000]<-iB
  collect.C[loop-10000]<-iC
  }
  if(loop==n+10000) {conv<-TRUE} else{loop<-loop+1}
}
mean.A<-mean(collect.A)-min(mean(collect.B),mean(collect.C))
mean.B<-mean(collect.B)-min(mean(collect.A),mean(collect.C))
mean.C<-mean(collect.C)-min(mean(collect.A),mean(collect.B))
  if(min(mean(collect.B),mean(collect.C))==mean(collect.B)){
    ZA<-collect.A-collect.B
  } else {
    ZA<-collect.A-collect.C
  }
  if(min(mean(collect.A),mean(collect.C))==mean(collect.A)){
    ZB<-collect.B-collect.A
  } else {
    ZB<-collect.B-collect.C
  }
  if(min(mean(collect.A),mean(collect.C))==mean(collect.A)){
    ZC<-collect.C-collect.A
  } else {
    ZC<-collect.C-collect.B
  }
lower.ZA<-mean(ZA)-qnorm(alpha/2,0,1,lower.tail = FALSE)*sqrt(var(ZA))/sqrt(n)
upper.ZA<-mean(ZA)+qnorm(alpha/2,0,1,lower.tail = FALSE)*sqrt(var(ZA))/sqrt(n)

lower.ZB<-mean(ZB)-qnorm(alpha/2,0,1,lower.tail = FALSE)*sqrt(var(ZB))/sqrt(n)
upper.ZB<-mean(ZB)+qnorm(alpha/2,0,1,lower.tail = FALSE)*sqrt(var(ZB))/sqrt(n)

lower.ZC<-mean(ZC)-qnorm(alpha/2,0,1,lower.tail = FALSE)*sqrt(var(ZC))/sqrt(n)
upper.ZC<-mean(ZC)+qnorm(alpha/2,0,1,lower.tail = FALSE)*sqrt(var(ZC))/sqrt(n)


compare.CI<-c(upper.ZA,upper.ZB,upper.ZC)
compare.CI1<-c(lower.ZA,lower.ZB,lower.ZC)
if(min(compare.CI)<0 && median(compare.CI)>0 && max(compare.CI)>0 && compare.CI1[which(compare.CI==median(compare.CI))]>0){
  cat("the 95% C.I of ZA is:[",lower.ZA,upper.ZA,"]","\n")
  cat("the 95% C.I of ZB is:[",lower.ZB,upper.ZB,"]","\n")
  cat("the 95% C.I of ZC is:[",lower.ZC,upper.ZC,"]","\n")
  cat("We found the best system is:",which(compare.CI==min(compare.CI)),"\n")
  cat("we first choose n is",initial.n,"loop","and final ust n is",n ,"loop","\n")
  conv1<-TRUE
} else {
  n<-n+10000
}
}
t2<-proc.time()-t1
cat("the time of running coding is:","\n")
cat(t2)
}
compare.policy(1000)



