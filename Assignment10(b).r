rm(list=ls())
graphics.off()
####
t1<-proc.time()
n0<-40
alpha<-0.05
h<- -2.786
d<-0.1
p<-c(0.06,0.01,0.12,0.14,0.08,0.18,0.04,0.10,0.22,0.05)
list.orA<-c(1:10)
list.orB<-c(1:10)
list.orC<-c(1:10)
initial.A<-numeric(n0)
initial.B<-numeric(n0)
initial.C<-numeric(n0)
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
        initial.A[loop]<-iA
        initial.B[loop]<-iB
        initial.C[loop]<-iC
      if(loop==n0) {conv<-TRUE} else{loop<-loop+1}
    }
}
#compute Ni
Var.initialA<-var(initial.A)
Var.initialB<-var(initial.B)
Var.initialC<-var(initial.C)
N.A<-max(n0+1,floor(h^2*Var.initialA/d^2)+1)
N.B<-max(n0+1,floor(h^2*Var.initialB/d^2)+1)
N.C<-max(n0+1,floor(h^2*Var.initialC/d^2)+1)
Ni<-c(N.A,N.B,N.C)
collect.A<-numeric(N.A-n0)
collect.B<-numeric(N.B-n0)
collect.C<-numeric(N.C-n0)
conv1<-FALSE
while(!conv1){
  seed<-LLG(seed,sample(10,1))$z0
  loop<-loop+1
  ith<-request(seed)
  if(loop<=N.A){
    iA<-which(list.orA==ith)
    list.orA<-moveA(iA,list.orA)
    collect.A[loop-40]<-iA
  }
  if(loop<=N.B){
    iB<-which(list.orB==ith)
    list.orB<-moveB(iB,list.orB)
    collect.B[loop-40]<-iB
  }
  if(loop<=N.C){
    iC<-which(list.orC==ith)
    list.orC<-moveA(iC,list.orC)
    collect.C[loop-40]<-iC
}

  if(loop==max(Ni)) (conv1<-TRUE)  

}
  mean.collect.A<-mean(collect.A)
  mean.collect.B<-mean(collect.B)
  mean.collect.C<-mean(collect.C)
  
  W1.A<- (n0/N.A)*(1+sqrt(1-(N.A/n0)*(1-((N.A-n0)*d^2)/(h^2*Var.initialA))))
  W1.B<- (n0/N.B)*(1+sqrt(1-(N.B/n0)*(1-((N.B-n0)*d^2)/(h^2*Var.initialB))))
  W1.C<- (n0/N.C)*(1+sqrt(1-(N.C/n0)*(1-((N.C-n0)*d^2)/(h^2*Var.initialC))))
  W2.A<-1-W1.A
  W2.B<-1-W1.B
  W2.C<-1-W1.C
  
  est.A<-W1.A*mean(initial.A)+W2.A*mean.collect.A
  est.B<-W1.B*mean(initial.B)+W2.B*mean.collect.B
  est.C<-W1.C*mean(initial.C)+W2.C*mean.collect.C
  t2<-proc.time()-t1
  
  cat("the total sample size of policy A:",N.A,"policy B:",N.B,"policy C:",N.C,"\n")
  cat("the weights of A WA1:",W1.A,"WA2:",W2.A,"\n")
  cat("the weights of B WB1:",W1.B,"WB2:",W2.B,"\n")
  cat("the weights of C WC1:",W1.C,"WC2:",W2.C,"\n")
  cat("the final of estimator of A is:",est.A,"\n")
  cat("the final of estimator of B is:",est.B,"\n")
  cat("the final of estimator of C is:",est.C,"\n")
  cat("the time of running coding:","\n")
  t2
