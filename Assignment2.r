#case1 Inverse transform
#t1<-Sys.time()
case1<-function(n){
X<-numeric(10)
p<-c(0.11,0.12,0.09,0.08,0.12,0.10,0.09,0.09,0.10,0.10)
z0<-as.numeric(Sys.time())
z<-0
while(sum(X)<n)
{
  zi<-z0
  z0<-(16807*zi)%%(2^31-1)
  z<-z0/(2^31-1)
#classify Xi
  F<-0
for(i in 1:length(p))
{
  F<-F+p[i]
  if(z<=F)
  {
    X[i]<-X[i]+1
    break
  }
}
}
cat("case1 number of Xi :","\n")
cat(X,"\n")
cat("case1 Xi probability:","\n")
cat(X/n,"\n")
}
system.time(case1(10000000))
#case1(10000000)
#t2<-Sys.time()
#t2-t1

#case2 acceptance-rejection
#t1<-Sys.time()
case2<-function(n){
X<-numeric(10)
p<-c(0.11,0.12,0.09,0.08,0.12,0.10,0.09,0.09,0.10,0.10)
z0<-as.numeric(Sys.time())
fail<-0
while(sum(X)<n){
z<-numeric(2)
for(i in 1:2)
{
  zi<-z0
  z0<-(16807*zi)%%(2^31-1)
  z[i]<-z0/(2^31-1)
}
Y<-floor(10*z[1])+1
if(z[2]<=(p[Y]/0.12))
{
  X[Y]<-X[Y]+1
}
else{
  fail<-fail+1
}
}
cat("case2 fail times:",fail,"\n")
cat("acceptance rate:",(n-fail)/n,"\n")
cat("case2 number of Xi :","\n")
cat(X,"\n")
cat("case2 Xi probability:","\n")
cat(X/n,"\n")
}
system.time(case2(10000000))
#case2(10000000)
#t2<-Sys.time()
#t2-t1

# two cases I only collect how many number of each Xi, so I didn't know its data
