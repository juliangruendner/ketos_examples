#cat("R function KD_tnorm1.R loaded....\n")
############################################
KD.tnorm1<-function (data,p,mu,sig,T1,crit) 
{
# KD.tnorm1 evaluates the modified kolmogorov- distance of distr. of empirical data (cdf) from 
# estimated truncated Normal distribution in interval [mod,max(data)]
p1<-pnorm(T1,mu,sig)
data1<-data[data>=T1] # eliminate extreme values
data1<-sort(data1)
m<-length(data1[data1 > crit])
n<-length(data1)
n1<-length(data1[data1 <= crit])
D<-c(rep(0,m));
f00<-(p*(pnorm(data1[n1],mu,sig)-p1))/(1-p1);
i<-1
while (i<=m)
          {
          fn0<-(i-1)/n+f00;
          f0<-(p*(pnorm(data1[i+n1],mu,sig)-p1))/(1-p1);
          D1<-f0-fn0
          D[i]<-max(D1,0);
          i<-i+1
          }
D<-max(D[1:m])
return(D)
}
