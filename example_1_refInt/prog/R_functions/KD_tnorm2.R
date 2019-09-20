#cat("R function KD_tnorm2.R loaded....\n")
###########################################
KD.tnorm2<-function (data,p,mu,sig,crit) 
{
# KD.tnorm2 evaluates the modified kolmogorov- distance of distr. of empirical data (cdf) from 
# estimated truncated Normal distribution in interval [0,mod(data)]
data1<-sort(data)
n<-length(data1)
m<-length(data1[data1 <= crit])
D<-c(rep(0,m));
i<-1
while (i<=m)
          {
          fn0<-(i-1)/n;
          f0<-p*(pnorm(data1[i],mu,sig));
          D1<-f0-fn0
          D[i]<-max(D1,0);
          i<-i+1
          }
D<-max(D[1:m])
return(D)
}