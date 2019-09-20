#cat("R function KD_tnorm.R loaded....\n")
##########################################
KD.tnorm<-function (data,mu,sig,T1,T2) 
{
# KD.tnorm evaluates the kolmogorov- distance of distr. of empirical data (cdf) from 
# estimated truncatedNormal-distribution between T1 and T2.
m<-length(data)
data1<-sort(data)
p1<-pnorm(T1,mu,sig)
p2<-pnorm(T2,mu,sig)
p0<-p2-p1
D<-c(rep(0,m));
i<-1
while (i<=m)
         {
        fn1<-i/m
        fn0<-(i-1)/m
        f0<-(pnorm(data1[i],mu,sig)-p1)/p0
        D1<-abs(fn0-f0);
        D2<-abs(f0-fn1);
        D[i]<-max(D1,D2);
        i<-i+1
        }
index<-which.max(D)
v<-data1[index]
j<-(index)/m
K<-max(D[1:m])
return(list(K=K,v=v,j=j))
}