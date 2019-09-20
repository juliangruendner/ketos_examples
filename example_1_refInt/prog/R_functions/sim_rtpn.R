#cat("R function sim_rtpn.R loaded....\n")
##########################################
sim.rtpn<-function(n=1000,n1=100,mu=0,sig=1,lam,T1=0,T2=Inf,com=0)
{
# simulates n1-datasets, sample size=n (each one) from a 
# truncated power normal distribution, and rounds each value to com-th decimal position
################
a<-matrix(rep(0,n1*n),ncol=n,nrow=n1)
er1<-0.5*(10^(-com)) 
T1<-max(er1,T1-er1)
T2<-T2+er1;
###############
            i<-1
            while (i<=n1)
                        {
                        a[i,]<-rtboxcox(n=n,lambda=lam,mean=mu,sd=sig,down=T1,up=T2)
                        a[i,]<-round(a[i,],com)
                        i<-i+1
                        }
return(a)
}
