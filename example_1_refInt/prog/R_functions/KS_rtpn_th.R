#cat("R function KS_rtpn_th.R loaded....\n")
#########################################
#######################
# last modification: 28.07.2017 (F. Arzideh) to consider the number of simulations, 
# where the algorithm does not converge.
#######################
KS.rtpn.th<-
function (q1=0,n,n1=100,mu=est$y3[1,6],sig=est$y3[1,7],lam,l0,sl,m1,alpha1=0.05,T1,T2,nen,q2=1,com,
epsilon1=1E-3,I=100,I1=40,mod) 
{
# Monte Carlo method to evaluate p-value of KS-test statistic for rtpn distribution 
# kD.rtpn.th simulates a data and calculates the Kolmogorv-Smirnov statistic for simulated data 
# r1 is the factor for rounding the simulated data as the data 
# and q1 without rounding-correction 
D1.emp<-NULL
# sim.tnorm simulates the data and rounds it with given factor r1 
data.sim1<-sim.rtpn(n=n,n1=n1,mu=mu,sig=sig,lam=lam,com=com,T1=T1,T2=T2);
i<-1
while(i<=n1)
        {
        d<-sort(data.sim1[i,])
        v<-est.tpn(q1=q1,q2=q2,T2=T2,T1=T1,com=com,d=data.sim1[i,],mu=mod,
        I=I,I1=I1,epsilon1=epsilon1,l0=l0,sl=sl,n=m1,nen=nen)
        if (v$h1[11]==-1E+7)
         {
          D1.emp[i]<-NA
         }else{
        mu1<-round(v$h1[6],8)
        sig1<-round(v$h1[7],8)
        lam1<-round(v$h1[10],8)
        T11<-round(v$h1[3],8)
        T21<-round(v$h1[4],8)
        if (lam1==0)
                    {
                     d<-log(d)
                    }
        else
                    {
                    d<-(d^lam1-1)/lam1
                    }
        data1<-d[d>=T11 & d<=T21]
        D1.emp[i]<-KD.tnorm(data=data1,mu=mu1,sig=sig1,T1=T11,T2=T21)$K
        }
        i<-i+1
       }
D1.emp<-D1.emp[!is.na(D1.emp)]
D1.sort<-sort(D1.emp)
x1<-quantile(D1.sort,(1-alpha1))
return(list(x1=x1,KD=D1.sort))
}