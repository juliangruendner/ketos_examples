# kalkuliert die RLs:
# library(msm)
############################
#cat("R function RLs.R loaded....\n")
# 12.10.2016 (Arzideh): parameter com was added. Estimated RLs are now rounded at com+1 decimals.
############################
RLs<-function(est,m=est$y0[1,6],s=est$y0[1,7],lam=est$y0[1,8],pc1=0.025,pc2=0.05,com=com)
{
# estimated RLs with minimum test-statistic
if (lam==0)
           {
             # RLs for log-Normal
             left0<-qnorm(pc1, mean=m, sd=s)
             right0<-qnorm(1-pc1, mean=m, sd=s)
             left0<-exp(left0)
             right0<-exp(right0)
             left1<-qnorm(pc2, mean=m, sd=s)
             right1<-qnorm(1-pc2, mean=m, sd=s)
             left1<-exp(left1)
             right1<-exp(right1)
            }
else
           {
            # RLs for Power-Normal
            left0<-qtnorm(pc1, mean=m, sd=s, lower=-1/lam, upper=Inf)
            right0<-qtnorm(1-pc1, mean=m, sd=s, lower=-1/lam, upper=Inf)
            left0<-(left0*lam+1)^(1/lam)
            right0<-(right0*lam+1)^(1/lam)
            left1<-qtnorm(pc2, mean=m, sd=s, lower=-1/lam, upper=Inf)
            right1<-qtnorm(1-pc2, mean=m, sd=s, lower=-1/lam, upper=Inf)
            left1<-(left1*lam+1)^(1/lam)
            right1<-(right1*lam+1)^(1/lam)
           }
return(list("RLL1"=round(left0,com+1),"RLL2"=round(left1,com+1),"RLR1"=round(right0,com+1),"RLR2"=round(right1,com+1)))
}