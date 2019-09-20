#library(msm)
####################################
#cat("R function RLs2.R loaded....\n")
####################################
RLs2<-function(est=est$y0,pc1=0.025)
{
########################
# as RL-function and used for simulation study
# RL-function uses only the parameters of optimized truncation point (TP) to estimate RLs.
# RL2-function uses "best" TPs (or all TPs) to estimate RLs. 
# this function is used by excel.tool1- program. 
########################
t<-nrow(est)
vn<-matrix(rep(0,t*2),ncol=2) # left and right quantiles
for (j in 1:t){
              m<-est[j,6]
              s<-est[j,7]
              lam<-est[j,8]
              if (lam==0){
                          #RLs für log-Normal
                          left0<-qnorm(pc1, mean=m, sd=s)
                          right0<-qnorm(1-pc1, mean=m, sd=s)
                          vn[j,1]<-exp(left0)
                          vn[j,2]<-exp(right0)
                          } else {
                          # RLs für Power-Normal
                          left0<-qtnorm(pc1, mean=m, sd=s, lower=-1/lam, upper=Inf)
                          right0<-qtnorm(1-pc1, mean=m, sd=s, lower=-1/lam, upper=Inf)
                          vn[j,1]<-(left0*lam+1)^(1/lam)
                          vn[j,2]<-(right0*lam+1)^(1/lam)
                          }
                }
return(vn)
}