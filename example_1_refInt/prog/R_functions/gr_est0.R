################################################################
# gr.est0 function displays the estimated distributions for  ###
# non-pathological and pathological values for path.=B       ###
# updated: 29.09.2014 (without DL) and some lables           ###
# Autor: Arzideh                                             ###
# 02.02.2015 (Ar): modified to:                              ###
#    i) display path. values on both sides (low and high     ### 
#       values),                                             ###
#    ii) display the estimated distributions for all 3       ###
#        cases (H, B, L),                                    ###
#        functions gr_est1 and gr_est2 have been eliminated  ### 
# 12.10.2016 (Ar): com parameter in gr.all0() is modified:   ###
#                  com=com+1  
################################################################
################################################################
gr.est0<-
function (data=te$dbm,est=est1,lam=est$y0[1,8],q1=est$y0[1,1],q2=est$y0[1,2],sig=est$y0[1,7],mu=est$y0[1,6],p=est$y0[1,5],
x12=0.001,x01=min(data),x02=max(data),over=40000,step0=0.1,com=1,er1=0.5*(10^(-com)),
nb=4096,c1=0.8,c2=1,sc1=1.2,labx="non-transformed values (U/l)",bw,low,high,main="estimation",lam2=mind) 
{
# gr.est : written at 12th August 2008
# display the estimated distributions for pathological and non-pathological values and
# calculates  Enscheidungsgrenze (DL) and fase positive and false negative prob.
########################################## 
res<-gr.all0(d=data,lam=lam,q1=q1,q2=q2,sig=sig,mu=mu,p=p,lam2=lam2,
x01=x01,x02=x02,x12=x12,step0=step0,bwd=bw,over=over,er1=er1,nb=nb,
sc1=sc1,c2=c2,c1=c1,com=com+1,labx=labx,low=low,high=high,main=main)
return(list(P=res$P,mu=res$mu,sig=res$sig,T2=res$T2,DL975=res$DL975,DL95=res$DL95))
}
