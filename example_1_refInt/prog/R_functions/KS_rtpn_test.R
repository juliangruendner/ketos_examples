#cat("R function KS_rtpn_test.R loaded....\n")
#######################
# modification: 28.07.2017 (F. Arzideh) to consider the number of simulations, 
# where the algorithm does not converge.
#########################################
# modification: 12.07.2016 (F. Arzideh):
# search for proper lambda has been changed; Now it is searched in a wider range: {0, 0.01, 0.02, ..., 1}  
#########################################
KS.rtpn.test<-
function (data=data,n1=100,alpha1=0.05,est=est1,T1=est$y0[1,3],T2=est$y0[1,4],
sig=est$y0[1,7],q1=0,q2=1,ke=est$y0[1,10],mu=est$y0[1,6],lam=est$y0[1,8],sl=0.01,
m1=101,nen=2,I=100,epsilon1=1E-3,I1=40,mod,com=1) 
{ 
# written on 6th jul. 2008
# kolm.test for truncated pn 
data1<-data[data <=T2 & data >=T1]
n<-length(data1)
l0<-0          # l0 is the start value for estimation of lambda
kth<-KS.rtpn.th(n=n,T1=T1,T2=T2,sig=sig,mu=mu,lam=lam,n1=n1,alpha1=alpha1,
l0=l0,sl=sl,m1=m1,nen=nen,q1=q1,q2=q2,com=com,I=I,epsilon1=epsilon1,I1=I1,mod=mod)
kth1<-kth$x1
k<-kth$KD
n1<-length(k)
v1<-k-ke
   if(max(v1)< 0)
   {
   px<-0
   }
   else if (min(v1)> 0)
   {
   px<-1
   } 
   else
   {   
   i2<-which.max(k[v1<0])
   px<-1-i2/n1
   }
return(list(KolmEmp=ke,KDcr5=kth1,px=px,KolmTheo=k))
}