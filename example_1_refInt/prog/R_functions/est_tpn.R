#source("TNorm.txt")
##################

##################
est.tpn<-function (d,q1=0.0,q2,T1=quantile(d,q1),T2=quantile(d,q2),com,mu=128,I1=50,I=200,l0,sl,n,
epsilon1=1E-4,nen=1,steptol=4)
 {
####################################
  # last modification: 30.05.2012: (h2 eliminated.) 
  # this function evaluate the fitted truncated Power-Normal distribution
###################################
  er1<-0.5*(10^(-com)) 
  T1<-max(er1,T1-er1)
  T2<-T2+er1
  dT<-d[d>T1 & d<T2]
  h<-matrix(rep(0,n*14),nrow=n)
  h1<-matrix(rep(0,14),nrow=1)
  i1<-0; while(i1<=n-1)
  {
  lam<-l0+i1*sl;
  if (lam==0)
            {
            T11<-log(T1)
            T21<-log(T2)
            d1<-log(d)
            d2<-log(dT)
            mu1<-log(mu)
            } else
       {
       T11<-(T1^lam-1)/lam 
       T21<-(T2^lam-1)/lam
       d1<-(d^lam-1)/lam
       d2<-(dT^lam-1)/lam
       mu1<-(mu^lam-1)/lam
       }
####################################
  u<-(lam-1)*(sum(log(dT)))
  sig<-nen*(sd(d2))/(q2)
  v<-TNorm(q1=q1,q2=q2,T1=T11,T2=T21,d=d1,mu=mu1,sig=sig,I=I1,er1=0,epsilon1=1E-2)
  step<-1
  while (v$differential > 0.01 && (step < steptol))
        {              
         nen<-1-(step/steptol)
         sig<-nen*(sd(d2))/(q2)
         v<-TNorm(q1=q1,q2=q2,T1=T11,T2=T21,d=d1,mu=mu1,sig=sig,I=I1,er1=0,epsilon1=1E-2)
         step<-step+1        
         }
#######################################
  step0<-1
  while (v$differential > 0.01 && (step0 < steptol))
        {              
         nen<-1+(step0/steptol)
         sig<-nen*(sd(d2))/(q2)
         v<-TNorm(q1=q1,q2=q2,T1=T11,T2=T21,d=d1,mu=mu1,sig=sig,I=I1,er1=0,epsilon1=1E-2)
         step0<-step0+1         
         }
######################################
  i<-i1+1
  if (v$differential > 0.01)
                {
                 h[i,1]<-h[i,2]<-h[i,3]<-h[i,4]<-h[i,5]<-h[i,6]<-h[i,7]<-h[i,8]<-h[i,9]<-h[i,10]<-h[i,12]<-h[i,13]<-1E+7
                 h[i,11]<- -1E+7
                 h[i,14]<- 1E+7
                 } else
                {

  v<-TNorm(q1=q1,q2=q2,T1=T11,T2=T21,d=d1,mu=mu1,sig=sig,I=I,er1=0,epsilon1=epsilon1)
  if (v$differential > epsilon1)
                {
                 h[i,1]<-h[i,2]<-h[i,3]<-h[i,4]<-h[i,5]<-h[i,6]<-h[i,7]<-h[i,8]<-h[i,9]<-h[i,10]<-h[i,12]<-h[i,13]<-1E+7
                 h[i,11]<- -1E+7
                 h[i,14]<- 1E+7
                 } else
                {
  h[i,1]<-round(v$q1,8)
  h[i,2]<-round(v$q2,8)
  h[i,3]<-round(v$T1,8)
  h[i,4]<-round(v$T2,8)
  h[i,5]<-round(v$I,4)
  h[i,6]<-round(v$mu,8)
  h[i,7]<-round(v$sig,8)
  h[i,8]<-round(v$Phi,8)
  h[i,9]<-round(v$frac,4)
  h[i,10]<-lam
  h[i,11]<-round(v$l+u,4)
  h[i,12]<-round(T2,4)
  h[i,13]<-round(T1,4)
  h[i,14]<-round(v$differential,4)
               }}
  i1<-i1+1
  }
  index<-which.max(h[,11])
  h1<-h[index,]
  return(list(h=h,h1=h1))
}
