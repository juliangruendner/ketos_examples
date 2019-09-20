#cat("R function kde1.R loaded....\n")
# is loaded in program1
##########################################
kde1<-
function (data,q1=0,x3=quantile(data,q1),nb=2048,b0=1E+2,bw0,mod)
{
# 22.04.09
# as kde function delivers the KDE of the data (k1 and u1 are the values of x and y),
# additionally it gives the values of (k1,u1) over the mod (k2,u2),
# thereby it is assumed that the pathological values are obtained only over the mod of the data.
# The values of (k2,u2) are used to estimate the curve of pathological values
########################################
######################################## 
t1<-density(data,bw=bw0,kernel="gaussian",from=x3,na.rm=TRUE,cut=3,n=nb)
k1<-t1$y
u1<-t1$x
u2<-u1[u1>=mod]
k2<-k1[u1>=mod]
m2<-length(k2)
u3<-u1[u1< mod]
k3<-k1[u1< mod]
m3<-length(k3)
bw<-t1$bw
return(list(bw1=bw,k1=k1,u1=u1,k2=k2,u2=u2,m2=m2,k3=k3,u3=u3,m3=m3))
}