#cat("R function kde.R loaded....\n")
#is loaded in program1
#############################################
kde<-
function (data,q1=0.025,x3=quantile(data,q1),nb=900,bw0=bw1)
{
# evaluation of mod of data and y_mod using density function  
t1<-density(x=data,bw=bw0,kernel="gaussian",from=x3,na.rm=TRUE,cut=3,n=nb)
k1<-t1$y
u1<-t1$x
a.max<-which.max(k1)
k<-k1[a.max]
u<-u1[a.max]
return(list(k=k,u=u))
}