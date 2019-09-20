##################################################################
# - gr.all0 function displays the estimated distributions for  ###
#   non-pathological and pathological values for path.=B       ###
# - updated: 29.09.2014 (without DL) and some lables           ###
# - updated: 08.10.2014 legends and footnotes are modified     ###
#   (Autor: Arzideh)                                           ###
# - 02.02.2015 (Ar): modified to:                              ###
#    i) display path. values on both sides (low and high       ### 
#       values),                                               ###
#    ii) display the estimated distributions for all 3         ###
#        cases (H, B, L),                                      ###
#        functions gr_all1 and gr_all2 have been eliminated    ###
#    iii) apply the tool as default in windows-system          ###
#         or alternatively in linux-system                     ###
# - 05.02.2015 (Ar): 5% and 95% were removed                   ###
# - 27.07.2016 (Arzideh)  y-limits has been fitted             ###
# - 17.08.2016 (Arzideh): for graphical purpose :              ###
#   kden<-kde1(data=d,bw=bwd,nb=nb,mod=mod,q1=0,x3=X3)         ###
##################################################################
#library(msm) 
#library(geoR) 
#source("kde.txt")
#source("kde1.txt")
#source("DL_pnm.txt")
################################
################################
gr.all0<-
function (d,lam=0.15,q1=0.025,T1=quantile(d,q1),q2=0.64,T2=quantile(d,q2),sig=34.5,mu=129.28,p=0.93,lam2=lam2,
eps1=0.2,x12=0.01,over=30000,er1,x01=x01,x02=x02,step0=bw,nb=2048,c1=0.8,c2=1,sc1=1.2,
labx=labx,fac1=0.75,bwd=bw,low,high,com=2,main="estimation") 
{
# written at 05th March 2009
# last modification: 22.08.2012, border: gray15
# for both-sides truncated (B)
# gr.all0 evaluates the DLs and displays the estimated distributions
#########################################
kdew<-kde(data=d,nb=nb,q1=0,bw0=bwd)
y00<-kdew$k
mod<-kdew$u
y00<-round(y00*sc1,3)
minD<-min(d)
maxD<-max(d)
if(maxD > 2*minD) X3<-0 else X3<-minD 
kden<-kde1(data=d,bw=bwd,nb=nb,mod=mod,q1=0,x3=X3)
# kde calculates a kernel density estimation with Sheather & Jones Method 
k1<-kden$k1
u1<-kden$u1 
# k1 and u1 give kde in all points
k2<-kden$k2
u2<-kden$u2
m2<-kden$m2
# k2 and u2 give kde in all points >=mode and m2 length of k2 
k3<-kden$k3
u3<-kden$u3
m3<-kden$m3
# k3 and u3 give kde in all points < mode and m3 length of k3 
bw1<-kden$bw1
k4<-p*(dboxcox(u2,lambda=lam,mean=mu,sd=sig)) # density of pn estimated for >=mod 
te<-rep(0,m2)
j<-1
while (j<=m2)
         {
          te[j]<-max(k2[j]-k4[j],0)
          j<-j+1
          }
te<-te
fx1<-p*(dboxcox(u1,lambda=lam,mean=mu,sd=sig))
###########################################
if (lam==0)
                 {
                 dl<-DL.log(mu0=mu,sig0=sig,p=p)
                 }
else
                 {
                dl<-DL.pnm(mu0=mu,sig0=sig,p=p,lam=lam,TL=-1/lam)
                 }
############################################
k5<-p*(dboxcox(u3,lambda=lam,mean=mu,sd=sig)) # density of pn estimated for < mod 
tet<-rep(0,m3)
j<-1
while (j<=m3)
         {
          tet[j]<-max(k3[j]-k5[j],0)
          j<-j+1
          }
tet<-tet
#####################################
DL25<-round(dl$DL25,com)+lam2
DL5<-round(dl$DL5,com)+lam2
DL975<-round(dl$DL975,com)+lam2
DL950<-round(dl$DL950,com)+lam2
Den25<-dl$Den25
Den95<-dl$Den950
Den975<-dl$Den975
Den5<-dl$Den5
f<-round(seq(0,y00,y00/4),3)
s<-(step0)*3
f1<-seq(x01,x02,s)
d<-d+lam2
u1<-u1+lam2
u2<-u2+lam2
u3<-u3+lam2
density02<-u2[te==0 & u2<=high]
P.min<-density02[which.min(high-density02)]
density03<-u3[tet==0 & u3>=low]
P.max<-density03[which.min(density03-low)]
# windows(width=7,height=5)
op<-par(mgp = c(1, 0.4, 0),mar=c(6,2,2,3))
Y00<-max(hist(d,br=c(-10,seq(x01+0.5*step0,x02+0.5*step0,step0),over),prob=TRUE)$density,fx1)
hist(d,br=c(-10,seq(x01+0.5*step0,x02+0.5*step0,step0),over),prob=TRUE,main=main,xlab=labx,lwd=1,
xlim=c(x01,x02),ylim=c(0,max(y00,Y00)),cex.axis=0.9,cex.lab=0.9,cex.main=0.9,border="gray50",axes=FALSE)
mtext(paste("Estimated distributions for non-pathological values (green curve), pathological values (red)","\n",
            "and whole data (blue). Green lines (and given numers) indicate 2.5 and 97.5 percentiles of","\n",
             "the estimated distribution for non-pathological values (RL)."),cex=0.8,font=3,outer=F,line=4,side=1)
lines(u1,fx1,lwd=2,lty=1,col="green4")
lines(u3[u3<=P.max],tet[u3<=P.max],lwd=0.4,lty=1,col="red")
lines(u2[u2>=P.min],te[u2>=P.min],lwd=0.4,lty=1,col="red")
axis(side=2,at=f,pos=c(x01,0),cex.axis=0.9,lwd=1,tcl=-0.25)
axis(side=1,at=f1,pos=c(0,0),cex.axis=0.9,lwd=1,tcl=-0.25)
lines(density(x=d,kernel="gaussian",n=nb,cut=3,from=x01,bw=bwd),col="blue",lty=2)
lines(x=c(DL975,DL975),y=c(0,Den975),col="green4",lty=1,lwd=2)
lines(x=c(DL25,DL25),y=c(0,Den25),col="green4",lty=1,lwd=2)
text(DL25,3*Den25,font=1,col="green4",cex=0.8,pos=2,labels=eval(DL25))
text(DL975,3*Den975,font=1,col="green4",cex=0.8,pos=4,labels=eval(DL975))
##################
legend(DL950,c2*0.95*y00,legend=c("whole data set","non-pathological","pathological"),col=c("blue","green4","red"),
lty=c(2,1,1),cex=0.7,lwd=1,bty="n")
abline(0,0)
abline(0,0)
par(op)
return(list(P=p,mu=mu,sig=sig,T2=T2,DL975=DL975,DL95=DL950))
}
