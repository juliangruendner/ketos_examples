# last modification: 07.03.2016 (Farhad Arzideh), if (wv >=1 || is.na(wv))
# est_dtpn2.R
#######################################
est.dtpn2<-
function (d=data,l0=0.5,sl=0.1,n=10,TL=0,mod=0.8,com=2,c2=1,TR2=max(d),TR1=round(max(mod+c2*(10^(-com)),mod+(min(quantile(d,0.95),TR2)-mod)*0.1),com),I=100,I1=25,T=max(d),nen=1,epsilon1=1E-4,steptol=5) 
{
###############################
# last modification: 05.03.2012: (for the case that no MLE delivered: y[,13]<-0).
# function est.dtpn2 (parameter estimation of TPN distribution to the data truncated at different points)  written on 12.08.2010
# for truncated only at high values (H)
# PN(lambda,mu,sigma)
# l0, sl and n: define the range in which the transformation parameter lambda is searched:
# e.g.: l0=0.5,sl=0.1 and n=10 means proper lambda is searched in {0.6,0.7,0.8,...,1.5}
# TL: fix truncation point at the left side of mod
# com: accuracy of data values; e.g. for data values 2.11, 1.67 is com=2 and for 144,150 is com=0
#  truncation points are determined at the right side of mod of the data in interval: TR=[TR1,TR2]
# c2 defines how far from mod TR1 is searched
# e.g. for creatinin with data given with two decimals (0.94 or 1.03) and mod=0.88 and for c2=10, TR1 and TR2 can be calculated as:
# , TR2=max(data),
#  TR1= 0.88+10*(10^-2)=0.98, and thereby:
#  TR=[0.98,TR2]
# T: limit of data; data >=T are eliminated. Default: T=max(d)
# I and epsilon: fixed variables to stop the numerical procedure if predefined criterion are satisfied.
# nen: correction factor to modify the proper choice for start value to estimate sigma. default: nen=1 
# mod: mod (mod of data), start value for estimation of mu. 
##############################
d<-d[d<=T]                                                  # eliminate extreme values
q1<-(length(d[d<TL]))/length(d)                             # q1: fraction of data smaller than TL, which are not used to estimate the parameters
dt<-T2point(data=d,TR1=TR1,TR2=TR2)              # delivers truncation points (T2) and fractions of data <=T2 (q2).
q2<-dt$q2
T2<-dt$T2
m<-length(q2)                                      # m: number of truncation points, which gives us m different estimations! 
y<-matrix(rep(0,m*13),ncol=13)
y0<-matrix(rep(0,13),ncol=13)
################################
# now for each truncation point from m truncation points the fitted distribution is estimated.
i<-1;
while(i<=m)
       {
         qu2<-q2[i]
         tr2<-T2[i]
         est<-est.tpn(d=d,mu=mod,q1=q1,T1=TL,q2=qu2,T2=tr2,I=I,I1=I1,l0=l0,sl=sl,n=n,
         nen=nen,com=com,epsilon1=epsilon1,steptol=steptol)$h1
         if (est[11]==-1E+7)
         {
         y[i,1:12]<-c(rep(1,12))
         y[i,13]<-0
         }
         else
        {
         T11<-est[3]
         T21<-est[4]
         p<-est[9]
         mu<-est[6]
         sig<-est[7]
         frac<-(qu2-q1)/est[8]
         lam<-est[10]
         ll<-est[11]
         d1<-d[d>=TL & d<=tr2]
         if (lam==0)
            {
             d2<-log(d1)
             d3<-log(d) 
            crit<-log(TR1)
            }
         else
            {
            d2<-(d1^lam-1)/lam
            d3<-(d^lam-1)/lam
            crit<-((TR1)^lam-1)/lam
            }
         w<-KD.tnorm(data=d2,mu=mu,sig=sig,T1=T11,T2=T21)$K 
         v<-KD.tnorm1(data=d3,p=frac,mu=mu,sig=sig,T1=T11,crit=crit)
         wv<-w+v
         if (wv >=1 || is.na(wv))
         {
         y[i,1:12]<-c(rep(1,12))
         y[i,13]<-0
         } else
        {
         wvinv<-1/wv
         y[i,1:13]<-c(q1,qu2,TL,tr2,p,mu,sig,lam,ll,w,v,v+w,wvinv)
         }
        } 
         i<-i+1;
       }
index0<-which.min(y[,12])
y0[1,]<-y[index0,]
y<-round(y,5)
y0<-round(y0,5)
colnames(y)<-c("q1","q2","T1","T2","P","mu","sigma","lam","LL","KD1","KD2","KD","invKD")
colnames(y0)<-c("q1","q2","T1","T2","P","mu","sigma","lam","LL","KD1","KD2","KD","invKD")
return(list(y=y,y0=y0))
}
