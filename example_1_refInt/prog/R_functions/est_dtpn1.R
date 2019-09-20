# last modification: 07.03.2016 (Farhad Arzideh), if (wv >=1 || is.na(wv))
# est_dtpn1.R
###################################
est.dtpn1<-
function (d=data,l0=0.5,sl=0.1,n=10,TR=max(data),mod=0.8,com=2,c2=1,TL2=round(min(mod-c2*(10^(-com)),mod-(mod-quantile(d,0.05))*0.1),com),
TL1=min(d),I=100,I1=25,T=max(d),nen=1,epsilon1=1E-4,steptol=5) 
{
#################################
# function est.dtpn1 (parameter estimation of TPN distribution to the data truncated at different points) written on 12.08.2010
# for truncated only at the low values (L)
# PN(lambda,mu,sigma)
# l0, sl and n: define the range in which the transformation parameter lambda is searched:
# e.g.: l0=0.5,sl=0.1 and n=10 means proper lambda is searched in {0.6,0.7,0.8,...,1.5}
# TR: fix truncation point at the right side of mod
# com: accuracy of data values; e.g. for data values 2.11, 1.67 is com=2 and for 144,150 is com=0
#  truncation points are determined at the left side of mod of the data in interval: TL=[TL1,TL2]
# c2 defines how far from mod TL2 is searched
# e.g.for creatinin with data given with two decimals(0.94 or 1.03)and mod=0.88 and for c2=10, TL1 and TL2 can be calculated as:
# TL1=min(data),
# TL2= 0.88-10*(10^-2)=0.78, and thereby:
# TL=[TL1,0.78]
# T: limit of data; data >=T are eliminated. Default: T=max(d)
# I and epsilon: fixed variables to stop the numerical procedure if predefined criterion are satisfied.
# nen: correction factor to modify the proper choice for start value to estimate sigma. default: nen=1 
# mod: mod (mod of data), start value for estimation of mu. 
#################################
d<-d[0<d & d<=T] # eliminate extreme values
q2<-(length(d[d<=TR]))/length(d) # q2: fraction of data smaller than TR, which are not used to estimate the parameters
dt<-T1point(data=d,TL1=TL1,TL2=TL2) # delivers truncation points (TL) and fractions of data <=TL (q1).
q1<-dt$q1
T1<-dt$T1
m<-length(q1) # m: number of truncation points, which gives us m different estimations! 
y<-matrix(rep(0,m*13),ncol=13)
y0<-matrix(rep(0,13),ncol=13)
# now for each truncation point from m truncation points the fitted distribution is estimated.
i<-1;
while(i<=m)
         {
         qu1<-q1[i]
         tr1<-T1[i]
         est<-est.tpn(d=d,mu=mod,q2=q2,T2=TR,q1=qu1,T1=tr1,I=I,I1=I1,l0=l0,sl=sl,n=n,
         nen=nen,com=com,epsilon1=epsilon1,steptol=steptol)$h1
         if (est[11]==-1E+7)
         {
         y[i,1:12]<-c(rep(1,12))
         y[i,13]<-1000
         }
         else
        {
         T11<-est[3]
         T21<-est[4]
         p<-est[9]
         mu<-est[6]
         sig<-est[7]
         frac<-(q2-qu1)/est[8]
         lam<-est[10]
         ll<-est[11]
         d1<-d[d>=tr1 & d<=TR]
         if (lam==0)
            {
             d2<-log(d1)
             d3<-log(d)
            crit<-log(TL2)
            }
         else
            {
            d2<-(d1^lam-1)/lam
            d3<-(d^lam-1)/lam
            crit<-((TL2)^lam-1)/lam
            }
         w<-KD.tnorm(data=d2,mu=mu,sig=sig,T1=T11,T2=T21)$K 
         v<-KD.tnorm2(data=d3,p=frac,mu=mu,sig=sig,crit=crit)
         wv<-w+v
         if (wv >=1 || is.na(wv))
         {
         y[i,1:12]<-c(rep(1,12))
         y[i,13]<-0
         } else
        {
         wvinv<-1/wv
         y[i,1:13]<-c(qu1,q2,tr1,TR,p,mu,sig,lam,ll,w,v,v+w,wvinv)
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
