# last modification: 27.04.2016 (Arzideh) to fit y-limits 
#source("kde.txt")
# graph.hist.R  displays the hist and kde and returns mod of the data
# modification (Ar): 22.08.2012, border=gray15
# last modification (Ar): 03.02.2015: this modification was developed to use the tool as default in windows-system 
# or alternatively in linux-system (e.g. OSM) 
###########################
# T0 and T the lower and upper limits of data
# x1 and x2 x-axis range
# below and over limits for histogram
# bw: estimated bandwidth
# br: histogram's classes
# n: number of classes for denstiy function used in kde
# method: kde method
# sc1: scaling of y-axis
# i: scaling of labling of x-axis
# border: colour of histigram borders
# xlab: x-axis label
# cex, cex1,cex2 and cex0: defined values for amount of text axis-text and lables-text
# main: title of graph 
############################
graph.hist<-
function (data=ka1,T=max(data),T0=min(data),x1=min(data),below=-10,x2=max(data),over=max(data),
bw=1,step,br=c(below,seq(x1+0.5*step,x2+0.5*step,step),over),n=2048,sc1=1.2,i=10,
border="gray15",xlab="",cex1=0.9,cex2=0.9,cex0=0.9,cex=0.9,main="kde")
{
# windows(width=7,height=5)
data1<-data[data<=T & data>=T0]
t<-kde(data=data1,nb=n,q1=0,bw0=bw)
y0<-t$k
mod<-t$u
y0<-round(y0*sc1,3)
Y0<-max(hist(data,br=br,freq=FALSE)$density)
hist(data,br=br,freq=FALSE,main=main,cex.main=0.8,xlab=xlab,ylab="Density",lwd=0.2,xlim=c(x1,x2),ylim=c(0,max(y0,Y0)),
cex.axis=cex0,cex.lab=cex,border=border,axes=FALSE)
f<-round(seq(0,y0,y0/4),3)
f1<-seq(x1,x2,i*bw)
axis(side=2,at=f,pos=c(x1,0),cex.axis=cex1,lwd=1,tcl=-0.25)
axis(side=1,at=f1,pos=c(0,0),cex.axis=cex2,lwd=1,tcl=-0.25)
lines(density(x=data,kernel="gaussian",n=n,cut=3,bw=bw,from=x1,to=x2),col="blue",lty=1)
abline(0,0)
return(list(mod=mod))
}
