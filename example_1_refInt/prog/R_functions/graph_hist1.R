#source("kde.txt")
# graph.hist.R  displays the hist and kde and returns mod of the data
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
############################
############################
############################
############################
#cat("R function graph_hist1.R loaded....\n")
############################
graph.hist<-
function (data=ka1,T=max(data),T0=min(data),x1=min(data),below=-10,x2=max(data),over=max(data),
bw=1,step,br=c(below,seq(x1+0.5*step,x2+0.5*step,step),over),n=2048,sc1=1.2,i=10,
border="khaki3",xlab="",cex1=0.9,cex2=0.9,cex0=0.9,cex=0.9,main="kde")
{
data1<-data[data<=T & data>=T0]
t<-kde(data=data1,nb=n,q1=0,bw0=bw)
mod<-t$u
return(list(mod=mod))
}
