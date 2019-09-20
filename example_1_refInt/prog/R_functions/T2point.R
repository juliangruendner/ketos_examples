#cat("R function T2point.R loaded....\n")
#########################################
Tpoint<-
function (data,mod=1,com=2,c1=3,c2=3,TL1=min(data),TL2=mod-c1*(10^(-com)),TR1=mod+c2*(10^(-com)),TR2=max(data)) 
{
#  Tpoint function calculates truncation points of data
#  truncation points are determined at the left and right sides of mod of the data
#  at the left side in interval: TL=[TL1,TL2] and at the right side in interval: TR=[TR1,TR2]
#  mod, c1,c2 and com: variables to calculate TL2 and TR1, upper limit of TL and lower limit of TR respectively 
#  mod: mod of data
#  com: accuracy of data values; e.g. for data values 2.11, 1.67 is com=2 and for 144,150 is com=0
#  c1 and c2 define how far from mod TL2 and TR1 are searched
# e.g. for creatinin with data given with two decimals (0.94 or 1.03) and mod=0.88 and for c1=c2=10, TL1, TL2, TR1 and TR2 can be calculated as:
# TL1=min(data) , TR2=max(data),
# TL2= 0.88-10*(10^-2)=0.78, and TR1= 0.88+10*(10^-2)=0.98, and thereby:
# TL=[TL1,0.78], TR=[0.98,TR2]
################################################################  
################################################################ 
 n<-length(data)
 data1<-sort(data)
 h<-T<-rep(0,n)
 i<-1
 while(i<=n-1)
 {
  if (data1[i]!=data1[i+1])
   {
   h[i]<-(i)/n
   T[i]<-data1[i]
   }
  else 
   {
   h[i]<-0
   T[i]<-0
   }
  i<-i+1
 }
 h[n]<-1
 T[n]<-data1[n]
 h1<-h[h!=0]
 T<-T[T!=0]
############################
# this part ensures that some data between TR1 and TR2 exist. If TR1 and TR2 are too near (e.g equal) 
# we make TR2 larger:    
 dataTR<-data1[data1>=TR1 & data1<=TR2]
 m1<-length(dataTR)
 j<-1
 while (j<=100 & m1==0)
       {
        TR2<-TR2+(10^(-com))
        dataTR<-data1[data1>=TR1 & data1<=TR2]
        m1<-length(dataTR)
        j<-j+1
        }
##########################
# this part ensures that some data between TL1 and TL2 exist. If TL1 and TL2 are too near (e.g equal) 
# we make TL2 larger:
 dataTL<-data1[data1>=TL1 & data1<=TL2]
 n1<-length(dataTL)
 k<-1
 while (k<=100 & n1==0)
       {
        TL2<-TL2+(10^(-com))
        dataTL<-data1[data1>=TL1 & data1<=TL2]
        n1<-length(dataTL)
        k<-k+1
        }
############################
 q2<-h1[T>=TR1 & T<=TR2]
 T2<-T[T>=TR1 & T<=TR2]
 q1<-h1[T>=TL1 & T<=TL2]
 T1<-T[T>=TL1& T<=TL2]
 if (TL1==0)
 {
 q1<-c(0,q1)
 T1<-c(0,T1)
}
 return(list(q2=q2,T2=T2,q1=q1,T1=T1))
# T1 and T2 are  vectors of truncation points and q1 and q2 the fractions of the data <=T1 and data <=T2 , respec.
}
###################################
#############################
T2point<-
function (data,mod=1,com=2,c2=3,TR1=mod+c2*(10^(-com)),TR2=max(data)) 
{
#  T2point function calculates truncation points of data
#  truncation points are determined at the right side of mod of the data in interval: TR=[TR1,TR2]
#  mod,c2 and com: variables to calculate TR1, lower limit of TR 
#  mod: mod of data
#  com: accuracy of data values; e.g. for data values 2.11, 1.67 is com=2 and for 144,150 is com=0
# c2 defines how far from mod TR1 is searched
# e.g. for creatinin with data given with two decimals (0.94 or 1.03) and mod=0.88 and for c2=10, TR1 and TR2 can be calculated as:
# , TR2=max(data),
#  TR1= 0.88+10*(10^-2)=0.98, and thereby:
#  TR=[0.98,TR2]
 n<-length(data)
 data1<-sort(data)
 h<-T<-rep(0,n)
 i<-1
 while(i<=n-1)
 {
  if (data1[i]!=data1[i+1])
   {
   h[i]<-i/n
   T[i]<-data1[i]
   }
  else 
   {
   h[i]<-0
   T[i]<-0
   }
  i<-i+1
 }
 h[n]<-1
 T[n]<-data1[n]
 h1<-h[h!=0]
 T<-T[T!=0]
#####################################
# this part ensures that some data between TR1 and TR2 exist. If TR1 and TR2 are too near (e.g equal) 
# we make TR2 larger:  
 dataT<-data1[data1>=TR1 & data1<=TR2]
 m1<-length(dataT)
 j<-1
 while (j<=100 & m1==0)
       {
        TR2<-TR2+(10^(-com))
        dataT<-data1[data1>=TR1 & data1<=TR2]
        m1<-length(dataT)
        j<-j+1
        } 
####################################
 q2<-h1[T>=TR1 & T<=TR2]
 T2<-T[T>=TR1 & T<=TR2] 
 return(list(q2=q2,T2=T2))
# T2 is the vector of truncation points and q2 the fraction of the data <=T2, respec.
}
#######################
T1point<-
function (data,mod=1,com=2,c2=3,TL2=mod-c2*(10^(-com)),TL1=min(data)) 
{
#  T1point function calculates truncation points of data
#  truncation points are determined at the left side of mod of the data in interval: TL=[TL1,TL2]
#  mod,c2 and com: variables to calculate TR1, lower limit of TR 
#  mod: mod of data
#  com: accuracy of data values; e.g. for data values 2.11, 1.67 is com=2 and for 144,150 is com=0
#  c2 defines how far from mod TL2 is searched
#  e.g. for creatinin with data given with two decimals (0.94 or 1.03) and mod=0.88 and for c2=10, TL1 and TL2 
#  can be calculated as:
#  TL1=min(data),
#  TL2= 0.88-10*(10^-2)=0.78, and thereby:
#  TL=[TL1,0.78]
 n<-length(data)
 data1<-sort(data)
 h<-T<-rep(0,n)
 i<-2
 while(i<=n)
 {
  if (data1[i]!=data1[i-1])
   {
   h[i]<-(i-1)/n
   T[i]<-data1[i]
   }
  else 
   {
   h[i]<-0
   T[i]<-0
   }
  i<-i+1
 }
 h[1]<-0
 T[1]<-data1[1]
 h1<-h[T!=0]
 T<-T[T!=0]
##########################
# this part ensures that some data between TL1 and TL2 exist. If TL1 and TL2 are too near (e.g equal) 
# we make TL2 larger:
 dataTL<-data1[data1>=TL1 & data1<=TL2]
 n1<-length(dataTL)
 k<-1
 while (k<=100 & n1==0)
       {
        TL2<-TL2+(10^(-com))
        dataTL<-data1[data1>=TL1 & data1<=TL2]
        n1<-length(dataTL)
        k<-k+1
        }
########################
 q1<-h1[T>=TL1 & T<=TL2]
 T1<-T[T>=TL1 & T<=TL2]
 return(list(q1=q1,T1=T1))
# T1 is the vector of truncation points and q1 the fraction of the data > = T1, respec.
}