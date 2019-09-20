# pU(x,RL1,RL2): permissable uncertainty evaluates maximum uncertainty value of x with given RL1 and RL2 (rounded at com decimal-points) 
# see R. Haeckel et al. (Equivalence limits of reference intervals for partitioneing of population data. Relevant
# differnces of reference limits (2016), equations (3), (4) and (12)  
# F. Arzideh: 23.09.2016
# F. Arzideh: 14.10.2016, estimated values are now rounded at com decimal-points.
####################################
####################################
####################################
####################################
####################################
pU<-
function (x,RL1,RL2,com) 
{
if(is.na(x) | is.na(RL1) | is.na(RL2) | x <= 0 | RL1 <= 0 | RL2 <= 0 | RL1 >= RL2 )
{
LRLx<-URLx<-NA
}else{
g<-sqrt(RL1*RL2)
sig.ln<-(log(RL2)-log(RL1))/3.92
CV.E<-100*(sqrt(exp(sig.ln**2)-1))
f<-sqrt(CV.E - 0.25)
f.g<-f*g
psA.x<-(0.8/100)*f*x + (0.2/100)*(f.g)
pUx<-psA.x*1.28
LRLx<-round(x-pUx,com)
URLx<-round(x+pUx,com) 
}
return(list(LRLx,URLx))
}
