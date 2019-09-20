##################################################################
# DL_pnm function calculates RLs for power-normal distribution ###
# updated: 29.09.2014 (without DL)                             ###
# Autor: Arzideh                                               ###
##################################################################
#library(msm)
#library(geoR)
#####################################
DL.pnm<-
function (mu0,sig0,p,lam,k2,TL) 
{
# DL.pnm function: 22.04.09
# p, mu0, sig0 and lam are the estimated parameters for the distr. of non-pathological values
# (xd, yd) and (xd,k2) display the coordinates of the distr. of non-pathological and pathological values, correspondingly.
# high is a limit value, which   
########################################
dl975<-qtnorm(0.975,mu0,sig0,lower=TL)
dl950<-qtnorm(0.95,mu0,sig0,lower=TL)
density975<-dtnorm(dl975,mu0,sig0,lower=TL)
density950<-dtnorm(dl950,mu0,sig0,lower=TL)
DL975<-(dl975*lam+1)^(1/lam)
DL950<-(dl950*lam+1)^(1/lam)
Den975<-(density975)*(DL975^(lam-1))
Den950<-(density950)*(DL950^(lam-1))
####################################
dl25<-qtnorm(0.025,mu0,sig0,lower=TL)
dl5<-qtnorm(0.05,mu0,sig0,lower=TL)
density25<-dtnorm(dl25,mu0,sig0,lower=TL)
density5<-dtnorm(dl5,mu0,sig0,lower=TL)
DL25<-(dl25*lam+1)^(1/lam)
DL5<-(dl5*lam+1)^(1/lam)
Den25<-(density25)*(DL25^(lam-1))
Den5<-(density5)*(DL5^(lam-1))
#########################
return(list(DL25=DL25,DL5=DL5,DL975=DL975,DL950=DL950,
Den25=Den25,Den5=Den5,Den975=Den975,Den950=Den950,mu=mu0,sigma=sig0,P=p))
}
