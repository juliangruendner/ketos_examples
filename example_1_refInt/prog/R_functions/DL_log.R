################################################################
# DL_log function calculates RLs for log-normal distribution ###
# updated: 29.09.2014 (without DL)                           ###
# Autor: Arzideh                                             ###
################################################################
#library(msm)
#library(geoR)
##############
DL.log<-
function (mu0,sig0,p,k2) 
{
# DL.log function: 25.01.10
# p, mu0, sig0 and lam are the estimated parameters for the distr. of non-pathological values
# (xd, yd) and (xd,k2) display the coordinates of the distr. of non-pathological and pathological values, correspondingly.
# high is a limit value, which   
########################################
dl975<-qnorm(0.975,mu0,sig0)
dl950<-qnorm(0.95,mu0,sig0)
density975<-dnorm(dl975,mu0,sig0)
density950<-dnorm(dl950,mu0,sig0)
DL975<-exp(dl975)
DL950<-exp(dl950)
Den975<-(density975)/(DL975)
Den950<-(density950)/(DL950)
dl25<-qnorm(0.025,mu0,sig0)
dl5<-qnorm(0.05,mu0,sig0)
density25<-dnorm(dl25,mu0,sig0)
density5<-dnorm(dl5,mu0,sig0)
DL25<-exp(dl25)
DL5<-exp(dl5)
Den25<-(density25)/(DL25)
Den5<-(density5)/(DL5)
return(list(DL25=DL25,DL5=DL5,DL975=DL975,DL950=DL950,
Den25=Den25,Den5=Den5,Den975=Den975,Den950=Den950,mu=mu0,sigma=sig0,P=p))
}