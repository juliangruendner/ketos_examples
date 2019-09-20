#cat("R function TNorm.R loaded....\n")
#########################################
TNorm<-
function(d,q1=0.0,T1=quantile(d,q1,na.rm =T),q2,T2=quantile(d,q2,na.rm =T),I=100,epsilon1=1E-4,
mu,sig,er1=0.0)
 {
  # Estimation of parameters of a truncated normal distribution
  # q1 and q2 are left and right quantiles, at which the data is truncated.
  # mu and sig are initial values for estimation.
  q1<-(length(d[d<=T1]))/length(d)
  q2<-(length(d[d<=T2]))/length(d)
  d.trunc<-d[d>T1 & d<T2]
  n<-length(d.trunc); m<-mean(d.trunc); s<-sd(d.trunc); i<-1; mu0<-mu; sig0<-sig; l.old<-1; 
  psi1<-(T1-mu)/sig; psi2<-(T2-mu)/sig;
  Phi1<-pnorm(psi1); Phi2<-pnorm(psi2); Phi<-Phi2-Phi1;
  theta.new<-t(t(c(mu,sig)));
  Diff<-t(t(c(0,0)));
  Ide<-diag(rep(1,2))
  differential<-1
  Hess<-iHess<-matrix(rep(0,4),ncol=2); 
  l.new<-(-n)*(log(Phi))-(n)*(log(sig))-(n/2)*(log(2*pi))-(1/(2*(sig^2)))*(sum((d.trunc-mu)^2))
  cr<-1
  while( i<=I && (differential > epsilon1))
          {#while
          phi1<-dnorm(psi1); phi2<-dnorm(psi2);
          Q1<-phi1/Phi; Q2<-phi2/Phi;
          P1<-Q1*(Q1-psi1);   P2<-(-Q2)*(Q2+psi2);
          la1<-psi1*P1+Q1; la2<-psi2*P2+Q2;
          et1<-psi1*(la1+Q1); et2<-psi2*(la2+Q2);
          jadid<-(-2*psi1*psi2*Q1*Q2)
          n1<-n/sig^2;
         Diff1<-n1*(m-mu-sig*(Q1-Q2));
         Diff2<-n1*((s^2+(m-mu)^2)/sig-sig*(1+psi1*Q1-psi2*Q2));
         Diff<-t(t(c(Diff1,Diff2)));
         Hess[1,1]<-(-n1)*(1-P1+P2);
         Hess[1,2]<-Hess[2,1]<-(-n1)*(2*(m-mu)/sig-la1+la2);
         Hess[2,2]<-(-n1)*(3*(s^2+(m-mu)^2)/sig^2-1-et1+et2-jadid);
         iHess<-try(qr.solve(Hess),TRUE);
         if ((any(is.na(Hess))) | abs(det(Hess)) < 1E-4 | class(iHess) == 'try-error')
            {#if1
                                   l.old<-1;
                                   sig<-mu<-Phi<-l.new<-frac<-differential<-cr<-1E+5
                                   iHess<-matrix(rep(1,4),ncol=2) 
                            i<-I+1
         } else {  
         theta.old<-theta.new;
         theta.new<-theta.old-(iHess)%*%(Diff)  
         mu<-theta.new[1,1];
         sig<-theta.new[2,1];
         mu.old<-theta.old[1,1];
         sig.old<-theta.old[2,1];
         di1<-abs(mu-mu.old)
         di2<-abs(sig-sig.old)
         cr<-max(di1,di2)
         differential<-sqrt(Diff1^2+Diff2^2)
         l.old<-l.new; 
         psi1<-(T1-mu)/sig; psi2<-(T2-mu)/sig;
        Phi1<-pnorm(psi1); Phi2<-pnorm(psi2);
        Phi<-Phi2-Phi1;
        if (sig > 1E-20 & Phi > 1E-20 & mu > T1)
        {#if2                            
        l.new<-(-n)*(log(Phi))-(n)*(log(sig))-(n/2)*(log(2*pi))-(1/(2*(sig^2)))*(sum((d.trunc-mu)^2));
        frac<-min((q2-q1)/Phi,1);
        i<-i+1
         }#if2
        else 
                           {#else2
                           l.old<-1;
                           sig<-mu<-Phi<-l.new<-frac<-differential<-cr<-1E+5
                            i<-I+1
                            }#else2
       }#else1
       }#while
 return(list(q1=q1,q2=q2,T1=T1,T2=T2,I=i-1,mu=mu,sig=sig,Phi=Phi,frac=frac,l=l.new,
Diff=Diff,cov=-iHess,differential=differential))
}
