# area function finds the area under a curve 
# a and b contain the values (x1,x2, ...,xn) and (y1,y2,...,yn)  
####################################################################
area<-
function (a,b) 
{
# for calculation of area under curve (for fraction of subp.)
# a and b are vectors
n<-length(a);
t<-0;
i<-1;
while(i<=n-1)
        {
        t.old<-t
        t<-t.old+(abs(a[i+1]-a[i]))*(b[i+1]+b[i])/2;
        i<-i+1
        }
return(t)
}