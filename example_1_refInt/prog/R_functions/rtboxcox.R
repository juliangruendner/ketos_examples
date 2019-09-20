#cat("R function rtboxcox.R loaded....\n")
###########################################
rtboxcox<-
function (n, lambda, lambda2 = NULL, mean = 0, sd = 1,down=0,up=Inf) 
{
    # generates dataset from a two-sided truncated power normal distribution
    # for lambda >=0 
    if (is.null(lambda2)) 
        lambda2 <- 0
    if (is.na(lambda2)) 
        lambda2 <- 0
    if (lambda==0)
    {
    down<-log(down)
    up<-log(up)
    }
    else
    {
    down<-((down^lambda)-1)/lambda
    up<-((up^lambda)-1)/lambda
    }
    xn <- rtnorm(n = n, mean = mean, sd = sd,lower=down,upper=up)
    if (isTRUE(all.equal(unname(lambda), 0))) 
    xbc <- exp(xn)
    else 
    {
    xbc <- ((xn * lambda) + 1)^(1/lambda)
    }
    return(xbc - lambda2)
}