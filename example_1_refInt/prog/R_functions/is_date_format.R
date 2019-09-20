#cat("R function is_date_fomrat.R loaded....\n")
##################
is_date_format <- function(s)
{
    if(!is.na(s)){
    # get characters following ``%'' characters
    v <- character(3)
    j <- 1
    k <- 1
    while (k <= nchar(s)) {
        if (substr(s, k, k) == "%") {
            v[j] <- substr(s, k + 1, k + 1)
            j <- j + 1
            k <- k + 2
            if (j > 4)
                return (FALSE)
        } else if (substr(s, k, k) %in% c(" ", ".", "/", "-")) {
            k <- k + 1
        } else {
            return (FALSE)
        }
    }

    for (k in 1:nchar(s)) {


    }
    if (j < 4)
        return (FALSE)

    v <- paste(v, collapse = "")

    # valid day
    if (length(grep("d", v)) != 1)
        return (FALSE)

    # valid month
    if (length(grep("[bBm]", v)) != 1)
        return (FALSE)

    # valid year
    if (length(grep("[yY]", v)) != 1)
        return (FALSE)

    return (TRUE)
    }else{
    return(2)
    } 
}

