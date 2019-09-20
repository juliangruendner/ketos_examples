############################################
# age.R                                    #
############################################
alt_kalk <- function(geb, end1=Sys.Date(), unit="years"){
  if (!inherits(geb, "Date") | !inherits(end1, "Date"))
    stop("geb und end1 sollen Date-class-Objekte sein!")
  start <- as.POSIXlt(geb)
  end <- as.POSIXlt(end1)

  years <- end$year - start$year
  if (unit=='years'){
     age <- ifelse((end$mon < start$mon) |
                      ((end$mon == start$mon) & (end$mday < start$mday)),
                      years - 1, years)
     }else if(unit=='months'){
     month1 <- (years) * 12
     diffmonth <- end$mon - start$mon
     age0 <-ifelse(end$mday < start$mday, diffmonth-1, diffmonth)
     age<-month1+resultm
     }
  return(age)
  }