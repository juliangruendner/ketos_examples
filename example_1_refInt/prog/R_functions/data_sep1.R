# data.sep- function returns values of male and female for the age group: a1< age <= a2 separately and together
# c1, c2 and c3: columns of age, value and sex, resp.
# a1 and a2: age ranges
# m and w: sex indicators
# all: male+female
# w: female
# m: male
####################################
# Arzideh, 25.07.2016: modified for the case that
# age-column is not available.
####################################
####################################
####################################
####################################
data.sep<-
function (data=data,c1=4,c2=5,c3=3,a1=16,a2=120,m="M",w="W") 
{
if(!is.na(c1))
{
data1<-subset(data,subset=(data[,c1]>=a1 & data[,c1]<=a2))
}
data1<-data1[!is.na(data1[,c2]),]
data1<-data1[data1[,c2] >0,]
d<-data1       # data male+female subjects (age group: a1< age <= a2)
dm<-data1[data1[,c3]==m,]  # data for male subjects (age group: a1< age <= a2)
dw<-data1[data1[,c3]==w,]  # data for female subjects (age group: a1< age <= a2)
return(list(all=d,w=dw,m=dm))
}