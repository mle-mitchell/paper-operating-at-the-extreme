

# R codes for "Supplementary Material document to: Operating at the extreme: estimating the upper yield boundary of winter wheat production in commercial practice".





# Note, this is NOT the code for the paper itself but is for the supplementary material. 
# Please see the R codes for "Operating at the extreme: estimating the upper yield boundary of winter wheat production in commercial practice" to reproduce the results in the paper.


# To reproduce the results in the supplementary material, first you will need access to the following Farm Business Survey data files from the UK Data Service (see data availability statement):

# SN 5838 Farm Business Survey, 2006-2007: Special Licence Access,
# SN 6144 Farm Business Survey, 2007-2008: Special Licence Access,
# SN 6387 Farm Business Survey, 2008-2009: Special Licence Access,
# SN 6682 Farm Business Survey, 2009-2010: Special Licence Access,
# SN 6967 Farm Business Survey, 2010-2011: Special Licence Access,
# SN 7231 Farm Business Survey, 2011-2012: Special Licence Access,
# SN 7461 Farm Business Survey, 2012-2013: Special Licence Access,
# SN 7659 Farm Business Survey, 2013-2014: Special Licence Access,
# SN 7914 Farm Business Survey, 2014-2015: Special Licence Access,
# SN 8158 Farm Business Survey, 2015-2016: Special Licence Access.

# Files of the form calcdata_protect contain all of the responses to the Farm Business Survey.
# The supplementary material only uses those concerned with growing winter wheat, where all monetary values are then deflated to their 2010 equivalent, using 
# DEFRA (2018). API – Index of the purchase prices of the means of agricultural production – historical data January 1988 to March 2018 dataset (based on 2010 = 100).
# Technical report, DEFRA Prices Team, available at www.gov.uk/government/statistics/agricultural-price-indices.

# Futhermore, those with zero wheat yield are removed. Yield is defined to be wheat produced (SortOrder=162) divided by wheat area (SortOrder=123).

# In the following code, tbl**** is defined to be responses from year ****.
# Here: tbl****[,1] contains the farm ID and tbl****[,4] contains the yield attained.



# Once these have been setup, please run the following instructions to replicate the results from the supplementary material.

# If you encounter any problems with the following code, please contact emily.mitchell@nottingham.ac.uk in the first instance, or email@emilymitchell.co.uk






#### SECTION B SENSITIVITY OF OUR RESULTS TO DATA SELECTION

# Note, all results discussed in section B are given in Table 1.

# The packages used here are evir and fExtremes.

require(evir)
require(fExtremes)






#### INDIVIDUAL YEARS 2009, 2012 AND 2015

# For each of the years, select one of the following according to which analysis you would like to perform.

## 2009

yields<-tbl2009[,c(1,4)]
n<-length(tbl2009[,1])
k<-140

## 2012

yields<-tbl2012[,c(1,4)]
n<-length(tbl2012[,1])
k<-250

## 2015

yields<-tbl2015[,c(1,4)]
n<-length(tbl2015[,1])
k<-100

#### CODE FOR EVT

MLEV<-shape(yields[,2],start=15,end=400,models=386)

# Shape parameter estimates and confidence intervals
shapevec<-matrix(NA,ncol=4,nrow=386)
for(i in 1:386){
  shapevec[i,1]<-MLEV[2,i]
  shapevec[i,2]<-MLEV[2,i]+1.96*sqrt((1+MLEV[2,i])^2)/sqrt(401-i)
  shapevec[i,3]<-MLEV[2,i]-1.96*sqrt((1+MLEV[2,i])^2)/sqrt(401-i)
  shapevec[i,4]<-MLEV[4,i]
} 

# Scale parameter estimates and confidence intervals for k exceedances
scalevec<-c()
scalevec[1]<-gpd(data=yields[,2],threshold=as.numeric(MLEV[1,401-k]),method="ml")$par.ests[2]
scalevec[2]<-(scalevec[1])/(1+1.96*sqrt((1+(1+shapevec[401-k,1])^2))/sqrt(k))
scalevec[3]<-(scalevec[1])/(1-1.96*sqrt((1+(1+shapevec[401-k,1])^2))/sqrt(k))
scalevec[4]<-MLEV[4,401-k]

# Endpoint estimates and confidence intervals for k exceedances

shapemle<-shape(yields[,2],start=15,end=400,models=386)
estscaleT<-gpd(data=yields[,2],threshold=shapemle[1,401-k],method="ml")$par.ests[2]
endpointsT<-shapemle[1,401-k]-(estscaleT/shapemle[2,401-k])
negvarT<-1+(4*shapemle[2,401-k])+(5*shapemle[2,401-k]^2)+(2*shapemle[2,401-k]^3)+(2*shapemle[2,401-k]^4)
upperCIT<-endpointsT+((1.96*estscaleT*sqrt(negvarT))/(sqrt(shapemle[4,401-k])*(shapemle[2,401-k]^2)))
lowerCIT<-endpointsT-((1.96*estscaleT*sqrt(negvarT))/(sqrt(shapemle[4,401-k])*(shapemle[2,401-k]^2)))  

#### FIGURE 1 (left 2009, centre 2012 and right 2015)

par(mar=c(5,4,1,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(15,400),ylim=c(min(shapevec[,3]),max(shapevec[,2])),cex.lab=0.7,cex.axis=0.7)
lines(rev(c(15:400)),shapevec[,1])
lines(rev(c(15:400)),shapevec[,2],col="grey")
lines(rev(c(15:400)),shapevec[,3],col="grey")
abline(h=0,lty=2)
abline(v=(k),lty=2,col="grey")
axis(1,at=(k),labels=paste(k),font=2,cex.axis=0.7)
lines(c(0,15),c(shapevec[386,1],shapevec[386,1]),lty=3)
lines(c(0,15),c(shapevec[386,2],shapevec[386,2]),col="grey",lty=3)
lines(c(0,15),c(shapevec[386,3],shapevec[386,3]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,1],shapevec[1,1]),lty=3)
lines(c(400,450),c(shapevec[1,2],shapevec[1,2]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,3],shapevec[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=200,cex=0.7)







#### YIELDS FOR A SINGLE RANDOMLY SELECTED YEAR OF DATA FOR EACH FARM

# For each unique farm, we create a table yearly.farm.record of the survey responses they have entered over the 10 years.

# The following code will produce different results each time. If you would like to reproduce the results exactly, please contact the author at the top of the page.

farmyieldmax<-matrix(ncol=3,nrow=length(unique.farms))
for(i in 1:length(unique.farms)){
  farmyieldmax[i,1]<-unique.farms[i]
  ifelse(sum(!is.na(yearly.farm.record[1,,i]))==1,yearRan<-which(!is.na(yearly.farm.record[1,,i])),yearRan<-sample(which(!is.na(yearly.farm.record[1,,i])),1))
  farmyieldmax[i,2]<-yearly.farm.record[1,yearRan,i]
  farmyieldmax[i,3]<-yearRan
}

n<-length(farmyieldmax[,1])
k<-200

#### CODE FOR EVT

MLEV<-shape(farmyieldmax[,2],start=15,end=400,models=386)

# Shape parameter estimates and confidence intervals
shapevec<-matrix(NA,ncol=4,nrow=386)
for(i in 1:386){
  shapevec[i,1]<-MLEV[2,i]
  shapevec[i,2]<-MLEV[2,i]+1.96*sqrt((1+MLEV[2,i])^2)/sqrt(401-i)
  shapevec[i,3]<-MLEV[2,i]-1.96*sqrt((1+MLEV[2,i])^2)/sqrt(401-i)
  shapevec[i,4]<-MLEV[4,i]
} 

# Scale parameter estimates and confidence intervals for k exceedances
scalevec<-c()
scalevec[1]<-gpd(data=farmyieldmax[,2],threshold=as.numeric(MLEV[1,401-k]),method="ml")$par.ests[2]
scalevec[2]<-(scalevec[1])/(1+1.96*sqrt((1+(1+shapevec[401-k,1])^2))/sqrt(k))
scalevec[3]<-(scalevec[1])/(1-1.96*sqrt((1+(1+shapevec[401-k,1])^2))/sqrt(k))
scalevec[4]<-MLEV[4,401-k]

# Endpoint estimates and confidence intervals for k exceedances

shapemle<-shape(farmyieldmax[,2],start=15,end=400,models=386)
estscaleT<-gpd(data=farmyieldmax[,2],threshold=shapemle[1,401-k],method="ml")$par.ests[2]
endpointsT<-shapemle[1,401-k]-(estscaleT/shapemle[2,401-k])
negvarT<-1+(4*shapemle[2,401-k])+(5*shapemle[2,401-k]^2)+(2*shapemle[2,401-k]^3)+(2*shapemle[2,401-k]^4)
upperCIT<-endpointsT+((1.96*estscaleT*sqrt(negvarT))/(sqrt(shapemle[4,401-k])*(shapemle[2,401-k]^2)))
lowerCIT<-endpointsT-((1.96*estscaleT*sqrt(negvarT))/(sqrt(shapemle[4,401-k])*(shapemle[2,401-k]^2)))  


#### FIGURE 2 (left)

par(mar=c(5,4,1,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(15,400),ylim=c(min(shapevec[,3]),max(shapevec[,2])),cex.lab=0.7,cex.axis=0.7)
lines(rev(c(15:400)),shapevec[,1])
lines(rev(c(15:400)),shapevec[,2],col="grey")
lines(rev(c(15:400)),shapevec[,3],col="grey")
abline(h=0,lty=2)
abline(v=(k),lty=2,col="grey")
axis(1,at=(k),labels=paste(k),font=2,cex.axis=0.7)
lines(c(0,15),c(shapevec[386,1],shapevec[386,1]),lty=3)
lines(c(0,15),c(shapevec[386,2],shapevec[386,2]),col="grey",lty=3)
lines(c(0,15),c(shapevec[386,3],shapevec[386,3]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,1],shapevec[1,1]),lty=3)
lines(c(400,450),c(shapevec[1,2],shapevec[1,2]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,3],shapevec[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=200,cex=0.7)







#### MAXIMUM YIELDS OVER A RANDOMLY SELECTED BLOCK OF 5 YEARS OF DATA

# For each unique farm, we create a table yearly.farm.record of the survey responses they have entered over the 10 years.

# The following code will produce different results each time. If you would like to reproduce the results exactly, use subset5<-c(3,5,6,9,10).

subset5<-sample(1:10,5)

unique.farms<-sort(unique(farms.recorded))

yearly.frnona<-yearly.farm.record
farmtemp<-c()
for(i in 1:length(unique.farms)){
  ifelse(sum(is.na(yearly.farm.record[1,subset5,i]))==5,farmtemp[i]<-0,farmtemp[i]<-1)
}
unique.farms<-unique.farms[which(farmtemp==1)]

farmyieldmax<-matrix(ncol=2,nrow=length(unique.farms))
for(i in 1:length(unique.farms)){
  farmyieldmax[i,1]<-unique.farms[i]
  farmyieldmax[i,2]<-max(na.omit(yearly.farm.record[1,subset5,which(farmtemp==1)[i]]))
}

n<-length(farmyieldmax[,1])
k<-230

#### CODE FOR EVT

MLEV<-shape(farmyieldmax[,2],start=15,end=400,models=386)

# Shape parameter estimates and confidence intervals
shapevec<-matrix(NA,ncol=4,nrow=386)
for(i in 1:386){
  shapevec[i,1]<-MLEV[2,i]
  shapevec[i,2]<-MLEV[2,i]+1.96*sqrt((1+MLEV[2,i])^2)/sqrt(401-i)
  shapevec[i,3]<-MLEV[2,i]-1.96*sqrt((1+MLEV[2,i])^2)/sqrt(401-i)
  shapevec[i,4]<-MLEV[4,i]
} 

# Scale parameter estimates and confidence intervals for k exceedances
scalevec<-c()
scalevec[1]<-gpd(data=farmyieldmax[,2],threshold=as.numeric(MLEV[1,401-k]),method="ml")$par.ests[2]
scalevec[2]<-(scalevec[1])/(1+1.96*sqrt((1+(1+shapevec[401-k,1])^2))/sqrt(k))
scalevec[3]<-(scalevec[1])/(1-1.96*sqrt((1+(1+shapevec[401-k,1])^2))/sqrt(k))
scalevec[4]<-MLEV[4,401-k]

# Endpoint estimates and confidence intervals for k exceedances

shapemle<-shape(farmyieldmax[,2],start=15,end=400,models=386)
estscaleT<-gpd(data=farmyieldmax[,2],threshold=shapemle[1,401-k],method="ml")$par.ests[2]
endpointsT<-shapemle[1,401-k]-(estscaleT/shapemle[2,401-k])
negvarT<-1+(4*shapemle[2,401-k])+(5*shapemle[2,401-k]^2)+(2*shapemle[2,401-k]^3)+(2*shapemle[2,401-k]^4)
upperCIT<-endpointsT+((1.96*estscaleT*sqrt(negvarT))/(sqrt(shapemle[4,401-k])*(shapemle[2,401-k]^2)))
lowerCIT<-endpointsT-((1.96*estscaleT*sqrt(negvarT))/(sqrt(shapemle[4,401-k])*(shapemle[2,401-k]^2)))  


#### FIGURE 2 (right)

par(mar=c(5,4,1,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(15,400),ylim=c(min(shapevec[,3]),max(shapevec[,2])),cex.lab=0.7,cex.axis=0.7)
lines(rev(c(15:400)),shapevec[,1])
lines(rev(c(15:400)),shapevec[,2],col="grey")
lines(rev(c(15:400)),shapevec[,3],col="grey")
abline(h=0,lty=2)
abline(v=(k),lty=2,col="grey")
axis(1,at=(k),labels=paste(k),font=2,cex.axis=0.7)
lines(c(0,15),c(shapevec[386,1],shapevec[386,1]),lty=3)
lines(c(0,15),c(shapevec[386,2],shapevec[386,2]),col="grey",lty=3)
lines(c(0,15),c(shapevec[386,3],shapevec[386,3]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,1],shapevec[1,1]),lty=3)
lines(c(400,450),c(shapevec[1,2],shapevec[1,2]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,3],shapevec[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=200,cex=0.7)









#### SECTION C AN ALTERNATIVE VIEW ON CONFIDENCE INTERVALS USING PROFILE LIKELIHOOD

farms.recorded<-c(tbl2006[,1],tbl2007[,1],tbl2008[,1],tbl2009[,1],tbl2010[,1],tbl2011[,1],tbl2012[,1],tbl2013[,1],tbl2014[,1],tbl2015[,1])
unique.farms<-sort(unique(farms.recorded))

# For each unique farm, we select the individual survey response corresponding to the largest yield, to create a table farmyieldmax.csv of maximum yields, with columns Unique Farm ID and corresponding yield.

# The package used here is ismev to perform profile likelihood.

require(ismev)

n<-1536
k<-250

#### CODE FOR EVT

MLET<-shape(farmyieldmax[,2],start=15,end=400,models=386)
colfit<-gpd.fit(farmyieldmax[,2],as.numeric(MLET[1,401-k]),npy=n)

### Profile likelihood for the shape parameter

par(mar=c(5,5,1,1))
colprofxi<-gpd.profxi(colfit, -0.3 , 0.2)
abline(v=0,lty=2)

### Profile likelihood for the endpoint

par(mfrow=c(2,2))
colprof1<-gpd.prof(colfit,0.1*n, 14, 30,npy=n)
colprof2<-gpd.prof(colfit,n, 14, 30,npy=n)
colprof3<-gpd.prof(colfit,10*n,14,30,npy=n)
colprof4<-gpd.prof(colfit,100*n,14,30,npy=n)








