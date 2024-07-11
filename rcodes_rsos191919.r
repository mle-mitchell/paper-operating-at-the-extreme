

# R codes for "Operating at the extreme: estimating the upper yield boundary of winter wheat production in commercial practice".



# To reproduce the results in the paper, first you will need access to the following Farm Business Survey data files from the UK Data Service (see data availability statement):

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
# This paper only uses those concerned with growing winter wheat, where all monetary values are then deflated to their 2010 equivalent, using 
# DEFRA (2018). API – Index of the purchase prices of the means of agricultural production – historical data January 1988 to March 2018 dataset (based on 2010 = 100).
# Technical report, DEFRA Prices Team, available at www.gov.uk/government/statistics/agricultural-price-indices.

# Futhermore, those with zero wheat yield are removed. Yield is defined to be wheat produced (SortOrder=162) divided by wheat area (SortOrder=123).

# In the following code, tbl**** is defined to be responses from year ****.
# Here: tbl****[,1] contains the farm ID, tbl****[,4] contains the yield attained, tbl****[,12] contains the fertiliser costs, tbl****[,13] contains the pesticide costs, and tbl****[,48] characterises the region.

farms.recorded<-c(tbl2006[,1],tbl2007[,1],tbl2008[,1],tbl2009[,1],tbl2010[,1],tbl2011[,1],tbl2012[,1],tbl2013[,1],tbl2014[,1],tbl2015[,1])
unique.farms<-sort(unique(farms.recorded))


# For each unique farm, we select the individual survey response corresponding to the largest yield, to create a table farmyieldmax.csv of maximum yields, with columns Unique Farm ID and corresponding yield.
# The packages used throughout are evir and fExtremes.

require(evir)
require(fExtremes)

# Once these have been setup, please run the following instructions to replicate the results from the paper.

# Note, all results discussed in the paper are given in Table 1.

# If you encounter any problems with the following code, please contact emily.mitchell@nottingham.ac.uk in the first instance, or email@emilymitchell.co.uk

#### FIGURE 1

boxplot(tbl2006[,4],tbl2007[,4],tbl2008[,4],tbl2009[,4],tbl2010[,4],tbl2011[,4],tbl2012[,4],tbl2013[,4],tbl2014[,4],tbl2015[,4],names=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"),xlab="Year",ylab="Yield (tonnes/hectare)",pch=16,cex=0.5,cex.lab=0.7,cex.axis=0.7)

#### CODE FOR EVT

MLET<-shape(farmyieldmax[,2],start=15,end=400,models=386)

# Shape parameter estimates and confidence intervals
shapevec<-matrix(NA,ncol=4,nrow=386)
for(i in 1:386){
  shapevec[i,1]<-MLET[2,i]
  shapevec[i,2]<-MLET[2,i]+1.96*sqrt((1+MLET[2,i])^2)/sqrt(401-i)
  shapevec[i,3]<-MLET[2,i]-1.96*sqrt((1+MLET[2,i])^2)/sqrt(401-i)
  shapevec[i,4]<-MLET[4,i]
} 
  
# Scale parameter estimates and confidence intervals for 250 exceedances
scalevec<-c()
scalevec[1]<-gpd(data=farmyieldmax[,2],threshold=as.numeric(MLET[1,151]),method="ml")$par.ests[2]
scalevec[2]<-(scalevec[1])/(1+1.96*sqrt((1+(1+shapevec[151,1])^2))/sqrt(250))
scalevec[3]<-(scalevec[1])/(1-1.96*sqrt((1+(1+shapevec[151,1])^2))/sqrt(250))
scalevec[4]<-MLET[4,151]

#### FIGURE 3 (left)

par(mar=c(5,4,1,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(15,400),ylim=c(min(shapevec[,3]),max(shapevec[,2])),cex.lab=0.7,cex.axis=0.7)
lines(rev(c(15:400)),shapevec[,1])
lines(rev(c(15:400)),shapevec[,2],col="grey")
lines(rev(c(15:400)),shapevec[,3],col="grey")
abline(h=0,lty=2)
abline(v=250,lty=2,col="grey")
axis(1,at=250,labels="250",font=2,cex.axis=0.7)
lines(c(0,15),c(shapevec[386,1],shapevec[386,1]),lty=3)
lines(c(0,15),c(shapevec[386,2],shapevec[386,2]),col="grey",lty=3)
lines(c(0,15),c(shapevec[386,3],shapevec[386,3]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,1],shapevec[1,1]),lty=3)
lines(c(400,450),c(shapevec[1,2],shapevec[1,2]),col="grey",lty=3)
lines(c(400,450),c(shapevec[1,3],shapevec[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=200,cex=0.7)

#### FIGURE 3 (right)

shapemle<-shape(farmyieldmax[,2],start=15,end=400,models=386)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c()
lowerCI<-c()

for(i in 1:386){
  estscale[i]<-gpd(data=farmyieldmax[,2],threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}
upperCIN<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsN<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCIN<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecneg<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

plot(0,0,col="white",xlim=c(15,400),ylim=c(-10,50),xlab="",ylab="Endpoint estimate",cex.lab=0.7,cex.axis=0.7)
abline(h=0,lty=2)
abline(v=250,lty=2,col="grey")
axis(1,at=250,labels="250",font=2,cex.axis=0.7)
lines(vecneg,endpointsN)
lines(vecneg,upperCIN,col="grey")
lines(vecneg,lowerCIN,col="grey")
lines(c(400,450),c(endpointsN[1],endpointsN[1]),lty=3)
lines(c(400,450),c(upperCIN[1],upperCIN[1]),lty=3,col="grey")
lines(c(400,450),c(lowerCIN[1],lowerCIN[1]),lty=3,col="grey")
ntemp<-length(vecneg)
lines(c(0,tail(vecneg,1)),c(endpointsN[ntemp],endpointsN[ntemp]),lty=3)
lines(c(0,tail(vecneg,1)),c(upperCIN[ntemp],upperCIN[ntemp]),lty=3,col="grey")
lines(c(0,tail(vecneg,1)),c(lowerCIN[ntemp],lowerCIN[ntemp]),lty=3,col="grey")
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=200,cex=0.7)

#### CODES FOR REGION

tempYield<-c(tbl2006[,4],tbl2007[,4],tbl2008[,4],tbl2009[,4],tbl2010[,4],tbl2011[,4],tbl2012[,4],tbl2013[,4],tbl2014[,4],tbl2015[,4])
tempfarms<-c(tbl2006[,1],tbl2007[,1],tbl2008[,1],tbl2009[,1],tbl2010[,1],tbl2011[,1],tbl2012[,1],tbl2013[,1],tbl2014[,1],tbl2015[,1])
tempRegion<-c(tbl2006[,48],tbl2007[,48],tbl2008[,48],tbl2009[,48],tbl2010[,48],tbl2011[,48],tbl2012[,48],tbl2013[,48],tbl2014[,48],tbl2015[,48])
RegionTotal<-cbind(tempfarms,tempRegion,tempYield)

maxRegion<-matrix(ncol=3,nrow=length(unique.farms))
for(i in 1:length(unique.farms)){
  maxRegion[i,1]<-unique.farms[i]
  maxRegion[i,3]<-max(RegionTotal[which(RegionTotal[,1]==unique.farms[i]),3])
  maxRegion[i,2]<-tail(RegionTotal[which(RegionTotal[,1]==unique.farms[i]),2],n=1)
  if(maxRegion[i,2]==421){maxRegion[i,2]<-413}
}

regionmaxyield1<-maxRegion[maxRegion[,2]==411,3]
regionmaxyield2<-maxRegion[maxRegion[,2]==413,3]
regionmaxyield3<-maxRegion[maxRegion[,2]==412,3]

MLE1<-shape(regionmaxyield1,start=15,end=250,models=236)
MLE2<-shape(regionmaxyield2,start=15,end=250,models=236)
MLE3<-shape(regionmaxyield3,start=15,end=250,models=236)

# Shape parameter estimates and confidence intervals
shapevec1<-matrix(NA,ncol=4,nrow=236)
for(i in 1:236){
  shapevec1[i,1]<-MLE1[2,i]
  shapevec1[i,2]<-MLE1[2,i]+1.96*sqrt((1+MLE1[2,i])^2)/sqrt(251-i)
  shapevec1[i,3]<-MLE1[2,i]-1.96*sqrt((1+MLE1[2,i])^2)/sqrt(251-i)
  shapevec1[i,4]<-MLE1[4,i]
}

# Scale parameter estimates and confidence intervals for 68 exceedances
scalevec1<-c()
scalevec1[1]<-gpd(data=regionmaxyield1,threshold=as.numeric(MLE1[1,183]),method="ml")$par.ests[2]
scalevec1[2]<-(scalevec1[1])/(1+1.96*sqrt((1+(1+shapevec1[183,1])^2))/sqrt(68))
scalevec1[3]<-(scalevec1[1])/(1-1.96*sqrt((1+(1+shapevec1[183,1])^2))/sqrt(68))
scalevec1[4]<-MLE1[4,183]

# Shape parameter estimates and confidence intervals
shapevec2<-matrix(NA,ncol=4,nrow=236)
for(i in 1:236){
  shapevec2[i,1]<-MLE2[2,i]
  shapevec2[i,2]<-MLE2[2,i]+1.96*sqrt((1+MLE2[2,i])^2)/sqrt(251-i)
  shapevec2[i,3]<-MLE2[2,i]-1.96*sqrt((1+MLE2[2,i])^2)/sqrt(251-i)
  shapevec2[i,4]<-MLE2[4,i]
}

# Scale parameter estimates and confidence intervals for 115 exceedances
scalevec2<-c()
scalevec2[1]<-gpd(data=regionmaxyield2,threshold=as.numeric(MLE2[1,136]),method="ml")$par.ests[2]
scalevec2[2]<-(scalevec2[1])/(1+1.96*sqrt((1+(1+shapevec2[136,1])^2))/sqrt(115))
scalevec2[3]<-(scalevec2[1])/(1-1.96*sqrt((1+(1+shapevec2[136,1])^2))/sqrt(115))
scalevec2[4]<-MLE2[4,136]

# Shape parameter estimates and confidence intervals
shapevec3<-matrix(NA,ncol=4,nrow=236)
for(i in 1:236){
  shapevec3[i,1]<-MLE3[2,i]
  shapevec3[i,2]<-MLE3[2,i]+1.96*sqrt((1+MLE3[2,i])^2)/sqrt(251-i)
  shapevec3[i,3]<-MLE3[2,i]-1.96*sqrt((1+MLE3[2,i])^2)/sqrt(251-i)
  shapevec3[i,4]<-MLE3[4,i]
}
  
# Scale parameter estimates and confidence intervals for 125 exceedances
scalevec3<-c()
scalevec3[1]<-gpd(data=regionmaxyield3,threshold=as.numeric(MLE3[1,126]),method="ml")$par.ests[2]
scalevec3[2]<-(scalevec3[1])/(1+1.96*sqrt((1+(1+shapevec3[126,1])^2))/sqrt(125))
scalevec3[3]<-(scalevec3[1])/(1-1.96*sqrt((1+(1+shapevec3[126,1])^2))/sqrt(125))
scalevec3[4]<-MLE3[4,126]

#### FIGURE 4 (left)

par(mar=c(5,4,2,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(9,250),ylim=c(-1,0.4),cex.lab=0.7,cex.axis=0.7,main="West England and Wales",cex.main=0.7)
lines(rev(c(15:250)),shapevec2[,1],lwd=0.2)
lines(rev(c(15:250)),shapevec2[,2],col="grey",lwd=0.2)
lines(rev(c(15:250)),shapevec2[,3],col="grey",lwd=0.2)
abline(h=0,lty=2)
abline(v=115,lty=2,col="grey")
lines(c(0,15),c(shapevec2[236,1],shapevec2[236,1]),lty=3,lwd=0.2)
lines(c(0,15),c(shapevec2[236,2],shapevec2[236,2]),col="grey",lty=3,lwd=0.2)
lines(c(0,15),c(shapevec2[236,3],shapevec2[236,3]),col="grey",lty=3,lwd=0.2)
lines(c(250,300),c(shapevec2[1,1],shapevec2[1,1]),lty=3)
lines(c(250,300),c(shapevec2[1,2],shapevec2[1,2]),col="grey",lty=3)
lines(c(250,300),c(shapevec2[1,3],shapevec2[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=125,cex=0.7)

#### FIGURE 4 (middle)

par(mar=c(5,4,2,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(9,250),ylim=c(-1,0.4),cex.lab=0.7,cex.axis=0.7,main="North England",cex.main=0.7)
lines(rev(c(15:250)),shapevec1[,1],lwd=0.2)
lines(rev(c(15:250)),shapevec1[,2],col="grey",lwd=0.2)
lines(rev(c(15:250)),shapevec1[,3],col="grey",lwd=0.2)
abline(h=0,lty=2)
abline(v=68,lty=2,col="grey")
lines(c(0,15),c(shapevec1[236,1],shapevec1[236,1]),lty=3,lwd=0.2)
lines(c(0,15),c(shapevec1[236,2],shapevec1[236,2]),col="grey",lty=3,lwd=0.2)
lines(c(0,15),c(shapevec1[236,3],shapevec1[236,3]),col="grey",lty=3,lwd=0.2)
lines(c(250,300),c(shapevec1[1,1],shapevec1[1,1]),lty=3)
lines(c(250,300),c(shapevec1[1,2],shapevec1[1,2]),col="grey",lty=3)
lines(c(250,300),c(shapevec1[1,3],shapevec1[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=125,cex=0.7)

#### FIGURE 4 (right)

par(mar=c(5,4,2,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(9,250),ylim=c(-1,0.4),cex.lab=0.7,cex.axis=0.7,main="East England",cex.main=0.7)
lines(rev(c(15:250)),shapevec3[,1],lwd=0.2)
lines(rev(c(15:250)),shapevec3[,2],col="grey",lwd=0.2)
lines(rev(c(15:250)),shapevec3[,3],col="grey",lwd=0.2)
abline(h=0,lty=2)
abline(v=125,lty=2,col="grey")
lines(c(0,15),c(shapevec3[236,1],shapevec3[236,1]),lty=3,lwd=0.2)
lines(c(0,15),c(shapevec3[236,2],shapevec3[236,2]),col="grey",lty=3,lwd=0.2)
lines(c(0,15),c(shapevec3[236,3],shapevec3[236,3]),col="grey",lty=3,lwd=0.2)
lines(c(250,300),c(shapevec3[1,1],shapevec3[1,1]),lty=3)
lines(c(250,300),c(shapevec3[1,2],shapevec3[1,2]),col="grey",lty=3)
lines(c(250,300),c(shapevec3[1,3],shapevec3[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=125,cex=0.7)

##### CODES FOR SPRAYS

tempYield<-c(tbl2006[,4],tbl2007[,4],tbl2008[,4],tbl2009[,4],tbl2010[,4],tbl2011[,4],tbl2012[,4],tbl2013[,4],tbl2014[,4],tbl2015[,4])
tempfarms<-c(tbl2006[,1],tbl2007[,1],tbl2008[,1],tbl2009[,1],tbl2010[,1],tbl2011[,1],tbl2012[,1],tbl2013[,1],tbl2014[,1],tbl2015[,1])
tempSprays<-c(tbl2006[,13],tbl2007[,13],tbl2008[,13],tbl2009[,13],tbl2010[,13],tbl2011[,13],tbl2012[,13],tbl2013[,13],tbl2014[,13],tbl2015[,13])
tempFert<-c(tbl2006[,12],tbl2007[,12],tbl2008[,12],tbl2009[,12],tbl2010[,12],tbl2011[,12],tbl2012[,12],tbl2013[,12],tbl2014[,12],tbl2015[,12])
tempSF<-tempSprays+tempFert
SFTotal<-cbind(tempfarms,tempSF,tempYield)
maxSF<-matrix(ncol=3,nrow=length(unique.farms))
for(i in 1:length(unique.farms)){
  maxSF[i,1]<-unique.farms[i]
  maxSF[i,3]<-max(SFTotal[which(SFTotal[,1]==unique.farms[i]),3])
  locmax<-which(SFTotal[which(SFTotal[,1]==unique.farms[i]),3]==maxSF[i,3])
  maxSF[i,2]<-mean(SFTotal[which(SFTotal[,1]==unique.farms[i]),2][locmax])
}
sfmaxyield1<-maxSF[order(maxSF[,2])[1:512],3]
sfmaxyield2<-maxSF[order(maxSF[,2])[513:1024],3]
sfmaxyield3<-maxSF[order(maxSF[,2])[1025:1536],3]

MLELow<-shape(sfmaxyield1,start=15,end=250,models=236)
MLEMed<-shape(sfmaxyield2,start=15,end=250,models=236)
MLEHigh<-shape(sfmaxyield3,start=15,end=250,models=236)

# Shape parameter estimates and confidence intervals
shapevec1<-matrix(NA,ncol=4,nrow=236)
for(i in 1:236){
  shapevec1[i,1]<-MLELow[2,i]
  shapevec1[i,2]<-MLELow[2,i]+1.96*sqrt((1+MLELow[2,i])^2)/sqrt(251-i)
  shapevec1[i,3]<-MLELow[2,i]-1.96*sqrt((1+MLELow[2,i])^2)/sqrt(251-i)
  shapevec1[i,4]<-MLELow[4,i]
}

# Scale parameter estimates and confidence intervals for 90 exceedances
scalevec1<-c()
scalevec1[1]<-gpd(data=sfmaxyield1,threshold=as.numeric(MLELow[1,161]),method="ml")$par.ests[2]
scalevec1[2]<-(scalevec1[1])/(1+1.96*sqrt((1+(1+shapevec1[161,1])^2))/sqrt(90))
scalevec1[3]<-(scalevec1[1])/(1-1.96*sqrt((1+(1+shapevec1[161,1])^2))/sqrt(90))
scalevec1[4]<-MLELow[4,161]

# Shape parameter estimates and confidence intervals
shapevec2<-matrix(NA,ncol=4,nrow=236)
for(i in 1:236){
  shapevec2[i,1]<-MLEMed[2,i]
  shapevec2[i,2]<-MLEMed[2,i]+1.96*sqrt((1+MLEMed[2,i])^2)/sqrt(251-i)
  shapevec2[i,3]<-MLEMed[2,i]-1.96*sqrt((1+MLEMed[2,i])^2)/sqrt(251-i)
  shapevec2[i,4]<-MLEMed[4,i]
}

# Scale parameter estimates and confidence intervals for 80 exceedances
scalevec2<-c()
scalevec2[1]<-gpd(data=sfmaxyield2,threshold=as.numeric(MLEMed[1,171]),method="ml")$par.ests[2]
scalevec2[2]<-(scalevecMed1)/(1+1.96*sqrt((1+(1+shapevec2[171,1])^2))/sqrt(251-171))
scalevec2[3]<-(scalevecMed1)/(1-1.96*sqrt((1+(1+shapevec2[171,1])^2))/sqrt(251-171))
scalevec2[4]<-MLEMed[4,171]

# Shape parameter estimates and confidence intervals
shapevec3<-matrix(NA,ncol=4,nrow=236)
for(i in 1:236){
  shapevec3[i,1]<-MLEHigh[2,i]
  shapevec3[i,2]<-MLEHigh[2,i]+1.96*sqrt((1+MLEHigh[2,i])^2)/sqrt(251-i)
  shapevec3[i,3]<-MLEHigh[2,i]-1.96*sqrt((1+MLEHigh[2,i])^2)/sqrt(251-i)
  shapevec3[i,4]<-MLEHigh[4,i]
}

# Scale parameter estimates and confidence intervals for 100 exceedances
scalevec3<-c()
scalevec3[1]<-gpd(data=sfmaxyield3,threshold=as.numeric(MLEHigh[1,151]),method="ml")$par.ests[2]
scalevec3[2]<-(scalevec3[1])/(1+1.96*sqrt((1+(1+shapevec3[151,1])^2))/sqrt(100))
scalevec3[3]<-(scalevec3[1])/(1-1.96*sqrt((1+(1+shapevec3[151,1])^2))/sqrt(100))
scalevec3[4]<-MLEHigh[4,151]

#### FIGURE 5 (left)

par(mar=c(5,4,2,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(9,250),ylim=c(-1.3,0.6),cex.lab=0.7,cex.axis=0.7,main="Low inputs",cex.main=0.7)
lines(rev(c(15:250)),shapevec1[,1],lwd=0.2)
lines(rev(c(15:250)),shapevec1[,2],col="grey",lwd=0.2)
lines(rev(c(15:250)),shapevec1[,3],col="grey",lwd=0.2)
abline(h=0,lty=2)
lines(c(0,15),c(shapevec1[236,1],shapevec1[236,1]),lty=3,lwd=0.2)
lines(c(0,15),c(shapevec1[236,2],shapevec1[236,2]),col="grey",lty=3,lwd=0.2)
lines(c(0,15),c(shapevec1[236,3],shapevec1[236,3]),col="grey",lty=3,lwd=0.2)
lines(c(250,300),c(shapevec1[1,1],shapevec1[1,1]),lty=3)
lines(c(250,300),c(shapevec1[1,2],shapevec1[1,2]),col="grey",lty=3)
lines(c(250,300),c(shapevec1[1,3],shapevec1[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=125,cex=0.7)
abline(v=90,lty=2,col="grey")

#### FIGURE 5 (middle)

par(mar=c(5,4,2,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(9,250),ylim=c(-1.3,0.6),cex.lab=0.7,cex.axis=0.7,main="Medium inputs",cex.main=0.7)
lines(rev(c(15:250)),shapevec2[,1],lwd=0.2)
lines(rev(c(15:250)),shapevec2[,2],col="grey",lwd=0.2)
lines(rev(c(15:250)),shapevec2[,3],col="grey",lwd=0.2)
abline(h=0,lty=2)
lines(c(0,15),c(shapevec2[236,1],shapevec2[236,1]),lty=3,lwd=0.2)
lines(c(0,15),c(shapevec2[236,2],shapevec2[236,2]),col="grey",lty=3,lwd=0.2)
lines(c(0,15),c(shapevec2[236,3],shapevec2[236,3]),col="grey",lty=3,lwd=0.2)
lines(c(250,300),c(shapevec2[1,1],shapevec2[1,1]),lty=3)
lines(c(250,300),c(shapevec2[1,2],shapevec2[1,2]),col="grey",lty=3)
lines(c(250,300),c(shapevec2[1,3],shapevec2[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=125,cex=0.7)
abline(v=80,lty=2,col="grey")

##### FIGURE 5 (right)

par(mar=c(5,4,2,1))
plot(0,0,xlab="",ylab="Shape parameter estimate",col="white",xlim=c(9,250),ylim=c(-1.3,0.6),cex.lab=0.7,cex.axis=0.7,main="High inputs",cex.main=0.7)
lines(rev(c(15:250)),shapevec3[,1],lwd=0.2)
lines(rev(c(15:250)),shapevec3[,2],col="grey",lwd=0.2)
lines(rev(c(15:250)),shapevec3[,3],col="grey",lwd=0.2)
abline(h=0,lty=2)
lines(c(0,15),c(shapevec3[236,1],shapevec3[236,1]),lty=3,lwd=0.2)
lines(c(0,15),c(shapevec3[236,2],shapevec3[236,2]),col="grey",lty=3,lwd=0.2)
lines(c(0,15),c(shapevec3[236,3],shapevec3[236,3]),col="grey",lty=3,lwd=0.2)
lines(c(250,300),c(shapevec3[1,1],shapevec3[1,1]),lty=3)
lines(c(250,300),c(shapevec3[1,2],shapevec3[1,2]),col="grey",lty=3)
lines(c(250,300),c(shapevec3[1,3],shapevec3[1,3]),col="grey",lty=3)
mtext(expression(paste("Effective sample size, ", k)),side=1,line=3,at=125,cex=0.7)
abline(v=100,lty=2,col="grey")

#### TABLE 1 VALUES

# YIELD

length(farmyieldmax[,2])
shapevec[151,]
estscale<-gpd(data=farmyieldmax[,2],threshold=MLET[1,151],method="ml")$par.ests[2]

# CONFIDENCE INTERVALS FOR YIELD

shapemle<-shape(farmyieldmax[,2],start=15,end=400,models=386)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c()
lowerCI<-c()

for(i in 1:386){
  estscale[i]<-gpd(data=farmyieldmax[,2],threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCIN<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsN<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCIN<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecneg<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCIN,endpointsN,upperCIN,vecneg)[151,]

########### LOCATION ###########################

# NORTH

length(regionmaxyield1)
shapevec1[183,]
estscale1<-gpd(data=regionmaxyield1,threshold=MLE1[1,183],method="ml")$par.ests[2]

# CONFIDENCE INTERVALS FOR NORTH ENGLAND ENDPOINT

shapemle<-shape(regionmaxyield1,start=15,end=250,models=236)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c ()
lowerCI<-c()

for(i in 1:236){
  estscale[i]<-gpd(data=regionmaxyield1,threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCINL<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsNL<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCINL<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecnegL<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCINL,endpointsNL,upperCINL,vecnegL)[183,]

# WEST ENGLAND AND WALES

length(regionmaxyield2)
shapevec2[136,]
estscale2<-gpd(data=regionmaxyield2,threshold=MLE2[1,136],method="ml")$par.ests[2]

# CONFIDENCE INTERVALS FOR WEST ENGLAND AND WALES ENDPOINT

shapemle<-shape(regionmaxyield2,start=15,end=250,models=236)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c ()
lowerCI<-c()

for(i in 1:236){
  estscale[i]<-gpd(data=regionmaxyield2,threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCINL<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsNL<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCINL<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecnegL<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCINL,endpointsNL,upperCINL,vecnegL)[136,]


# EAST ENGLAND

length(regionmaxyield3)

shapevec3[126,]
estscale3<-gpd(data=regionmaxyield3,threshold=MLE3[1,126],method="ml")$par.ests[2]

# CONFIDENCE INTERVALS FOR EAST ENGLAND ENDPOINT

shapemle<-shape(regionmaxyield3,start=15,end=250,models=236)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c ()
lowerCI<-c()

for(i in 1:236){
  estscale[i]<-gpd(data=regionmaxyield3,threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCINL<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsNL<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCINL<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecnegL<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCINL,endpointsNL,upperCINL,vecnegL)[126,]

########### SPRAYS ###########################

# LOW

length(sfmaxyield1)
shapevec1[161,]
estscale1<-gpd(data=sfmaxyield1,threshold=MLELow[1,161],method="ml")$par.ests[2]

## CONFIDENCE INTERVAL FOR LOW ENDPOINT

shapemle<-shape(sfmaxyield1,start=15,end=250,models=236)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c ()
lowerCI<-c()

for(i in 1:236){
  estscale[i]<-gpd(data=sfmaxyield1,threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCINL<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsNL<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCINL<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecnegL<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCINL,endpointsNL,upperCINL)[161,]

# MEDIUM

length(sfmaxyield2)
shapevec2[171,]
estscale2<-gpd(data=sfmaxyield2,threshold=MLEMed[1,171],method="ml")$par.ests[2]

# CONFIDENCE INTERVAL FOR MEDIUM ENDPOINT

shapemle<-shape(sfmaxyield2,start=15,end=240,models=226)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c ()
lowerCI<-c()

for(i in 1:226){
  estscale[i]<-gpd(data=sfmaxyield2,threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCINM<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsNM<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCINM<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecnegM<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCINM,endpointsNM,upperCINM)[161,]

# HIGH

length(sfmaxyield3)
shapevec3[151,]
estscale3<-gpd(data=sfmaxyield3,threshold=MLEHigh[1,151],method="ml")$par.ests[2]

# CONFIDENCE INTERVAL FOR HIGH ENDPOINT

shapemle<-shape(sfmaxyield3,start=15,end=250,models=236)
estscale<-c()
endpoints<-c()
negvar<-c()
upperCI<-c ()
lowerCI<-c()

for(i in 1:236){
  estscale[i]<-gpd(data=sfmaxyield3,threshold=shapemle[1,i],method="ml")$par.ests[2]
  endpoints[i]<-shapemle[1,i]-(estscale[i]/shapemle[2,i])
  negvar[i]<-1+(4*shapemle[2,i])+(5*shapemle[2,i]^2)+(2*shapemle[2,i]^3)+(2*shapemle[2,i]^4)
  upperCI[i]<-endpoints[i]+((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
  lowerCI[i]<-endpoints[i]-((1.96*estscale[i]*sqrt(negvar[i]))/(sqrt(shapemle[4,i])*(shapemle[2,i]^2)))
}

upperCINH<-upperCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
endpointsNH<-endpoints[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
lowerCINH<-lowerCI[which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]
vecnegH<-shapemle[4,which(shapemle[2,]<0 & shapemle[2,]>(-0.5))]

cbind(lowerCINH,endpointsNH,upperCINH,vecnegH)[151,]
