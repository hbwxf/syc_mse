data<-read.csv("SYCdata.csv")
data<-data[(rank(data$Year)),]
plot(small_yellow_croaker_catch~Year,data=data)

#===========================================
# Fit production model
#==========================================

ProdMod<-function(z,CatchData,IndexData)
{
  K<-exp(abs(z[1]))
  r<-abs(z[2])
  q<-log(abs(z[3]))
  B1<-z[4]
  #B1<-K
  predBio<-rep(0,length(IndexData))
  predBio[1]<-B1*K
  for(i in 2:length(CatchData))
  {
    predBio[i]<-predBio[i-1]+r*predBio[i-1]*(1-predBio[i-1]/K)-CatchData[i]
  }
  SSQ<-sum((q*predBio-IndexData)^2,na.rm=T)
  return(SSQ)
}

ProdModPlot<-function(z,CatchData,IndexData,plots=0)
{
  K<-exp(abs(z[1]))
  r<-abs(z[2])
  q<-log(abs(z[3]))
  B1<-z[4]
  #B1<-K
  predBio<-rep(0,length(IndexData)+1)
  predBio[1]<-B1*K
  CatchData<-c(CatchData,0)
  for(i in 2:length(CatchData))
  {
    predBio[i]<-predBio[i-1]+r*predBio[i-1]*(1-predBio[i-1]/K)-CatchData[i]
  }
  if(plots==1)
  {
    plot(IndexData[1:(length(IndexData)-1)],ylim=c(0,max(IndexData,na.rm=T)),las=1,ylab="",xaxt='n',pch=16)
    lines(q*predBio[1:(length(predBio)-1)])
    lines(CatchData[1:(length(CatchData)-1)],lty=2,col=2)
  }
  return(predBio)
}

inCatch	<-data$small_yellow_croaker_catch
inCPUE	<-data$small_yellow_croaker_catch/data$Kilowatts
inYear  <-data$Year

z<-c(17,.6,1.01,.2)
outs		<-nlminb(start=z,objective=ProdMod,CatchData=inCatch,IndexData=inCPUE)
outs		<-nlminb(start=outs$par,objective=ProdMod,CatchData=inCatch,IndexData=inCPUE)
ProdModPlot(z=outs$par,CatchData=inCatch,IndexData=inCPUE,plots=1)


par(mfrow=c(2,1),mar=c(0.1,.1,.1,.1),oma=c(4,4,1,4))
plot(data$small_yellow_croaker_catch/10000~data$Year,las=1,ylim=c(0,11),ylab='',type='l',xaxt='n')
par(new=T)
plot(data$Kilowatts/1000000~data$Year,las=1,ylim=c(0,4),ylab='',type='l',yaxt='n',xaxt='n',lty=2)
legend('bottomright',bty='n',lty=c(1,2),legend=c("Catch","Effort"))
mtext(side=4,"Effort (1,000,000 kilowatts)",line=2)
mtext(side=2,"Catch (10,000 t)",line=2)
axis(side=4,las=1)
ProdModPlot(z=outs$par,CatchData=inCatch,IndexData=inCPUE,plots=1)
#plot(data$small_yellow_croaker_catch/data$Kilowatts~data$Year,las=1,pch=16)
axis(side=1,labels=data$Year,at=seq(1,length(data$Year)))
legend("bottomright",bty='n',pch=c(16,NA),lty=c(NA,1),legend=c("Observed","Predicted"))
mtext(side=2,line=3.3,"Catch per unit effort")

#===================================================
# Fit other data poor models??
#===================================================


#==================================================
# Show violations of the assumptions
#==================================================



