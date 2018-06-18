################################
# Set up life history
################################
years<-seq(1960,2016)
totYears<-seq(1960,2031)
ages<-seq(0,10)
lengths<-seq(0,40)
# these are taken from Shan's slide for the yellow sea
kappa<-c(seq(0.26,0.4,length.out=26),seq(0.4,0.48,length.out=14),seq(0.48,0.56,length.out=17),rep(0.56,16))
linf<-c(seq(34.2,30.17,length.out=26),seq(30.17,25.54,length.out=14),seq(25.54,24.06,length.out=17),rep(24.06,16))
tzero<-c(seq(-0.58,-0.37,length.out=26),seq(-0.37,-0.3,length.out=14),seq(-0.3,-0.25,length.out=17),rep(-0.25,16))
natmort<-c(seq(.24,.33,length.out=26),seq(.33,.39,length.out=14),seq(.39,.43,length.out=17),rep(0.43,16))
fmort<-c(seq(.27,1.47,length.out=26),seq(1.47,2.45,length.out=14),seq(2.45,2.46,length.out=17),rep(2.46,16))

#==fishery stuff
fish_sel_50<-seq(21,11,length.out=length(totYears))
fish_sel_95<-seq(26,14,length.out=length(totYears))
  
#==maturity 
mat50<-c(seq(3,.8,length.out=71))
mat95<-c(seq(5,1,length.out=71))

#==plot maturity curve
mat_at_age<-matrix(nrow=length(totYears),ncol=length(ages))
for(x in 1:length(totYears))
  for(y in 1:length(ages))
    mat_at_age[x,y]<- 1 / (1+exp(-log(19) * (ages[y] - mat50[x])/(mat95[x]-mat50[x])))

inCol<-grey.colors(nrow(mat_at_age),start=.1,end=.9)
plot(mat_at_age[1,]~ages,type='l',las=1,ylab='',xlab='')
for(x in 1:nrow(mat_at_age))
  lines(mat_at_age[x,]~ages,col=inCol[x])
lines(mat_at_age[1,]~ages)

#==growth curve
len_at_age<-matrix(nrow=length(years),ncol=length(ages))
for(x in 1:length(years))
  for(y in 1:length(ages))
    len_at_age[x,y]<-linf[x]*(1-exp(-kappa[x]*(ages[y]-tzero[x])))

inCol<-grey.colors(nrow(len_at_age),start=.1,end=.99)
plot(len_at_age[1,]~ages,type='l',las=1,ylab='',xlab='')
for(x in 1:nrow(len_at_age))
  lines(len_at_age[x,]~ages,col=inCol[x])


#==weight at length (a = 0.00000943, b= 3.1161)
alpha<-c(seq(0.0000943,0.00000943,length.out=71))
beta<-c(seq(3,3.1161,length.out=71))

wt_at_len<-matrix(nrow=length(years),ncol=length(lengths))
for(x in 1:length(years))
  for(y in 1:length(lengths))
   wt_at_len[x,y]<-alpha[x]*lengths[y]^beta[x]

inCol<-grey.colors(nrow(wt_at_len),start=.1,end=.99)
plot(wt_at_len[1,]~lengths,type='l',las=1,ylab='',xlab='')
for(x in 1:nrow(wt_at_len))
  lines(wt_at_len[x,]~lengths,col=inCol[x])

#==plot selectivity at length curve
sel_at_len<-matrix(nrow=length(years),ncol=length(lengths))
for(x in 1:length(years))
  for(y in 1:length(lengths))
    sel_at_len[x,y]<-1 / (1+exp(-log(19) * (lengths[y] - fish_sel_50[x])/(fish_sel_95[x]-fish_sel_50[x])))

inCol<-grey.colors(nrow(sel_at_len),start=.1,end=.99)
plot(sel_at_len[1,]~lengths,type='l',las=1,ylab='',xlab='')
for(x in 1:nrow(len_at_age))
  lines(sel_at_len[x,]~lengths,col=inCol[x])


#######################################
# Run simulation
#######################################

CurDir<-"C:/Users/Cody/Desktop/small_yellow_croaker_MSE/"
setwd(CurDir)
source("C:/GeMS/General_MSE_helpers.R")
source("C:/GeMS/General_MSE_main.R")

CreateFolderNameList<-c("Master")
				
#==Loop that reads in CTL files stored in CurDir and executes code
for(x in 1:length(CreateFolderNameList))
{
 Inout<-ReadCTLfile(input=paste(CurDir,CreateFolderNameList[x],"_CTL.csv",sep=""))
 GeMS(out=Inout,CreateFolderName=CreateFolderNameList[x])
}


##########################################
# Plot output
##########################################
Inout<-ReadCTLfile(input=paste(CurDir,CreateFolderNameList[1],"_CTL.csv",sep=""))
#======================================================
Data<-rep(list(list()))
for(x in 1:length(CreateFolderNameList))
 Data[[x]]<-readLines(paste(CurDir,CreateFolderNameList[x],"/ProdOutputs.csv",sep=""))

#==estimated and true CPUE
estCPUE<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))
trueCPUE<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))
trueBMSY<-rep(0,length(CreateFolderNameList))
estBMSY<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))
trueFMSY<-rep(0,length(CreateFolderNameList))
estFMSY<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))
estTAC<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))
trueTAC<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))
trueCatch<-array(dim=c(Inout$OM$Nsim,Inout$OM$SimYear,length(CreateFolderNameList)))

for(y in 1:length(CreateFolderNameList))
{
 tmp<-grep("est cpue",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 estCPUE[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))

tmp<-grep("true CPUE",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 trueCPUE[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))

tmp<-grep("true BMSY",Data[[y]])
trueBMSY[y]<-as.numeric(Data[[y]][(tmp+1)])

tmp<-grep("est BMSY",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 estBMSY[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))

tmp<-grep("true FMSY",Data[[y]])
trueFMSY[y]<-as.numeric(Data[[y]][(tmp+1)])

tmp<-grep("est FMSY",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 estFMSY[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))

 tmp<-grep("est total allowable catch",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 estTAC[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))

tmp<-grep("true total allowable catch",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 trueTAC[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))

tmp<-grep("true Catch",Data[[y]])
Quant<-Data[[y]][(tmp+1):(tmp+Inout$OM$Nsim)]
for(x in 1:length(Quant))
 trueCatch[x,,y]<-as.numeric(unlist(strsplit(Quant[x],split=" ")))
}

#==================================
# Biomass and catch
#==================================
dev.new()
par(mfcol=c(2,length(CreateFolderNameList)),mar=c(.1,.1,.1,.1),oma=c(4,6,1,4))

for(y in 1:length(CreateFolderNameList))
{
boxplot(trueCPUE[,40:ncol(trueCatch),y],type="l",ylim=c(0,max(trueCPUE,na.rm=T)),
 las=1,xaxt='n',ylab='',yaxt='n')
for(x in 1:nrow(estCPUE))
 lines(estCPUE[x,40:ncol(trueCatch),y],col='#ff000033')
if(y==1)
{
 axis(side=2,las=1)
 mtext(side=2,"Biomass",line=4,cex=.7)
}

plot(trueCatch[1,40:ncol(trueCatch),y]~totYears,col=4,ylim=c(0,max(trueCatch,na.rm=T)),
 las=1,ylab='')

for(x in 1:nrow(trueCatch))
 lines(trueCatch[x,40:ncol(trueCatch),y]~totYears,type="l",col=4)
}

dev.new()
par(mfcol=c(4,length(CreateFolderNameList)),mar=c(.1,.1,.1,.1),oma=c(4,6,1,4))

for(y in 1:length(CreateFolderNameList))
{
boxplot(trueCPUE[,,y],type="l",ylim=c(0,max(trueCPUE,na.rm=T)),
 las=1,xaxt='n',ylab='',yaxt='n')
for(x in 1:nrow(estCPUE))
 lines(estCPUE[x,,y],col='#ff000033')
if(y==1)
{
 axis(side=2,las=1)
 mtext(side=2,"Biomass",line=4,cex=.7)
}
temp<-sweep(estBMSY[,,y],MAR=2,trueBMSY[y],FUN="-")
RelativeErrorBMSY<-sweep(temp,MAR=2,trueBMSY[y],FUN="/")
boxplot(RelativeErrorBMSY[,Inout$OM$InitYear:Inout$OM$SimYear],
 las=1,yaxt='n',xaxt='n',ylim=c(-1.7,1.7))
abline(h=0,lty=2)

if(y==1)
{
 axis(side=2,las=1)
 mtext(side=2,"Relative error",line=2.5,cex=.7)
 mtext(side=2,"Target biomass",line=3.5,cex=.7)
}

temp<-sweep(estFMSY[,,y],MAR=2,trueFMSY[y],FUN="-")
RelativeErrorBMSY<-sweep(temp,MAR=2,trueFMSY[y],FUN="/")
boxplot(RelativeErrorBMSY[,Inout$OM$InitYear:Inout$OM$SimYear],
 las=1,yaxt='n',xaxt='n',ylim=c(-1.7,1.7))
abline(h=0,lty=2)

if(y==1)
{
 axis(side=2,las=1)
 mtext(side=2,"Relative error",line=2.5,cex=.7)
 mtext(side=2,"Target fishing mortality",line=3.5,cex=.7)
}

boxplot(trueTAC[,,y],type="l",ylim=c(0,max(trueTAC,na.rm=T)),
 las=1,xaxt='n',ylab='',yaxt='n')
for(x in 1:nrow(estCPUE))
 lines(estTAC[x,,y],col='#ff000033')
if(y==1)
{
 axis(side=2,las=1)
 mtext(side=2,"Total allowable catch",line=3.5,cex=.7)
}
}

#==============================================
# Plot one example
#==============================================

