#cd Box\ Sync/GeMS/General-MSE/syc_mse
#rt

MSEdir <- MSEDir <- CurDir <- getwd()

library(GeMS)

###########

CTLNameList<-CTLName<-CTLNames<-CreateFolderName<-CreateFolderNameList<-c("SYC_1_noTV_2")
CTLNameList<-CTLName<-CTLNames<-CreateFolderName<-CreateFolderNameList<-c("SYC_2_allGAMs_2")
CTLNameList<-CTLName<-CTLNames<-CreateFolderName<-CreateFolderNameList<-c("SYC_3_changeM_2")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
run_GeMS(CreateFolderNameList,MSEdir,silent=T)


#########

CreateFolderNameList<-c("SYC_1_noTV_1","SYC_2_allGAMs_1","SYC_3_changeM_1")
CreateFolderNameList<-c("SYC_1_noTV_2","SYC_2_allGAMs_2","SYC_3_changeM_2")
CreateFolderNameList<-c("SYC_1_noTV_3","SYC_2_allGAMs_3","SYC_3_changeM_3")
CreateFolderNameList<-c("SYC_1_noTV_4","SYC_2_allGAMs_4","SYC_3_changeM_4")
run_GeMS(CreateFolderNameList,runparallel=T,cores=3,silent=T)

out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
ProductionModelOutput(Inout,CreateFolderNameList,MSEdir,
                      plotNames=c("Time-Invariant",
                                  "Increasing M", 
                                  "Decreasing M"))

#########

CTLNameList<-prodCTLNames <- CreateFolderNameList <- c("SYC_1_noTV_1","SYC_2_allGAMs_1","SYC_3_changeM_1")
prodCTLNames<-c("SYC_1_noTV_1","SYC_1_noTV_1a",
                "SYC_2_allGAMs_1","SYC_2_allGAMs_1a",
                "SYC_3_changeM_1","SYC_3_changeM_1a")

out <- Inout <- ReadCTLfile(prodCTLNames[1])
ProductionModelOutput(Inout,prodCTLNames,MSEdir,
                      plotNames=c("Time-Invariant", "Time-Invariant (est Init)",
                                  "Increasing M", "Increasing M (est Init)", 
                                  "Decreasing M", "Decreasing M (est Init)"))

prodCTLNames<-c("SYC_1_noTV_1",
                "SYC_2_allGAMs_1",
                "SYC_3_changeM_1")
run_GeMS(prodCTLNames,runparallel=T,cores=3,silent=T)

out <- Inout <- ReadCTLfile(prodCTLNames[1])
ProductionModelOutput(Inout,prodCTLNames,MSEdir,
                      plotNames=c("Time-Invariant",
                                  "Increasing M",
                                  "Decreasing M"))



#########

CreateFolderNameList <- c("SYC_2_allGAMs_2","SYC_2_allGAMs_2_1","SYC_2_allGAMs_2_2")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=2,CreateFolderNameList,MSEdir)

#########

CreateFolderNameList<-c("SYC_1_noTV_2","SYC_1_noTV_3","SYC_1_noTV_4",
						"SYC_2_allGAMs_2","SYC_2_allGAMs_3","SYC_2_allGAMs_4",
						"SYC_3_changeM_2","SYC_3_changeM_3","SYC_3_changeM_4")

CreateFolderNameList<-c("SYC_1_noTV_1","SYC_2_allGAMs_1","SYC_3_changeM_1",
                        "SYC_1_noTV_1a","SYC_2_allGAMs_1a","SYC_3_changeM_1a")


CTLNames<-CTLNameList<-CreateFolderNameList<-c("SYC_1_noTV_2","SYC_1_noTV_3",
						"SYC_2_allGAMs_2","SYC_2_allGAMs_3",
						"SYC_3_changeM_2","SYC_3_changeM_3")


out <- Inout <- ReadCTLfile(CreateFolderNameList[5])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                plotNames=c("Time-Invariant;\nFixed M", "Time-Invariant;\nEstimated M",
                            "Increasing M;\nFixed M", "Increasing M;\nEstimated M",
                            "Decreasing M;\nFixed M", "Decreasing M;\nEstimated M"),
                Nruns=5,plottiff=F)

#run_GeMS(CreateFolderNameList,GeMSdir,MSEdir,runparallel=T,cores=8,GeMSops=list(silent=T))


######################
# Age-structured plots
######################

CTLNames <- CreateFolderNameList<-c("SYC_1_noTV_2","SYC_1_noTV_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

CTLNames <- CreateFolderNameList<-c("SYC_2_allGAMs_2","SYC_2_allGAMs_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

CTLNames <- CreateFolderNameList<-c("SYC_3_changeM_2","SYC_3_changeM_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

#############

CreateFolderNameList<-c("SYC_1_noTV_5","SYC_2_allGAMs_5","SYC_3_changeM_5")
out <- Inout <- ReadCTLfile(paste0(CreateFolderNameList[1],".csv"))
AgeStructureComp(Inout,RetroPeels=2,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

############

par(mfrow=c(5,1),mar=c(.1,.1,.3,.1),oma=c(1,3,1,1),cex=1.2)
  
inYlim<-c(min(BigMohn,BigBias,ReB35,ReF35,BigOFL),max(BigMohn[BigMohn<10],
                                                      BigBias[BigBias<10],ReB35[ReB35<10],
                                                      ReF35[ReF35<10],BigOFL[BigOFL<10]))
boxplot(BigMohn,col=ScenCols,ylim=inYlim,xaxt='n',las=1)
legend("topleft",c("(a) Retrospective bias"),bty='n')
mtext(side=2,"Mohn's rho",line=3,cex=1.2)
abline(h=0,lty=2)

boxplot(BigBias,col=ScenCols,xaxt='n',ylim=inYlim,las=1)
legend("topleft",c("(b) Spawning biomass"),bty='n')
mtext(side=2,"relative error",line=3,cex=1.2)
abline(h=0,lty=2)
boxplot(ReB35,col=ScenCols,,ylim=inYlim,xaxt='n',las=1)
legend("topleft",expression("(c) B"[35]),bty='n')
mtext(side=2,"relative error",line=3,cex=1.2)
abline(h=0,lty=2)
boxplot(ReF35,col=ScenCols,ylim=inYlim,xaxt='n',las=1)
legend("topleft",expression("(c) F"[35]),bty='n')
mtext(side=2,"relative error",line=3,cex=1.2)
abline(h=0,lty=2)
boxplot(BigOFL,col=ScenCols,ylim=inYlim,las=2,names=CTLNames,xaxt="n")
abline(h=0,lty=2)
legend("topleft",c("(e) OFL"),bty='n')
mtext(side=2,"relative error",line=3,cex=1.2)

CTLs <- ReadCTLfile(file.path(MSEdir,paste0(CreateFolderName,".csv")))

###########

par(mfcol=c(1,length(CTLNames)),mar=c(.1,.1,.1,.1),oma=c(2,6,1,1),cex=1.2)

for(y in seq_along(CTLNames))
{

 use_ylim<-c(0,max(trueCPUE,estCPUE,na.rm=T))
  
  if(y==1)
  {
    PolygonPlots(Truth=trueCPUE[,,y],Estimated=estCPUE[,,y],SimYear=Inout$OM$SimYear,Nsim=Inout$OM$Nsim,ylimIN = use_ylim)
    axis(side=2,las=1)
    axis(side=1,at=c(0,50,100))
    mtext(side=2,"Biomass",line=5,cex=1.1)
    legend("topright",col=c(2,"#0000ff99","#00800077"),lty=c(1,1,2),
           legend=c("Estimates","True BMSY","Estimated BMSY range"),bty='n',cex=.7)
  }
  if(y>1)
   PolygonPlots(Truth=trueCPUE[,,y],Estimated=estCPUE[,,y],SimYear=Inout$OM$SimYear,Nsim=Inout$OM$Nsim,ylimIN = use_ylim)
# mtext(side=3,CTLNames[y],cex=1.1)
abline(h=trueBMSY[y],col="#0000ff99",lty=1)
abline(h=max(estBMSY[,,y],na.rm=T),col="#00800099",lty=2)
abline(h=min(estBMSY[,,y],na.rm=T),col="#00800099",lty=2)
if(y!=1) axis(side=1,at=c(50,100))
use_ylim<-c(0,max(trueTAC,estTAC,na.rm=T))

}

###########

AgeStructureComp<-function(Inout,RetroPeels=6,CTLNames,MSEdir)
{
  TakeRows<-(Inout$OM$SimYear-RetroPeels+1):Inout$OM$SimYear
  GradientSave<-array(dim=c(RetroPeels,Inout$OM$Nsim,length(CTLNames)))
  
  for(x in seq_along(CTLNames))
  {
    Inout<-ReadCTLfile(file.path(MSEdir,paste0(CTLNames[x],".csv")))
    GradientSave[,,x]<-CheckGradient(DrawDir=CTLNames[x],out=Inout,MSEdir)[TakeRows,]
  }
  #ScenCols<-c("grey",as.numeric(seq(2,length(CTLNames),1)))
  ScenCols<-colorspace::rainbow_hcl(length(CTLNames))

  #==reference points
  TakeRows	<-(Inout$OM$InitYear+1):Inout$OM$SimYear
  B35save	<-array(dim=c(length(TakeRows),Inout$OM$Nsim,length(CTLNames)))
  F35save	<-array(dim=c(length(TakeRows),Inout$OM$Nsim,length(CTLNames)))
  OFLsave	<-array(dim=c(length(TakeRows),Inout$OM$Nsim,length(CTLNames)))
  tOFLsave	<-array(dim=c(length(TakeRows),Inout$OM$Nsim,length(CTLNames)))
  tF35save	<-array(dim=c(length(TakeRows),Inout$OM$Nsim,length(CTLNames)))
  tB35save	<-array(dim=c(length(TakeRows),Inout$OM$Nsim,length(CTLNames)))
  
  for(x in seq_along(CTLNames))
  {
    temp<-CheckReferencePoints(DrawDir=CTLNames[x],out=Inout,MSEdir)
    B35save[,,x]	<-temp[[1]][TakeRows,]
    F35save[,,x]	<-temp[[2]][TakeRows,]
    OFLsave[,,x]	<-temp[[3]][TakeRows,]
    tB35save[,,x]	<-temp[[4]][TakeRows,]
    tF35save[,,x]	<-temp[[5]][TakeRows]
    tOFLsave[,,x]	<-temp[[6]][TakeRows,]
  } 
  
  BigB35<-matrix(nrow=Inout$OM$Nsim,ncol=length(CTLNames))
  BigF35<-matrix(nrow=Inout$OM$Nsim,ncol=length(CTLNames))
  BigOFL<-matrix(nrow=Inout$OM$Nsim,ncol=length(CTLNames))
  if(Inout$OM$Nsim>1) {
    for(x in seq_along(CTLNames))
    {
      BigB35[,x]<-apply((B35save[,,x]-tB35save[,,x])/tB35save[,,x],2,median,na.rm=T)
      BigF35[,x]<-apply((F35save[,,x]-tF35save[,,x])/tF35save[,,x],2,median,na.rm=T)
      BigOFL[,x]<-apply((OFLsave[,,x]-tOFLsave[,,x])/tOFLsave[,,x],2,median,na.rm=T)
    }
  }
  
  if(Inout$OM$Nsim==1) {
    for(x in seq_along(CTLNames))
    {
      BigB35[,x]<-median((B35save[,,x]-tB35save[,,x])/tB35save[,,x],na.rm=T)
      BigF35[,x]<-median((F35save[,,x]-tF35save[,,x])/tF35save[,,x],na.rm=T)
      BigOFL[,x]<-median((OFLsave[,,x]-tOFLsave[,,x])/tOFLsave[,,x],na.rm=T)
    }
  }
  
  ReB35<-BigB35
  ReF35<-BigF35
  ReOFL<-BigOFL
  
  #=plotting output
  # GeMSplots(out=Inout,SourceFolder=CTLNames[x])
  MohnsRho<-array(dim=c(RetroPeels-1,Inout$OM$Nsim,length(CTLNames)))
  SSBbias<-array(dim=c(RetroPeels,Inout$OM$Nsim,length(CTLNames)))
  TrueOFL<-array(dim=c(RetroPeels,Inout$OM$Nsim,length(CTLNames)))
  
  for(x in seq_along(CTLNames))
  {
    Inout			<-ReadCTLfile(file.path(MSEdir,paste0(paste(CTLNames[x],sep="_"),".csv")))
    DrawDir		<-CTLNames[x]
    RetroOuts		<-CheckRetro(RetroPeels=RetroPeels,DrawDir,PlotRetro=0,out=Inout,MSEdir)
    MohnsRho[,,x]	<-CalculateMohn(RetroOuts)
    SSBbias[,,x]	<-CalculateBias(RetroOuts)
  }
  
  BigMohn<-matrix(nrow=Inout$OM$Nsim,ncol=length(CTLNames))
  if(Inout$OM$Nsim > 1 & (RetroPeels-1)>1) {
    for(x in seq_along(CTLNames))
    {
      temp<-MohnsRho[,,x]
      BigMohn[,x]<-apply(temp,2,mean) 
    }
  }
  
  if(Inout$OM$Nsim == 1 | (RetroPeels-1) == 1) {
    for(x in seq_along(CTLNames))
    {
      temp<-MohnsRho[,,x]
      BigMohn[,x]<-mean(temp) 
    }
  }

  BigBias<-matrix(nrow=Inout$OM$Nsim,ncol=length(CTLNames))
  if(Inout$OM$Nsim > 1 & (RetroPeels-1) > 1) {
    for(x in seq_along(CTLNames))
    {
      temp<-SSBbias[,,x]
      BigBias[,x]<-apply(temp,2,mean) 
    }
  }
  
  if(Inout$OM$Nsim == 1 | (RetroPeels-1) == 1) {
    for(x in seq_along(CTLNames))
    {
      temp<-SSBbias[,,x]
      BigBias[,x]<-mean(temp) 
    }
  }

#  png(file.path(MSEdir,"plots",paste0("CompareRefPoints",paste(CTLNames,sep="_",collapse=""),".png")),height=7,width=3.5,units='in',res=1200)
  par(mfrow=c(1,4),mar=c(.1,3,.3,.1),oma=c(1,3,1,1))
  
  inYlim<-c(min(BigMohn,BigBias,ReB35,ReF35,BigOFL),max(BigMohn[BigMohn<10],
                                                        BigBias[BigBias<10],ReB35[ReB35<10],
                                                        ReF35[ReF35<10],BigOFL[BigOFL<10]))
  
  boxplot(BigBias,col=ScenCols,xaxt='n',ylim=inYlim,las=1)
  mtext(side=2,"Relative Error",line=3,cex=1.1)
  abline(h=0,lty=2)
  boxplot(ReB35,col=ScenCols,,ylim=inYlim,xaxt='n',las=1)
  abline(h=0,lty=2)
  boxplot(ReF35,col=ScenCols,ylim=inYlim,xaxt='n',las=1)
  abline(h=0,lty=2)
  boxplot(BigOFL,col=ScenCols,ylim=inYlim,xaxt='n')
  abline(h=0,lty=2)
#  dev.off()
#  
  quants<-PullTimevary(MSEdir,inFolders=CTLNames,out=Inout)
#  
#  png(file.path(MSEdir,"plots",paste0("ComparePopulationProcess_",paste(CTLNames,sep="_",collapse=""),".png")),height=5,width=7.5,units='in',res=1200)
#  inmat<-matrix(c(1,1,2,2,3,3),
#                  nrow=2,byrow=T)
#  layout(inmat)
# par(mfrow=c(1,3),mar=c(.1,.1,.1,.1),oma=c(4,5,4,4),cex=0.9)
# 
# #==RECRUITMENT
# input<-quants[[1]]
# tInput<-quants[[2]] 
# yrs2plot <-Inout$OM$start_assessment:(Inout$OM$SimYear-1)
# xlim2use <- c(1,length(yrs2plot))
# plot(-1000000,xlim=xlim2use,ylim=c(0,max(input,na.rm=T)),las=1,xaxt='n')
# for(x in seq_along(CTLNames))
# {
#   #color<-seq(1,length(CTLNames)+1)
#   color<-colorspace::rainbow_hcl(length(CTLNames))
#   incol<-adjustcolor(color,alpha.f=.2)
#   tCol<-rgb(0,0,0,0.2)
#   temp<-input[,yrs2plot,x]
#   for(y in 1:dim(input)[1])
#     lines(input[y,yrs2plot,x],col=incol[x])
# } 
# medians<- apply(input,c(2,3),median,na.rm=T)
# for(x in 1:ncol(medians))
#   lines(medians[yrs2plot,x],col=color[x],cex=2)
# 
# abline(v=(Inout$OM$InitYear-Inout$OM$start_assessment+1),lty=3)
# 
# #==true R
# plotIn<-apply(tInput,2,median)
# plotIn[1]<-NA
# lines(plotIn[yrs2plot],col="black",lwd=1.5,lty=2)
# 
# legend("topleft",bty='n',"(a) Recruitment")
# mtext(side=2,"Numbers",line=4)
# #==FISHING MORTALITY
# input<-quants[[3]]
# NatM<-quants[[7]]
# tInput<-quants[[4]] 
# 
# plot(-100000,xlim=xlim2use,ylim=c(0,max(input,NatM,na.rm=T)),las=1,xaxt='n',yaxt='n')
# axis(side=3)
# mtext(side=3,line=2,"Time")
# for(x in seq_along(CTLNames))
# {
#   #color<-seq(1,length(CTLNames)+1)
#   color<-colorspace::rainbow_hcl(length(CTLNames))
#   incol<-adjustcolor(color,alpha.f=.2)
#   tCol<-rgb(0,0,0,0.2)
#   temp<-input[,yrs2plot,x]
#   for(y in 1:dim(input)[1])
#     lines(input[y,yrs2plot,x],col=incol[x])
# } 
# medians<- apply(input,c(2,3),median,na.rm=T)
# for(x in 1:ncol(medians))
#   lines(medians[yrs2plot,x],col=color[x],cex=2)
# 
# abline(v=Inout$OM$InitYear-Inout$OM$start_assessment+1,lty=3)
# 
# #==true F
# lines(apply(tInput,2,median)[yrs2plot],col="black",lwd=1.5,lty=2)
# legend("topleft",bty='n',"(b) Fishing mortality")
# 
# #==NATURAL MORTALITY
# input<-quants[[7]]
# tInput<-quants[[8]] 
# 
# plot(-1000,xlim=xlim2use,ylim=c(0,max(input,NatM,na.rm=T)),las=1,xaxt='n',yaxt='n')
# axis(side=4,las=1)
# for(x in seq_along(CTLNames))
# {
#   #color<-seq(1,length(CTLNames)+1)
#   color<-colorspace::rainbow_hcl(length(CTLNames))
#   incol<-adjustcolor(color,alpha.f=.2)
#   tCol<-rgb(0,0,0,0.2)
#   temp<-input[,,x]
#   for(y in 1:dim(input)[1])
#     lines(input[y,yrs2plot,x],col=incol[x])
# } 
# medians<- apply(input,c(2,3),median,na.rm=T)
# for(x in 1:ncol(medians))
#   lines(medians[yrs2plot,x],col=color[x],cex=2)
# 
# #==true M
# dim(tInput)
# lines(tInput[yrs2plot],col="black",lwd=1.5,lty=2)
# 
# abline(v=Inout$OM$InitYear-Inout$OM$start_assessment+1,lty=3)
# 
# legend("topleft",bty='n',"(c) Natural mortality")
# mtext(side=4,"rate (/year)",line=2)
# 
# legend("bottomright",bty='n',col=c(1,color,"black"),lty=c(3,rep(1,length(ScenCols)),2),
#        legend=c("Projection begins","Fixed M","Est M","Est TV M","Truth"),lwd=2)
#  
  
  
  #================================
  # Plot the comparisons of fits like the production models
  #====================================
  
  
#  png(file.path(MSEdir,"plots",paste0("AgeStructuredFits_",paste(CTLNames,sep="_",collapse=""),".png")),height=5,width=7.5,units='in',res=1200)
#  par(mfcol=c(2,length(CTLNames)),mar=c(.1,.1,.1,.1),oma=c(4,6,1,4))
#  
#  if(Inout$OM$Nsim>1) {
#    for(y in seq_along(CTLNames))
#    {
#    boxplot(quants[[14]][,,y],type="l",ylim=c(0,max(c(quants[[14]],quants[[15]][,,]),na.rm=T)),
#            las=1,xaxt='n',ylab='',yaxt='n')
#    for(x in 1:nrow(  quants[[14]]))
#      lines(quants[[15]][x,,y],col='#ff000033')
#    if(y==1)
#    {
#      axis(side=2,las=1)
#      mtext(side=2,"Biomass",line=5,cex=.9)
#    }
#    legend("topright",bty='n',,legend=CTLNames[y])
#   # abline(h=trueBMSY,col="#0000ff99",lty=1)
#  #  abline(h=max(estBMSY,na.rm=T),col="#00800099",lty=2)
#   # abline(h=min(estBMSY,na.rm=T),col="#00800099",lty=2)
#    
#    #legend("topright",col=c(1,2,"#0000ff99","#00800077"),pch=c(15,NA,NA,NA),lty=c(NA,1,1,2),
#    #       legend=c("Observations","Estimates","True BMSY","Estimated BMSY range"),bty='n')
#    
#    boxplot(quants[[12]][,,y],type="l",ylim=c(0,max(quants[[12]],na.rm=T)),
#            las=1,xaxt='n',ylab='',yaxt='n')
#    for(x in 1:nrow(quants[[11]]))
#      lines(quants[[11]][x,,y],col='#ff000033')
#    if(y==1)
#    {
#      axis(side=2,las=1)
#      mtext(side=2,"Total allowable catch",line=4.5,cex=.9)
#    }
#    axis(side=1)  
#  }
#  }
#  
#    if(Inout$OM$Nsim==1) {
#    for(y in seq_along(CTLNames))
#    {
#    plot(quants[[14]][,,y],type="l",ylim=c(0,max(quants[[14]],na.rm=T)),
#            las=1,xaxt='n',ylab='',yaxt='n')
#    for(x in 1:nrow(  quants[[14]]))
#      lines(quants[[15]][x,,y],col='#ff000033')
#    if(y==1)
#    {
#      axis(side=2,las=1)
#      mtext(side=2,"Biomass",line=5,cex=.9)
#    }
#    legend("topright",bty='n',,legend=CTLNames[y])
#   # abline(h=trueBMSY,col="#0000ff99",lty=1)
#  #  abline(h=max(estBMSY,na.rm=T),col="#00800099",lty=2)
#   # abline(h=min(estBMSY,na.rm=T),col="#00800099",lty=2)
#    
#    #legend("topright",col=c(1,2,"#0000ff99","#00800077"),pch=c(15,NA,NA,NA),lty=c(NA,1,1,2),
#    #       legend=c("Observations","Estimates","True BMSY","Estimated BMSY range"),bty='n')
#    
#    plot(quants[[12]][,,y],type="l",ylim=c(0,max(quants[[12]],na.rm=T)),
#            las=1,xaxt='n',ylab='',yaxt='n')
#    for(x in 1:nrow(quants[[11]]))
#      lines(quants[[11]][x,,y],col='#ff000033')
#    if(y==1)
#    {
#      axis(side=2,las=1)
#      mtext(side=2,"Total allowable catch",line=4.5,cex=.9)
#    }
#    axis(side=1)  
#  }
#  }
#  
#  legend('topleft',col=c(1,2),pch=c(15,NA),lty=c(NA,1),legend=c("True","Estimated"),bty='n')
#  dev.off()
  
  
  #==================================
  # relative error in OFL over time
  # NEED TO GET THIS ONCE FIGURED OUT AT SOME POINT
  #====================================
#  OFLrel<-0
#  if(OFLrel>0)
#  {
#  InitYear<-Inout$OM$InitYear
#  SimYear<-Inout$OM$SimYear
#  
#  REofl<-rep(list(list()))
#  for(x in seq_along(SaveTrueOFL))
#    REofl[[x]]<-(SaveTrueOFL[[x]][,(InitYear+1):(SimYear-1),]-SaveTrueOFL[[x]][,(InitYear+1):(SimYear-1),])/SaveTrueOFL[[x]][,(InitYear+1):(SimYear-1),]
#  
#  emNames<-c("Base","M vary","Growth vary","Sel vary")
#  omNames<-c("Base","M vary","Sel vary","Growth vary")
#  hexDec<-c("#ff000055","#00ff0055","#0000ff55")
#  par(mfrow=c(4,1),mar=c(.1,.1,.1,.1),oma=c(4,4,1,1))
#  for(x in seq_along(REofl))
#  {
#    #boxplot(REofl[[x]][,,1],col="darkgrey",ylim=c(min(REofl[[x]]),max(REofl[[x]])),xaxt='n',las=1)
#    
#    plot(NA,ylim=c(min(unlist(REofl)),max(unlist(REofl))),xlim=c(1,9),xaxt='n',las=1)
#    plotQuant<-REofl[[x]][,,1]
#    sortQuant<-apply(plotQuant,2,sort)
#    #polygon(x=c(seq(1,9),seq(9,1)),y=c(sortQuant[3,],rev(sortQuant[18,])),col="#cfcfcf99",border=F)
#    #lines(sortQuant[3,],col='darkgrey',lty=2)
#    #lines(sortQuant[18,],col='darkgrey',lty=2)
#    lines(apply(sortQuant,2,median),col='darkgrey',lwd=2)
#    for(y in 2:4)
#    {
#      plotQuant<-REofl[[x]][,,y]
#      sortQuant<-apply(plotQuant,2,sort)
#      # lines(sortQuant[3,],col=y,lty=2)
#      #lines(sortQuant[18,],col=y,lty=2)
#      lines(apply(sortQuant,2,median),col=y,lwd=2)
#      
#      #polygon(x=c(seq(1,9),seq(9,1)),y=c(sortQuant[3,],rev(sortQuant[18,])),col=hexDec[y-1],border=F)
#      #lines(apply(sortQuant,2,median),col=hexDec[y-1])
#    } 
#    abline(h=0,lty=2)
#    legend('topright',bty='n',omNames[x])
#    if(x==1)
#      legend('topleft',emNames,col=c('darkgrey',2,3,4),pch=15,bty='n')
#  }
#  axis(side=1)
#  mtext(outer=T,side=2,"Relative error in TAC",line=2.5)
#  mtext(outer=T,side=1,"Projection year",line=2)
#  
#  dev.new()
#  boxplot(SaveTrueOFL[[1]][,(InitYear+1):(SimYear-1),],col="#00ff0033")
#  boxplot(SaveEstOFL[[1]][,(InitYear+1):(SimYear-1),1],add=T,col="#ff000033")
#  
#  dev.new()
#  par(mfrow=c(4,2),mar=c(.1,.1,.1,.1),oma=c(4,5,1,4))
#  for(x in seq_along(REofl))
#  {
#    plot(NA,ylim=c(min(unlist(SaveEstOFL),na.rm=T),max(unlist(SaveEstOFL),na.rm=T)),xlim=c(1,9),xaxt='n',las=1)
#    plotQuant<-SaveEstOFL[[x]][,51:60,1]
#    sortQuant<-apply(plotQuant,2,sort)
#    lines(apply(sortQuant,2,median),col='darkgrey',lwd=2)
#    for(y in 2:4)
#    {
#      plotQuant<-SaveEstOFL[[x]][,51:60,y]
#      sortQuant<-apply(plotQuant,2,sort)
#      lines(apply(sortQuant,2,median),col=y,lwd=2)
#    } 
#    abline(h=0,lty=2)
#    legend('topright',bty='n',paste("(",letters[2*x-1],") ",omNames[x],sep=""))
#    
#    if(x==1)
#      legend('topleft',emNames,col=c('darkgrey',2,3,4),pch=15,bty='n')
#    if(x==4)
#      axis(side=1)
#    #==plot the status true
#    temp<-lapply(SaveSpBio,'[',,51:60,)
#    tempSB<-t(as.matrix(temp[[x]][,,1]))
#    tempB35<-tB35save[1,1,1,x]
#    bigStat<-tempSB/tempB35
#    plot(NA,ylim=c(.75,1.75),xlim=c(1,9),xaxt='n',las=1,yaxt='n',las=1)
#    axis(side=4,las=1)
#    plotQuant<-bigStat
#    lines(apply(plotQuant,1,median),col='darkgrey',lwd=2)
#    for(y in 2:4)
#    {
#      tempSB<-t(as.matrix(temp[[x]][,,y]))
#      tempB35<-tB35save[1,1,y,x]
#      bigStat<-tempSB/tempB35
#      plotQuant<-bigStat
#      lines(apply(plotQuant,1,median),col=y,lwd=2)
#    } 
#    legend('topright',bty='n',paste("(",letters[2*x],") ",sep=""))
#    abline(h=1,lty=2)
#    if(x==4)
#      axis(side=1)
#  }
#  mtext(side=4,outer=T,text=expression("B/B"[35]),line=2.5)
#  mtext(side=2,outer=T,"Catch",line=3.25)
#  mtext(side=1,outer=T,"Projection year",line=2.5)
#  }
}

##########

PolygonPlots<-function(Truth=NA,Estimated=NA,Observed=NA,AddLegend=F,bottom=F,quantity=NA,SimYear=NA,Nsim=NA,ylimIN=NA)
{
 EstShape<-apply(Estimated[,1:(ncol(Estimated))],2,quantile,probs=c(.05,.25,.75,.95),na.rm=T)
 TrueShape<-apply(Truth[,1:(ncol(Truth))],2,quantile,probs=c(.05,.25,.75,.95),na.rm=T)

 DataColor<-"#6699ff11"
 EstimateColor25<-"darkgrey"
 EstimateColor5<-"lightgrey"
 TruthColor<-"#FF000033"

 if(!is.na(ylimIN[1]))
   use_ylim<-ylimIN
 if(is.na(ylimIN[1]))
   use_ylim<-c(0,max(EstShape,TrueShape,Observed,na.rm=t))
 
 if(bottom==F)
  plot(-100000000000,ylim=use_ylim,xlim=c(1,SimYear),las=1,ylab="",xlab="Year",xaxt='n',yaxt='n')
 if(bottom==T)
  plot(-100000000000,ylim=use_ylim,xlim=c(1,SimYear),las=1,ylab="",xlab="Year")
 
 if(!is.na(quantity))
  legend("bottomleft",bty='n',legend=quantity)
 
  if(length(Observed)>1)
  {
  if(AddLegend==T)
   legend("topright",bty='n',pch=c(15,16,NA),lty=c(NA,NA,1),col=c(EstimateColor5,DataColor,TruthColor),legend=c("Estimates","Observed Data","Truth"))
  for(z in 1:Nsim)
   points(Observed[z,]~seq(1,(SimYear-1)),col=DataColor,pch=16)
  }
  polygon(x=c(seq(1,(SimYear-1)),rev(seq(1,(SimYear-1)))),y=c(TrueShape[1,1:SimYear-1],rev(TrueShape[4,1:SimYear-1])),col=EstimateColor5,border=F)
  polygon(x=c(seq(1,(SimYear-1)),rev(seq(1,(SimYear-1)))),y=c(TrueShape[2,1:SimYear-1],rev(TrueShape[3,1:SimYear-1])),col=EstimateColor25,border=F)
  if(AddLegend==T&length(Observed)<1)
  legend("topright",bty='n',pch=c(15,NA),lty=c(NA,1),col=c(EstimateColor5,TruthColor),legend=c("Truth","Estimates"))
 
  for(z in 1:Nsim) {
   lines(Truth[z,]~seq(1,SimYear),col="black",lty=1)
   lines(Estimated[z,]~seq(1,(SimYear)),col=TruthColor,lty=1)
 }
}

par(mfcol=c(2,length(CTLNames)),mar=c(.1,.1,.1,.1),oma=c(4,6,1,4),cex=1.2)

for(y in seq_along(CTLNames))
{
	# boxplot(trueCPUE[,,y],type="l",ylim=c(0,max(trueCPUE,na.rm=T)),
	#  las=1,xaxt='n',ylab='',yaxt='n')
	#   input<-trueCPUE[,,y]
	  # for(x in 1:nrow(estCPUE))
	  #   lines(estCPUE[x,,y],col='#ff000033')
	 use_ylim<-c(0,max(trueCPUE,estCPUE,na.rm=T))
	  
	  if(y==1)
	  {
	    PolygonPlots(Truth=trueCPUE[,,y],Estimated=estCPUE[,,y],SimYear=Inout$OM$SimYear,Nsim=Inout$OM$Nsim,ylimIN = use_ylim)
	    axis(side=2,las=1)
	    mtext(side=2,"Biomass",line=5,cex=1.2)
	    legend("topright",col=c(1,2,"#0000ff99","#00800077"),lty=c(1,1,1,2),
	           legend=c("Observations","Estimates","True BMSY","Estimated BMSY range"),bty='n',cex=.9)
	  }
	  if(y>1)
	   PolygonPlots(Truth=trueCPUE[,,y],Estimated=estCPUE[,,y],SimYear=Inout$OM$SimYear,Nsim=Inout$OM$Nsim,ylimIN = use_ylim)
	 mtext(side=3,CTLNames[y],cex=.7)
	abline(h=trueBMSY[y],col="#0000ff99",lty=1)
	abline(h=max(estBMSY[,,y],na.rm=T),col="#00800099",lty=2)
	abline(h=min(estBMSY[,,y],na.rm=T),col="#00800099",lty=2)
	
	use_ylim<-c(0,max(trueTAC,estTAC,na.rm=T))
	
	if(y==1)
	{
	  PolygonPlots(Truth=trueTAC[,,y],Estimated=estTAC[,,y],SimYear=Inout$OM$SimYear,Nsim=Inout$OM$Nsim,ylimIN = use_ylim,bottom=T)
	  mtext(side=2,"Total allowable catch",line=4.5,cex=1.2)
	  legend('topleft',col=c(1,2),pch=c(15,NA),lty=c(NA,1),legend=c("True","Estimated"),bty='n',cex=.9)
	}
	if(y>1)
	 {
	  PolygonPlots(Truth=trueTAC[,,y],Estimated=estTAC[,,y],SimYear=Inout$OM$SimYear,Nsim=Inout$OM$Nsim,ylimIN = use_ylim)
	  axis(side=1)
	 }
}

##############
par(mfrow=c(4,2),mar=c(3,3,0,0),oma=c(1,1,1,1))
PlotLifeHistory<-function(LenAtAgeN,LenAtAgeS,matureN,matureS,vulnN,vulnS,survSelN,survSelS,WeightAtAgeN,
	WeightAtAgeS,MovementN,MovementS,NatMs,NatMn,VirBioN,VirBioS,RzeroN,RecErrN,steepnessN,steepnessS,
	RzeroS,RecErrS,sigmaRn,sigmaRs,HistoricalFn,HistoricalFs,SimYear,MaxAge)
  {
 plot(LenAtAgeN[1,],type="l",las=1,ylim=c(0,max(LenAtAgeN)))
	# for(x in 2:SimYear)
	#  lines(LenAtAgeN[x,])
	 #mtext(side=3,"Length at Age (N)",cex=.7)
	
	 plot(matureN[1,],las=1,type="l",ylim=c(0,max(matureN)))
	# for(x in 2:SimYear)
	#  lines(matureN[x,])
	 #mtext(side=3,"Maturity at age (N)",cex=.7)
	
	 plot(vulnN[1,],las=1,type="l",ylim=c(0,max(vulnN)))
	# for(x in 2:SimYear)
	#  lines(vulnN[x,])
	 #mtext(side=3,"Fishery selectivity at age(N)",cex=.7)
	
	 plot(survSelN[1,],las=1,type="l",ylim=c(0,max(survSelN)))
	# for(x in 2:SimYear)
	#  lines(survSelN[x,])
	 #mtext(side=3,"Index selectivity at age(N)",cex=.7)
	
	
	 plot(WeightAtAgeN[1,],las=1,type="l",ylim=c(0,max(WeightAtAgeN)))
	# for(x in 2:SimYear)
	#  lines(WeightAtAgeN[x,])
	 #mtext(side=3,"Weight at age (N)",cex=.7)
	
	 plot(NatMs,las=1,type="l",ylim=c(0,max(NatMs)))
	# lines(NatMn,lty=2)
	# legend("bottomleft",bty='n',lty=c(1,2),legend=c("South","North"))
	  #mtext(side=3,"Natural mortality by year",cex=.7)
	
	ssb<-seq(1,VirBioN,VirBioN/100)
	record<-ssb
	  for(x in seq_along(record))
	   {
	     record[x]<-Recruitment(EggsIN=ssb[x],steepnessIN=steepnessN[1],RzeroIN=RzeroN[1],RecErrIN=RecErrN[1],recType="BH",NatMin=NatMn[1],
								vulnIN=vulnN[1,],matureIN=matureN[1,],weightIN=WeightAtAgeN[1,],LenAtAgeIN=LenAtAgeN[1,],MaxAge=MaxAge,sigmaRin=sigmaRn[1])
	   }
	plot(record/10000~ssb,type='l',las=1)
	#mtext(side=3,"SSB vs Rec (N)",cex=.7)
	plot(HistoricalFn,type='l',las=1)
	#mtext(side=3,"Historical F",cex=.7)
}



