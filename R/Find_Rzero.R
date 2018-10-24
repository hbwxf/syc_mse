cd ~/Box\ Sync/China_LQ/syc_mse
rt

# 2014 is (OM$InitYear-1)
library(GeMS)

dir.MSE <- file.path(getwd(),"Data")

MasterOMname <- "SYC_2_allGAMs_1"
MasterOMname <- "SYC_3_changeM_1"
MasterOMname <- "SYC_1_noTV_1"
#OMname <- "FindRzero_miniGeMS"

ctlfilename <- MasterOMname
CTLfile <- ReadCTLfile(ctlfilename)
#CTLfile$OM$InitYear <- CTLfile$OM$InitYear-1
#CTLfile$OM$SimYear <- CTLfile$OM$InitYear+1
CTLfile$OM$Nsim <- 1

#CTLfile$OM$LinfN <- c(rep(lhdat$Linf[1],CTLfile$OM$InitYear-(max(lhdat$X)-min(lhdat$X)+1)),lhdat$Linf)
#CTLfile$OM$VonKn <- c(rep(lhdat$k[1],CTLfile$OM$InitYear-(max(lhdat$X)-min(lhdat$X)+1)),lhdat$k)
#CTLfile$OM$NatMn <- c(rep(lhdat$M[1],CTLfile$OM$InitYear-(max(lhdat$X)-min(lhdat$X)+1)),lhdat$M)
#CTLfile$OM$t0n <- c(rep(lhdat$t0[1],CTLfile$OM$InitYear-(max(lhdat$X)-min(lhdat$X)+1)),lhdat$t0)
#CTLfile$OM$alphaN <- c(rep(lhdat$a[1],CTLfile$OM$InitYear-(max(lhdat$X)-min(lhdat$X)+1)),lhdat$a)
#CTLfile$OM$betaN <- c(rep(lhdat$b[1],CTLfile$OM$InitYear-(max(lhdat$X)-min(lhdat$X)+1)),lhdat$b)

newOM <- CTLfile

catchdat <- read.csv(file.path(dir.MSE,"SYC_catches.csv"),header=T)
tCatch <- catchdat$Larimichthys.polyactis
FAOtot <- c(63800,81500,82100,79900,80300,82100,110300,124300,105200,118000,131300,114000,100300,107700,111700,134500,133500,114500,114600,89600,104900,85400,62000,77200,76030,54855,58150,35914,55188,80124,90781,76244,52097,41701,31119,41656,33536,44648,46460,38807,52895,85553,103667,110544,143504,181553,280036,156195,185971,225513,262090,218139,233274,245382,284936,293674,342364,373249,388018,407081,438837,458801,437613,399600,370520,411735)
library(tibble)
library(dplyr)
library(ggplot2)

catchtib <- tibble(Catch=FAOtot,Year=1950:2015,Source="Global") %>%
				add_row(Catch=tCatch,Year=catchdat$Year,Source="ECS")



ggplot(data = catchdat) + 
  geom_col(mapping = aes(x = Year, y = Larimichthys.polyactis)) +
  scale_y_continuous(name = "Catch (t)") +
	theme_classic()
#  scale_fill_continuous(name = "Catch (mt)")

ggplot(data = catchtib) + 
  geom_col(mapping = aes(x = Year, y = Catch,fill=Source),position="identity") +
  theme(text = element_text(size=20)) +
  scale_y_continuous(name = "Catch (t)") +
  theme_classic()

tCatch <- tCatch[-length(tCatch)]
Nyears <- length(tCatch)

miniGeMS <- function(out) {
	#set.seed(12345)
	Nsim			<-out$OM$Nsim			# number of simulations to do in the MSE
	SimYear		<-out$OM$SimYear			# total number of years in simulation
	InitYear		<-out$OM$InitYear			# year in which MSE starts (i.e. the number of years of data available for initial assessment)
	AssessmentType	<-out$OM$AssessmentType		# 0 = projection only, 1 = production, 2= agestructured
	FisheryIndepenDat <-out$OM$FisheryIndepenDat
	LifeHistoryPlots	<-out$OM$LifeHistoryPlots
	EstimationPlots	<-out$OM$EstimationPlots	# this plots the diagnostics for each run of the estimation model (will be a lot of plots!)
	AssessmentData	<-out$OM$AssessmentData		# this plots the diagnostics for each run of the estimation model (will be a lot of plots!)
	PlotYieldCurve	<-out$OM$PlotYieldCurve
	TwoPop		<-out$OM$TwoPop

	#==sampling uncertainty 
	CatchCVn	<-CleanInput(out$OM$CatchCVn,SimYear)
	CatchCVs	<-CatchCVn
	if(TwoPop>0) 
	 CatchCVs	<-CleanInput(out$OM$CatchCVs,SimYear)
	
	IndexCVn	<-CleanInput(out$OM$IndexCVn,SimYear)
	IndexCVs	<-IndexCVn
	if(TwoPop>0) 
	 IndexCVs	<-CleanInput(out$OM$IndexCVs,SimYear)
	
	LenSampleN	<-CleanInput(out$OM$LenSampleN,SimYear)
	LenSampleS	<-LenSampleN
	if(TwoPop>0) 
	 LenSampleS	<-CleanInput(out$OM$LenSampleS,SimYear)
	
	GrowthSDn	<-CleanInput(out$OM$GrowthSDn,SimYear)
	GrowthSDs	<-GrowthSDn
	if(TwoPop>0) 
	 GrowthSDs	<-CleanInput(out$OM$GrowthSDs,SimYear)
	
	#==========================================================
	#=================population dynamics processes============
	#==============================================+===========
	MaxAge<-out$OM$MaxAge
	Ages<-seq(1,MaxAge)
	
	#==natural mortality================
	 NatMn	<-CleanInput(out$OM$NatMn,SimYear)
	 NatMs	<-NatMn
	if(TwoPop>0) 
	 NatMs	<-CleanInput(out$OM$NatMn,SimYear)
	
	#==Length at age====================
	VonKn		<-CleanInput(out$OM$VonKn,SimYear)
	LinfN		<-CleanInput(out$OM$LinfN,SimYear)
	t0n		<-CleanInput(out$OM$t0n,SimYear)
	LenAtAgeN	<-matrix(nrow=SimYear,ncol=MaxAge)
	
	LenAtAgeN[1,]<-LinfN[1]*(1-exp(-VonKn[1]*(Ages-t0n[1])))
	LenAtAgeN[,1]<-LenAtAgeN[1,1]
	for(i in 2:SimYear)
	 for(j in 2:MaxAge)
	  LenAtAgeN[i,j]<-LenAtAgeN[i-1,j-1]+(LinfN[i]-LenAtAgeN[i-1,j-1])*(1-exp(-VonKn[i]))
	
	LenAtAgeS<-LenAtAgeN
	
	
	if(TwoPop>0)
	{ 
	VonKs		<-CleanInput(out$OM$VonKs,SimYear)
	LinfS		<-CleanInput(out$OM$LinfS,SimYear)
	t0s		<-CleanInput(out$OM$t0s,SimYear)
	LenAtAgeS	<-matrix(nrow=SimYear,ncol=MaxAge)
	
	LenAtAgeS[1,]<-LinfS[1]*(1-exp(-VonKs[1]*(Ages-t0s[1])))
	LenAtAgeS[,1]<-LenAtAgeS[1,1]
	for(i in 2:SimYear)
	 for(j in 2:MaxAge)
	  LenAtAgeS[i,j]<-LenAtAgeS[i-1,j-1]+(LinfS[i]-LenAtAgeS[i-1,j-1])*(1-exp(-VonKs[i]))
	}
	
	#==specify the number of length bins
	 BinWidth		<-(max(LenAtAgeS,LenAtAgeN)*1.05)/(out$OM$LengthBinN)
	 LengthBins		<-seq(0,max(LenAtAgeS,LenAtAgeN)*1.05,BinWidth)
	 LengthBinsMid	<-LengthBins[1:(length(LengthBins)-1)] + mean(LengthBins[1:2])
	 LengthBinN		<-length(LengthBinsMid)
	
	#==maturity at age==========================
	mat50n	<-CleanInput(out$OM$mat50n,SimYear)
	mat95n	<-CleanInput(out$OM$mat95n,SimYear)
	matureN	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 matureN[i,]<-1/(1+exp(-1*log(19)*(Ages-mat50n[i])/(mat95n[i]-mat50n[i])))
	
	matureS	<-matureN
	
	if(TwoPop>0) 
	{
	mat50s	<-CleanInput(out$OM$mat50s,SimYear)
	mat95s	<-CleanInput(out$OM$mat95s,SimYear)
	matureS	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 matureS[i,]<-1/(1+exp(-1*log(19)*(Ages-mat50s[i])/(mat95s[i]-mat50s[i])))
	}
	
	#=====================================
	#==fishery selectivitiy============== 
	#=====================================
	sel50n	<-CleanInput(out$OM$sel50n,SimYear)
	sel95n	<-CleanInput(out$OM$sel95n,SimYear)
	vulnN		<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 vulnN[i,]	<-1/(1+exp(-1*log(19)*(LenAtAgeN[i,]-sel50n[i])/(sel95n[i]-sel50n[i])))
	
	vulnN[vulnN<0.01]<-0
	vulnS		<-vulnN
	if(TwoPop>0) 
	{
	sel50s	<-CleanInput(out$OM$sel50s,SimYear)
	sel95s	<-CleanInput(out$OM$sel95s,SimYear)
	vulnS		<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 vulnS[i,]	<-1/(1+exp(-1*log(19)*(LenAtAgeN[i,]-sel50s[i])/(sel95s[i]-sel50s[i])))
	
	vulnS[vulnS<0.01]<-0
	}
	#==index selectivity=====================
	surv50n	<-CleanInput(out$OM$surv50n,SimYear)
	surv95n	<-CleanInput(out$OM$surv95n,SimYear)
	survSelN		<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 survSelN[i,]	<-1/(1+exp(-1*log(19)*(LenAtAgeN[i,]-surv50n[i])/(surv95n[i]-surv50n[i])))
	
	survSelS	<-survSelN
	surv50s	<-surv50n
	surv95s	<-surv95n
	
	if(TwoPop>0) 
	{
	surv50s	<-CleanInput(out$OM$surv50s,SimYear)
	surv95s	<-CleanInput(out$OM$surv95s,SimYear)
	survSelS	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 survSelS[i,]	<-1/(1+exp(-1*log(19)*(LenAtAgeN[i,]-surv50s[i])/(surv95s[i]-surv50s[i])))
	}
	
	#==weight at age==========================
	alphaN		<-CleanInput(out$OM$alphaN,SimYear)
	betaN			<-CleanInput(out$OM$betaN,SimYear)
	WeightAtAgeN	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 WeightAtAgeN[i,]	<-alphaN[i]*LenAtAgeN[i,]^betaN[i]
	
	WeightAtAgeS	<-WeightAtAgeN
	if(TwoPop>0) 
	{
	alphaS		<-CleanInput(out$OM$alphaS,SimYear)
	betaS			<-CleanInput(out$OM$betaS,SimYear)
	WeightAtAgeS	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 WeightAtAgeS[i,]	<-alphaS[i]*LenAtAgeS[i,]^betaS[i]
	}
	
	#==movement from box to box==
	MaxMovingN	<-CleanInput(out$OM$MaxMovingN,SimYear)
	Move50n	<-CleanInput(out$OM$Move50n,SimYear)
	Move95n	<-CleanInput(out$OM$Move95n,SimYear)
	MovementN	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 MovementN[i,]	<-MaxMovingN[i]/(1+exp(-1*log(19)*(Ages-Move50n[i])/(Move95n[i]-Move50n[i])))
	
	MovementS	<-MovementN
	if(TwoPop>0) 
	{
	MaxMovingS	<-CleanInput(out$OM$MaxMovingS,SimYear)
	Move50s	<-CleanInput(out$OM$Move50s,SimYear)
	Move95s	<-CleanInput(out$OM$Move95s,SimYear)
	MovementS	<-matrix(nrow=SimYear,ncol=MaxAge)
	for(i in 1:SimYear)
	 MovementS[i,]	<-MaxMovingS[i]/(1+exp(-1*log(19)*(Ages-Move50s[i])/(Move95s[i]-Move50s[i])))
	}
	
	if(sum(MovementS)>0 | sum(MovementN)>0) 
	 {print("Movement is occuring between populations")}
	
	#==recruitment parameters==
	steepnessN	<-CleanInput(out$OM$steepnessN,SimYear)
	sigmaRn	<-CleanInput(out$OM$sigmaRn,SimYear)
	RzeroN	<-CleanInput(out$OM$RzeroN,SimYear)
	
	steepnessS	<-steepnessN
	sigmaRs	<-sigmaRn
	RzeroS	<-RzeroN
	
	if(TwoPop>0)
	{
	steepnessS	<-CleanInput(out$OM$steepnessS,SimYear)
	sigmaRs	<-CleanInput(out$OM$sigmaRs,SimYear)
	RzeroS	<-CleanInput(out$OM$RzeroS,SimYear)
	}
	
	RecErrN	<-matrix(rnorm(SimYear*Nsim,0,sigmaRn),ncol=SimYear,byrow=T)
	RecErrS	<-matrix(rnorm(SimYear*Nsim,0,sigmaRs),ncol=SimYear,byrow=T)
	
	#==historic fishing mortality
	HistoricalFn	<-CleanInput(out$OM$HistoricalFn,SimYear)[1:InitYear]
	PastFsdN		<-out$OM$PastFsdN
	HarvestControlN	<-out$OM$HarvestControlN
	ConstantCatchN	<-out$OM$ConstantCatchN
	ConstantFn		<-out$OM$ConstantFn
	HCalphaN		<-out$OM$HCalphaN
	HCbetaN		<-out$OM$HCbetaN
	
	HistoricalFs	<-HistoricalFn
	PastFsdS		<-PastFsdN
	HarvestControlS	<-HarvestControlN
	ConstantCatchS	<-ConstantCatchN
	ConstantFs		<-ConstantFn
	HCalphaS		<-HCalphaN
	HCbetaS		<-HCbetaN
	
	if(TwoPop>0)
	{
	HistoricalFs	<-CleanInput(out$OM$HistoricalFs,SimYear)[1:InitYear]
	PastFsdS		<-out$OM$PastFsdS
	HarvestControlS	<-out$OM$HarvestControlS
	ConstantCatchS	<-out$OM$ConstantCatchS
	ConstantFs		<-out$OM$ConstantFs
	HCalphaS		<-out$OM$HCalphaS
	HCbetaS		<-out$OM$HCbetaS
	}
	
	start_assessment<-out$OM$start_assessment
	SmallNum		<-out$OM$SmalNum
	InitSmooth		<-out$OM$InitSmooth	
	FmortPen		<-out$OM$FmortPen
	RecruitPen		<-out$OM$RecruitPen	
	Mpenalty		<-out$OM$Mpenalty	
	Growthpenalty	<-out$OM$Growthpenalty	
	SelPenalty		<-out$OM$SelPenalty
	
	EstM			<-out$OM$EstM
	TimeVaryM		<-out$OM$TimeVaryM	
	EstGrowthK		<-out$OM$EstGrowthK
	TimeVaryGrowthK	<-out$OM$TimeVaryGrowthK
	EstLinf		<-out$OM$EstLinf
	TimeVaryLinf	<-out$OM$TimeVaryLinf
	TimeVarySel50	<-out$OM$TimeVarySel50
	TimeVarySel95	<-out$OM$TimeVarySel95
	ProjectTimeVary	<-out$OM$ProjectTimeVary
	InitValSig		<-out$OM$InitValSig
	
	InitBzeroMod	<-out$OM$InitBzeroMod
	InitGrowthRate	<-out$OM$InitGrowthRate
	estInit       <-out$OM$estInit
	InitBioProd   <-out$OM$InitBioProd
	
	#===================================================
	#==Virgin numbers at age, biomass, recruitment
	#===================================================
	VirInitN		<-initialN(Rzero=RzeroN[1],NatM=NatMn[1],inAge=MaxAge)
	VirInitS		<-initialN(Rzero=RzeroS[1],NatM=NatMs[1],inAge=MaxAge)
	
	VirBioN		<-sum(VirInitN*matureN[1,]*WeightAtAgeN[1,])
	VirBioS		<-sum(VirInitS*matureS[1,]*WeightAtAgeS[1,])
	
	ExploitBioN		<-sum(VirInitN*vulnN[1,]*WeightAtAgeN[1,])
	ExploitBioS		<-sum(VirInitS*vulnS[1,]*WeightAtAgeS[1,])
	
	tempNn		<-array(dim=c(InitYear,MaxAge,Nsim))
	tempNn[1,,]		<-VirInitN
	tempNs		<-array(dim=c(InitYear,MaxAge,Nsim))
	tempNs[1,,]		<-VirInitS
	tempCatchN		<-matrix(ncol=InitYear,nrow=Nsim)
	tempCatchS		<-matrix(ncol=InitYear,nrow=Nsim)
	tempRecN		<-matrix(ncol=InitYear,nrow=Nsim)
	tempRecS		<-matrix(ncol=InitYear,nrow=Nsim)
	tempCatchAtAgeN	<-array(dim=c(InitYear,MaxAge,Nsim))
	tempCatchAtAgeS	<-array(dim=c(InitYear,MaxAge,Nsim))
	
	#==Make a matrix of past Fs based on a sd and an input time series==
	HistoricalFsInit	<-matrix(HistoricalFs,ncol=InitYear,nrow=Nsim,byrow=T)
	HistoricalFnInit	<-matrix(HistoricalFn,ncol=InitYear,nrow=Nsim,byrow=T)
	FerrN			<-matrix(rnorm(InitYear*Nsim,1,PastFsdN),ncol=InitYear)
	FerrS			<-matrix(rnorm(InitYear*Nsim,1,PastFsdS),ncol=InitYear)
	HistoricalFsIn	<-HistoricalFsInit*FerrN
	HistoricalFnIn	<-HistoricalFnInit*FerrS
	
	for(k in 1:Nsim)
	{
	 for (j in 2:InitYear)
	 {
	  for (i in 2:(MaxAge-1))
	  {
	   tempNn[j,i,k]		<-tempNn[j-1,i-1,k]*exp(-HistoricalFnIn[k,j]*vulnN[1,i-1])*exp(-NatMn[j])
	   tempNs[j,i,k]		<-tempNs[j-1,i-1,k]*exp(-HistoricalFsIn[k,j]*vulnS[1,i-1])*exp(-NatMs[j])
	  }
	   tempNn[j,MaxAge,k]	<-(tempNn[j-1,(MaxAge-1),k])*exp(-HistoricalFnIn[k,j]*vulnN[j,MaxAge])*exp(-NatMn[j])+ tempNn[j-1,MaxAge,k]*exp(-HistoricalFnIn[k,j]*vulnN[j,MaxAge])*exp(-NatMn[j])
	   tempNs[j,MaxAge,k]	<-(tempNs[j-1,(MaxAge-1),k])*exp(-HistoricalFsIn[k,j]*vulnS[j,MaxAge])*exp(-NatMs[j])+ tempNs[j-1,MaxAge,k]*exp(-HistoricalFsIn[k,j]*vulnS[j,MaxAge])*exp(-NatMs[j])
	
	   moveFromN		<-tempNn[j,,k]*MovementN[j,]
	   moveFromS		<-tempNs[j,,k]*MovementS[j,]
	
	   tempNn[j,,k]		<-tempNn[j,,k]-moveFromN+moveFromS
	   tempNs[j,,k]		<-tempNs[j,,k]-moveFromS+moveFromN
	
	   EggsN			<-sum(tempNn[j-1,,k]*matureN[j,]*WeightAtAgeN[j,])
	   EggsS			<-sum(tempNs[j-1,,k]*matureS[j,]*WeightAtAgeS[j,])
	
	   tempNn[j,1,k]		<-Recruitment(EggsIN=EggsN,steepnessIN=steepnessN[j],RzeroIN=RzeroN[j],RecErrIN=RecErrN[k,j],recType="BH",NatMin=NatMn[j],
								vulnIN=vulnN[j,],matureIN=matureN[j,],weightIN=WeightAtAgeN[j,],LenAtAgeIN=LenAtAgeN[j,],MaxAge=MaxAge,sigmaRin=sigmaRn[j])
	   tempNs[j,1,k]		<-Recruitment(EggsIN=EggsS,steepnessIN=steepnessS[j],RzeroIN=RzeroS[j],RecErrIN=RecErrS[k,j],recType="BH",NatMin=NatMs[j],
								vulnIN=vulnS[j,],matureIN=matureS[j,],weightIN=WeightAtAgeS[j,],LenAtAgeIN=LenAtAgeS[j,],MaxAge=MaxAge,sigmaRin=sigmaRs[j])
	   tempRecN[k,j]		<-tempNn[j,1,k]
	   tempRecS[k,j]		<-tempNs[j,1,k]
	
	   tempCatchAtAgeN[j,,k]<-((vulnN[j,]*HistoricalFnIn[k,j])/(vulnN[j,]*HistoricalFnIn[k,j]+NatMn[j])) * (1-exp(-(vulnN[j,]*HistoricalFnIn[k,j]+NatMn[j]))) * tempNn[j-1,,k]
	   tempCatchN[k,j]	<-sum(tempCatchAtAgeN[j,,k]*WeightAtAgeN[j,])
	
	   tempCatchAtAgeS[j,,k]<-((vulnS[j,]*HistoricalFsIn[k,j])/(vulnS[j,]*HistoricalFsIn[k,j]+NatMs[j])) * (1-exp(-(vulnS[j,]*HistoricalFsIn[k,j]+NatMs[j]))) * tempNs[j-1,,k]
	   tempCatchS[k,j]	<-sum(tempCatchAtAgeS[j,,k]*WeightAtAgeS[j,])
	 }
	}
	#===============================================================
	# BEGIN SIMULATION OF ASSESSMENT AND HARVEST
	#===============================================================
	#==tempNn and tempNs from above are the starting points
	projNn	<-array(dim=c(SimYear,MaxAge,Nsim))
	projNs	<-array(dim=c(SimYear,MaxAge,Nsim))
	
	projCatchAtAgeN	<-array(dim=c(SimYear,MaxAge,Nsim))
	projCatchAtAgeS	<-array(dim=c(SimYear,MaxAge,Nsim))
	
	projCatchN	<-matrix(nrow=Nsim,ncol=SimYear)
	projCatchS	<-matrix(nrow=Nsim,ncol=SimYear)
	projSSBn	<-matrix(nrow=Nsim,ncol=SimYear)
	projSSBs	<-matrix(nrow=Nsim,ncol=SimYear)
	projExpBn	<-matrix(nrow=Nsim,ncol=SimYear)
	projExpBs	<-matrix(nrow=Nsim,ncol=SimYear)
	projSurvN	<-matrix(nrow=Nsim,ncol=SimYear)
	projSurvS	<-matrix(nrow=Nsim,ncol=SimYear)
	projRecN	<-matrix(nrow=Nsim,ncol=SimYear)
	projRecS	<-matrix(nrow=Nsim,ncol=SimYear)
	projFmortN	<-matrix(nrow=Nsim,ncol=SimYear)
	projFmortS	<-matrix(nrow=Nsim,ncol=SimYear)
	
	projCatLenFreqN	<-array(dim=c(SimYear,LengthBinN,Nsim))
	projCatLenFreqS	<-array(dim=c(SimYear,LengthBinN,Nsim))
	projSurvLenFreqN	<-array(dim=c(SimYear,LengthBinN,Nsim))
	projSurvLenFreqS	<-array(dim=c(SimYear,LengthBinN,Nsim))
	
	#=storage for the true quantities
	trueRecN		<-matrix(ncol=SimYear,nrow=Nsim)
	trueCatchN		<-matrix(ncol=SimYear,nrow=Nsim)
	
	trueRecS		<-matrix(ncol=SimYear,nrow=Nsim)
	trueCatchS		<-matrix(ncol=SimYear,nrow=Nsim)
	
	trueFmortN		<-matrix(ncol=SimYear,nrow=Nsim)
	for(x in 1:Nsim)
	 trueFmortN[x,1:InitYear]<-HistoricalFn
	trueFmortS		<-matrix(ncol=SimYear,nrow=Nsim)
	for(x in 1:Nsim)
	 trueFmortS[x,1:InitYear]<-HistoricalFn
	
	trueSpbioN		<-matrix(ncol=SimYear,nrow=Nsim)
	trueSurvIndN	<-matrix(ncol=SimYear,nrow=Nsim)
	trueCPUEindN	<-matrix(ncol=SimYear,nrow=Nsim)
	
	trueSpbioS		<-matrix(ncol=SimYear,nrow=Nsim)
	trueSurvIndS	<-matrix(ncol=SimYear,nrow=Nsim)
	trueCPUEindS	<-matrix(ncol=SimYear,nrow=Nsim)
	
	trueF35   		<-rep(0,SimYear)
	trueSBPR35 		<-rep(0,SimYear)
	trueB35		    <-matrix(ncol=SimYear,nrow=Nsim)
	tempCatAtLenN<-array(dim=c(ncol(LenAtAgeN),LengthBinN,Nsim))
	tempCatAtLenS<-array(dim=c(ncol(LenAtAgeN),LengthBinN,Nsim))
	
	for(x in 1:Nsim)
	 for(y in 2:InitYear)
	 {
	 #==make length frequencies for catch==
	 for(w in 1:ncol(LenAtAgeN))
	 {
	  probtemp<-dnorm(LengthBinsMid,mean=LenAtAgeN[y,w],sd=GrowthSDn[y])
	  ProbN<-probtemp/sum(probtemp)
	  tempCatAtLenN[w,,x]<-tempCatchAtAgeN[y,w,x]*ProbN
	  probtemp<-dnorm(LengthBinsMid,mean=LenAtAgeS[y,w],sd=GrowthSDs[y])
	  ProbS<-probtemp/sum(probtemp)
	  tempCatAtLenS[w,,x]<-tempCatchAtAgeS[y,w,x]*ProbS
	 }
	 projCatLenFreqN[y,,x]<-apply(tempCatAtLenN[,,x],2,sum)
	 projCatLenFreqS[y,,x]<-apply(tempCatAtLenS[,,x],2,sum)
	}
	
	#==============================================================
	# calculate the survey proportion at length for assessment
	#==============================================================
	 tempSurvAtLenN<-array(dim=c(ncol(LenAtAgeN),LengthBinN,Nsim))
	 tempSurvAtLenS<-array(dim=c(ncol(LenAtAgeN),LengthBinN,Nsim))
	
	for(x in 1:Nsim)
	 for(y in 2:InitYear)
	 {
	 #==make length frequencies for s==
	 for(w in 1:ncol(LenAtAgeS))
	 {
	  probtemp<-dnorm(LengthBinsMid,mean=LenAtAgeN[y,w],sd=GrowthSDn[y])
	  ProbN<-probtemp/sum(probtemp)
	  tempSurvAtLenN[w,,x]<-tempNn[y,w,x]*survSelN[y,w]*ProbN
	  probtemp<-dnorm(LengthBinsMid,mean=LenAtAgeS[y,w],sd=GrowthSDs[y])
	  ProbS<-probtemp/sum(probtemp)
	  tempSurvAtLenS[w,,x]<-tempNs[y,w,x]*survSelS[y,w]*ProbS
	 }
	 projSurvLenFreqN[y,,x]<-apply(tempSurvAtLenN[,,x],2,sum)
	 projSurvLenFreqS[y,,x]<-apply(tempSurvAtLenS[,,x],2,sum)
	}
	for(x in 1:Nsim)
	{
		projNn[1:InitYear,,x]			<-tempNn[,,x]
		projNs[1:InitYear,,x]			<-tempNs[,,x]
		projCatchN[x,1:InitYear]		<-tempCatchN[x,]
		projCatchS[x,1:InitYear]		<-tempCatchS[x,]
		projRecN[x,1:InitYear]			<-tempRecN[x,]
		projRecS[x,1:InitYear]			<-tempRecS[x,]
		projFmortN[x,1:InitYear]		<-HistoricalFnInit[x,]
		projFmortS[x,1:InitYear]		<-HistoricalFsInit[x,]
		projSurvN[x,1:InitYear]			<-apply(tempNn[,,x]*survSelN[1:InitYear,]*WeightAtAgeN[1:InitYear,],1,sum)
		projSurvS[x,1:InitYear]			<-apply(tempNs[,,x]*survSelS[1:InitYear,]*WeightAtAgeS[1:InitYear,],1,sum)
	
		for(y in 1:InitYear)
		{
	 	projSSBn[x,y]	<-sum(projNn[y,,x]*matureN[y,]*WeightAtAgeN[y,])
	 	projSSBs[x,y]	<-sum(projNs[y,,x]*matureS[y,]*WeightAtAgeS[y,])
	 	projExpBn[x,y]	<-sum(projNn[y,,x]*vulnN[y,]*WeightAtAgeN[y,])
	 	projExpBs[x,y]	<-sum(projNs[y,,x]*vulnS[y,]*WeightAtAgeS[y,])
		}
	}
	
	projCatchN[,1]		<-0
	projCatchS[,1]		<-0
	
	#==fill in true storage arrays
	for(x in 1:Nsim) {
	trueCatchN[x,1:InitYear]		<-tempCatchN[x,]		
	}
	
	return(trueCatchN[!is.na(trueCatchN)])
}


Find_Rzero<-function(logRvec,trueCatch=tCatch,Nyrs=Nyears,OMname=CTLfile,MSEDir=dir.MSE) {
	#	mainOM <- as.matrix(read.csv(ctlfilename,header=F))
	#	RzeroLine <- grep("RzeroN",mainOM[,3])
	#
	#	RvecString <- paste0(Rvec[-length(Rvec)],split=",",collapse="")
	#	RvecString <- paste0("c(",RvecString,Rvec[length(Rvec)],")")
	#
	#	mainOM[RzeroLine,1] <- RvecString
	#
	#	write.table(mainOM,file=file.path(MSEdir,"Test.csv"),row.names=F,col.names=F,na="",sep=",")

	Rvec <- exp(logRvec^2)
	#	smoothingLL <- OMname$OM$RecruitPen * sum((Rvec[2:length(Rvec)]-Rvec[1:(length(Rvec)-1)])^2)
	smoothingLL <- 0.001*sum((Rvec[2:length(Rvec)]-Rvec[1:(length(Rvec)-1)])^2)
	InitYr <- OMname$OM$InitYear

	OMname$OM$RzeroN <- c(rep(Rvec[1],(InitYr-Nyrs)),Rvec)
	#	OMname$OM$Nsim <- 1
	#	OMname$OM$SimYear <- OMname$OM$InitYear+1
	#	OMname$OM$AssessmentType <- 0

	estCatch <- miniGeMS(OMname)[(InitYr-Nyrs):(InitYr-1)]

	if(length(estCatch) != length(trueCatch)) {stop("Nyears of OM does not match Nyears of observed catch.")}
	catchLL <- sum((estCatch - trueCatch)^2)
	catchLL <- catchLL + smoothingLL
	#	cat(paste0(catchLL,"\n"))
	if(is.na(catchLL)) {catchLL <- 9E20}
	return(catchLL)
}

#tempRvec <- c(10.004940,9.314674,10.704970,9.529311,8.623016,8.464436,8.929413,8.899917,8.544863,9.580275,10.023096,9.263349,10.138669,10.544040,11.121398,13.308079,11.373295,13.366014,13.571139,14.962503,14.189333,13.836758,14.045398,14.041382,14.054207,13.868701,14.136773,12.171786,14.650042,14.939543,13.563642)
#tempRvec <- c(3.218634,3.361886,3.317144,3.420753,3.515817,3.382012,3.520675,3.622005,3.656149,3.695863,3.733077,3.729675,3.702038,3.710736,3.707991,3.723767,3.714826,3.735953,3.742681,3.761789,3.780912,3.797085,3.805724,3.803103,3.797917,3.788047,3.779096,3.772777)
#tempRvec <- c(3.638243,3.639810,3.641743,3.644044,3.646650,3.649485,3.652466,3.655537,3.658614,3.661673,3.664587,3.667314,3.669761,3.671987,3.674047,3.675947,3.677698,3.679302,3.680764,3.682098,3.683310,3.684376,3.685286,3.685983,3.686470,3.686757,3.686883,3.686921)
#res <- optim(tempRvec,Find_Rzero,control=list(maxit=10000))
#res <- optim(tempRvec,Find_Rzero,control=list(maxit=10000),method="L-BFGS-B")
res <- optim(tempRvec,Find_Rzero,method="L-BFGS-B")

res2 <- res
for(i in 1:1000) {
	res1 <- res
	res <- optim(res$par,Find_Rzero,method="L-BFGS-B")

	cat(paste0(res$value,"\n"))
	#if(i%%10==0)
	#{
		newOM$OM$RzeroN <- exp(res$par^2)
		newOM$OM$RzeroN <- c(rep(newOM$OM$RzeroN[1],(newOM$OM$InitYear-Nyears)),newOM$OM$RzeroN)
		#newOM$OM$Nsim <- 1
		#newOM$OM$SimYear <- Nyears+2
		#newOM$OM$InitYear <- Nyears+1
		
		#GeMS(newOM, "Test",MSEdir=dir.MSE,GeMSdir=dir.GeMS)
		par(mfrow=c(2,1),mar=c(1,1,1,1),oma=c(3.3,3.3,1,1),xpd=NA)
		plot(miniGeMS(newOM),type="l",ylab="SYC Catch", xlab="",xaxt="n")
		lines(tCatch,x=(newOM$OM$InitYear-Nyears):(newOM$OM$InitYear-1),col="red")
		legend("topleft",legend=c("Observed","Calculated"),lty=1,col=c("red","black"))
		
		plot(newOM$OM$RzeroN,type="l",xlab="Year",ylab="Rzero")
	#}
}


InitYr <- CTLfile$OM$InitYear
Rvec <- exp(res$par^2)
paste0("c(",paste(c(rep(Rvec[1],(InitYr-Nyears)),Rvec,rep(rev(Rvec)[1],(CTLfile$OM$SimYear-InitYr))),collapse=","),")")
paste0("c(",paste(res$par,collapse=","),")")
cat(paste(c(rep(Rvec[1],(InitYr-Nyears)),Rvec,rep(rev(Rvec)[1],(CTLfile$OM$SimYear-InitYr))),collapse="\n"))


# SYC_1_noTV_1
# tempRvec <- logRvec <- c(3.322815,3.378362,3.310632,3.356366,3.449175,3.426118,3.435929,3.520617,3.621657,3.661277,3.675468,3.726448,3.747927,3.756060,3.725559,3.726213,3.711630,3.721733,3.711954,3.729777,3.728193,3.737423,3.748759,3.754897,3.768401,3.754084,3.745279,3.750011)
# 10/10
# tempRvec <- logRvec <- c(3.28193973328586,3.34865352920615,3.32575476372597,3.39546339482791,3.4425868744312,3.45468387438493,3.48981094983005,3.58252057718006,3.61709808889412,3.65782410651784,3.67803276454669,3.72569248896769,3.74950552235813,3.75031738665453,3.71560110352304,3.7209517183809,3.71174255646807,3.71479901308818,3.71657685491219,3.71866254671831,3.72691813698647,3.74283876603094,3.73467379969697,3.75164432944208,3.76041916395035,3.74916251799062,3.7208731884103,3.72714383099106)
# c(47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,47625.7286914811,74122.94842368,63617.5559664937,101637.888774364,140281.218162,152487.922006424,194615.377280634,374914.652575967,480892.285183127,646725.484872139,750072.402169368,1067451.42800331,1275427.10168441,1283216.66188128,990229.051500125,1030424.78779587,962253.112258475,984344.983561124,997436.252533044,1013024.67002583,1077246.49282511,1213284.89507209,1141425.39992334,1296054.4574319,1384365.24903591,1272150.82161553,1029822.77699393,1079060.30985955,1079060.30985955,1079060.30985955)
# Current 15/10
# c(30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,30045.4507942941,69762.8161324619,66789.136116601,117802.654458012,148541.841648488,171886.284079687,197882.429219281,293016.475297933,470530.747333832,567792.004452658,782689.62898642,1095110.14145555,1246999.21458056,1125531.6534386,863615.22565664,959604.146910388,945743.919497417,884273.25494036,844764.250036904,1028693.87598984,1092334.59030674,1099004.28369748,1201112.89837731,1244508.71583838,1222925.79361715,1167085.52668289,957258.009782952,1024461.30188525,1024461.30188525)

# SYC_2_allGAMs_1
# tempRvec <- logRvec <- c(3.218634,3.361886,3.317144,3.420753,3.515817,3.382012,3.520675,3.622005,3.656149,3.695863,3.733077,3.729675,3.702038,3.710736,3.707991,3.723767,3.714826,3.735953,3.742681,3.761789,3.780912,3.797085,3.805724,3.803103,3.797917,3.788047,3.779096,3.772777)
# 10/10
# tempRvec <- logRvec <- c(3.16774431928247,3.33262637212698,3.29938481846356,3.45423944316026,3.43883250021834,3.3408836458757,3.48953545205109,3.5329066724686,3.58486614727659,3.63510276776657,3.65493966401207,3.70442825815771,3.73718429934073,3.73508316622956,3.6942639436769,3.71196601979648,3.71304509454283,3.71558404035363,3.71979955851938,3.73719963203498,3.7488826372282,3.76015810005786,3.78191643032735,3.79604903563833,3.81014309194335,3.80545313955773,3.79803580246078,3.79895513315336)
# c(22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,22802.0123576084,66595.916191083,53419.9836344571,152020.41966153,136703.409592848,70368.6784862649,194241.531446282,263400.128560779,381270.84998349,547967.979264686,633226.810622629,911446.994841552,1163038.44445878,1144921.07063529,845417.684631109,963850.744379689,971604.273104655,990103.498841399,1021628.78077812,1163171.73914425,1269483.93785972,1381649.92034272,1628047.31107622,1812084.28783916,2017138.29862616,1946364.044326,1839631.71888843,1852524.9488227,1852524.9488227,1852524.9488227)
# 15/10
# c(12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,12424.9538884406,34027.75077488,44197.7176962891,101724.375061506,158911.236069884,50659.180225565,121864.245002041,257012.608372371,348296.78221974,400281.986044233,440958.307915135,770063.204424395,937137.75866489,801141.129592968,670829.806627289,722683.131238309,712651.407282553,764789.309495533,829653.797642101,894042.091316624,993231.173491289,1175645.13831306,1311463.21709422,1466401.05035078,1614603.38753473,1447527.0118742,1396825.77399345,1359685.29420451,1359685.29420451)

# SYC_3_changeM_1
# tempRvec <- logRvec <- c(3.253607,3.297577,3.322966,3.382585,3.379940,3.349765,3.430931,3.523572,3.545785,3.609618,3.634957,3.678478,3.706611,3.696470,3.662396,3.667572,3.675086,3.688739,3.685912,3.697891,3.712632,3.722106,3.745087,3.762320,3.777177,3.765035,3.758052,3.768154)
# 10/10
# tempRvec <- logRvec <- c(3.25234060244971,3.22584346961405,3.33367123674387,3.3157222928655,3.43556333417114,3.33292382972855,3.46294276641109,3.51326902795907,3.56261714637833,3.60059571030227,3.64786232057171,3.66860507910939,3.70137187495135,3.70329933538129,3.65828954329113,3.67789265528644,3.68354934338889,3.69213241190588,3.69212053936404,3.70163160522021,3.7091542382824,3.73115655041859,3.74146316092497,3.7614727803965,3.77305243712675,3.76000187152471,3.76040612896993,3.75772874916249)
# c(39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,39250.4973450983,33059.5609355873,67061.4012114227,59516.8249247146,133665.464404633,66728.0881378012,161453.524048366,229362.560190061,325214.723767554,426893.338832884,601330.217779406,699879.339109699,891048.160054627,903856.57194664,648931.460696077,749299.750825975,781160.384162085,832211.377512545,832138.420862187,892763.101477781,943946.871514272,1111842.60747173,1200857.67011377,1395380.1954746,1522591.98422318,1380027.62337201,1384229.54108765,1356645.07349642,1356645.07349642,1356645.07349642)
# 15/10
# c(39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,39257.8277934088,22205.126953367,94422.0364369608,41825.3773194868,80375.3738863309,73764.066721374,166939.181653783,185131.240744356,264388.746333975,361455.198030086,393087.36487242,563444.370617119,691919.365754608,609668.839276955,414456.119429818,518216.115922662,508616.209680037,583675.707267653,573958.800903067,625978.831807373,758064.712863567,793168.646371213,971588.648840405,1017768.23671627,1136692.09904978,1005043.42628205,979084.275283231,997251.139304291,997251.139304291)"

res <- optim(res$par,Find_Rzero,control=list(maxit=1000))



