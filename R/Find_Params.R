cd ~/Box\ Sync/China_LQ/syc_mse/Current\ Runs
rt

# 2014 is (OM$InitYear-1)
library(GeMS)
library(dplyr)

dir.dat <- file.path(getwd(),"..","Data")
oldpar <- par(no.readonly=T)

MasterOMname <- "SYC_2_allGAMs_1"
#MasterOMname <- "SYC_3_changeM_1"

ctlfilename <- MasterOMname
CTLfile <- ReadCTLfile(ctlfilename)
CTLfile$OM$Nsim <- 1

yrchangeLH <- 46 #i.e. c(rep(vec[1],(yrchangeLH-1)),vec,rev(vec)[1])
yrchangeFS <- 45

out<- newOM <- CTLfile

paramdat <- read.csv(file.path(dir.dat,"SYC_params.csv"))
firstyear <- min(paramdat$tYear)

catchdat <- read.csv(file.path(dir.dat,"ECS_SYC_catches.csv"),header=T)
tCatch <- catchdat$Catch
Cyears <- catchdat$Year
Cyears <- Cyears[Cyears %in% paramdat$tYear]
Cindex <- (Cyears-firstyear)+1

# Missing 1982 and 2009
effortdat <- read.csv(file.path(dir.dat,"ECS_Effort.csv"))
Eyears <- effortdat$Year
Eyears <- Eyears[Eyears %in% paramdat$tYear]
tEffort <- effortdat$Kilowatts[effortdat$Year %in% Eyears]
Eindex <- (Eyears-firstyear)+1

tYears <- unique(c(Cyears,Eyears))
Nyears <- length(tYears)

LenComps <- read.csv(file.path(dir.dat,"FakeLenComps.csv"))
LenComps <- LenComps %>% filter(Year<=2013)
tLenFreq <- cbind((LenComps[,1]-firstyear+1),t(apply(LenComps[,-1],1,function(x) x/sum(x))))


# paramvec <- c(sqrt(log(RzeroN)), log(sel50n), log(sel95n), log(q))
 paramvec <- c(sqrt(log(CTLfile$OM$RzeroN))[Cindex],log(CTLfile$OM$sel50n)[yrchangeFS:CTLfile$OM$SimYear],log(CTLfile$OM$sel95n)[yrchangeFS:CTLfile$OM$SimYear],log(6E-7))

# SYC_1_noTV_1
# tempRvec <- logRvec <- c(3.49293774935434,4.33512139805958,4.41887033647284,4.46677019861378,4.49965063720264,4.52308255148875,4.54054757849236,4.55268341884631,4.56138458379387,4.56628646939909,4.5675536843914,4.56131525807924,4.5421455764159,4.49120950194161,3.68756302246947,4.35095156359801,3.24253542047144,4.28358368254685,3.53636873448639,4.29320735371574,3.70624663645934,3.97318113286013,3.77635421981443,4.00286126971524,3.85010509795193,3.91965713989963,3.6380920653603,4.38844297485508,4.37488194816333)


# SYC_2_allGAMs_1
# tempRvec <- logRvec <- c(3.4163775114052,1.99498043098554,3.68709941593389,3.76878679066162,3.81790371338995,3.85099968191242,3.87648213849955,3.89624262186802,3.91197957011037,3.92329899782346,3.9315125014966,3.93389742407816,3.92396237011933,3.88619442488659,3.81219467472983,3.83197763184383,3.83181033727299,3.8540695259196,3.86832850429266,3.88620059272261,3.90104822878442,3.9198648966449,3.94205408811059,3.95947110281312,3.9676796336361,3.96633328623673,3.973270373086,3.97859460761226,3.97859047767005)
# Penalties
# smoothingLL <- smoothLL(Rvec,1E-6) + smoothLL(sel50,15) + smoothLL(sel95,15)
# LenFreqLL <- -.0001*sum(log(estLenFreq)*trueLenFreq[,-1],na.rm=T)

# SYC_3_changeM_1
# tempRvec <- logRvec <- c(3.29824550816879,3.63056109861048,3.71687016762725,3.76729442134179,3.80249545980056,3.82860533611471,3.84911437811408,3.86484398365185,3.8766453025401,3.8838833265021,3.8868831459901,3.88212250220607,3.86305700627406,3.81023951892637,3.69722609815576,3.71971865945037,3.71821961538392,3.73457225478281,3.73755516101395,3.75639539306737,3.77008158378599,3.78499419488393,3.806055188913,3.82098594369777,3.81678800063999,3.79260128873636,3.79399216482833,3.81819178708157,3.81818696613326)
# Penalties
# smoothingLL <- smoothLL(Rvec,5E-6) + smoothLL(sel50,12) + smoothLL(sel95,12)
# LenFreqLL <- -.01*sum(log(estLenFreq)*trueLenFreq[,-1],na.rm=T)


miniGeMS <- function(out) {
{	set.seed(123)
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
	for(i in 2:SimYear) {
	  LenAtAgeN[i,1]<-LinfN[i]*(1-exp(-VonKn[i]*(Ages[1]-t0n[i])))
	 for(j in 2:MaxAge)
	  LenAtAgeN[i,j]<-LenAtAgeN[i-1,j-1]+(LinfN[i]-LenAtAgeN[i-1,j-1])*(1-exp(-VonKn[i]))
	}
	LenAtAgeS<-LenAtAgeN
	
	
	if(TwoPop>0)
	{ 
	VonKs		<-CleanInput(out$OM$VonKs,SimYear)
	LinfS		<-CleanInput(out$OM$LinfS,SimYear)
	t0s		<-CleanInput(out$OM$t0s,SimYear)
	LenAtAgeS	<-matrix(nrow=SimYear,ncol=MaxAge)
	
	LenAtAgeS[1,]<-LinfS[1]*(1-exp(-VonKs[1]*(Ages-t0s[1])))
	LenAtAgeS[,1]<-LenAtAgeS[1,1]
	for(i in 2:SimYear) {
	  LenAtAgeN[i,1]<-LinfN[i]*(1-exp(-VonKn[i]*(Ages[1]-t0n[i])))
	 for(j in 2:MaxAge)
	  LenAtAgeN[i,j]<-LenAtAgeN[i-1,j-1]+(LinfN[i]-LenAtAgeN[i-1,j-1])*(1-exp(-VonKn[i]))
		}	
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

   tempCatAtLenN<-array(dim=c(ncol(LenAtAgeN),LengthBinN,Nsim))
  
   projCatLenFreqN	<-array(dim=c(SimYear,LengthBinN,Nsim))

  for(x in 1:Nsim)
   for(y in 2:InitYear)
   {
   #==make length frequencies for catch==
   for(w in 1:ncol(LenAtAgeN))
   {
    probtemp<-dnorm(LengthBinsMid,mean=LenAtAgeN[y,w],sd=GrowthSDn[y])
    ProbN<-probtemp/sum(probtemp)
    tempCatAtLenN[w,,x]<-tempCatchAtAgeN[y,w,x]*ProbN
   }
   projCatLenFreqN[y,,x]<-apply(tempCatAtLenN[,,x],2,sum)
  }
#   par(mfrow=c(ceiling(sqrt(InitYear)),ceiling(sqrt(InitYear))),mar=c(.1,.1,.1,.1))
#   for(i in 2:InitYear)
#   	barplot(rbind(projCatLenFreqN[i,,1]),beside=T,xaxt='n',yaxt='n')
	}
	trueCatchN		<-matrix(ncol=SimYear,nrow=Nsim)
	#==fill in true storage arrays
	for(x in 1:Nsim) {
	trueCatchN[x,1:InitYear]		<-tempCatchN[x,]		
	}
	tempres<-list()
	tempres$LenFreq<-t(apply(projCatLenFreqN,1,function(x) x/sum(x)))
	tempres$trueCatchN <- as.vector(trueCatchN)
	return(tempres)
}

smoothLL <- function(x,tol=1) {
	#res<-tol*norm(diff(x,lag=1),type="2") # Apparently slower than below
	res<-tol*sqrt(sum(diff(x,lag=1)^2))
	return(res)
}

# Need to play with weights
Find_params <-function(paramvec,
					 trueCatch=tCatch,trueEffort=tEffort,trueLenFreq=tLenFreq,
					 Cind=Cindex,Eind=Eindex,
					 FSyr=yrchangeFS,#LHyr=yrchangeLH,
					 OMname=newOM) {

	SimYear <- OMname$OM$SimYear
	
	logRvec <- paramvec[seq_along(Cind)]
	paredparams <- paramvec[-(seq_along(logRvec))]
	logsel50n <- paredparams[seq_along(FSyr:SimYear)]
	paredparams <- paredparams[-(seq_along(logsel50n))]
	logsel95n <- paredparams[seq_along(FSyr:SimYear)]
	logq <- rev(paramvec)[1]
		
	Rvec <- exp(logRvec^2)
	sel50 <- exp(logsel50n)
	sel95 <- exp(logsel95n)
	qpar <- exp(logq)
	HistoricalF <- qpar*trueEffort

	#OM 2
	smoothingLL <- smoothLL(Rvec,4E-5) + smoothLL(sel50,20) + smoothLL(sel95,40)

	#OM 3
#	smoothingLL <- smoothLL(Rvec,1E-6) + smoothLL(sel50,1) + smoothLL(sel95,1)

	InitYr <- OMname$OM$InitYear
	
	OMname$OM$RzeroN <- c(rep(Rvec[1],(InitYr-length(Rvec))),Rvec,rev(Rvec)[1])
	
	# Assuming initial F in 50s was .05 of F_2000
	InitF <- .05*HistoricalF[22]
	OMname$OM$HistoricalFn <- c(rep(InitF,(FSyr-1)),seq(from=InitF, to=HistoricalF[1],length=(Eind[1]-FSyr+1)),HistoricalF[-1])
	
	OMname$OM$sel50n <- c(rep(sel50[1],(FSyr-1)),sel50)
	OMname$OM$sel95n <- c(rep(sel95[1],(FSyr-1)),sel95)

	temp<-miniGeMS(OMname)

	estCatch <- temp$trueCatchN[Cind]
	if(length(estCatch) != length(trueCatch)) {stop("Nyears of OM does not match Nyears of observed catch.")}
	catchLL <- sum(((log(estCatch) - log(trueCatch))^2)/log(newOM$OM$CatchCVn*newOM$OM$CatchCVn+1))
	
	estLenFreq <- temp$LenFreq[trueLenFreq[,1],]
	LenFreqLL <- -10*sum(log(estLenFreq)*trueLenFreq[,-1],na.rm=T)
	
	totLL <- catchLL + smoothingLL + LenFreqLL
	if(is.na(totLL)) {totLL <- 9E20}
	return(totLL)
}

#res <- optim(tempRvec,Find_Rzero,control=list(maxit=10000))
#res <- optim(tempRvec,Find_Rzero,control=list(maxit=10000),method="L-BFGS-B")
#res <- optim(paramvec,Find_params,method="L-BFGS-B")
res <-nlminb(paramvec,Find_params)

res2 <- res
for(i in 1:100000) {
	res1 <- res
#	res <- optim(res$par,Find_Rzero,method="L-BFGS-B")
	res<-nlminb(res$par,Find_params)

#	cat(paste0(res$value,"\n"))
	cat(paste0(res$objective,"\n"))

	logRvec <- res$par[seq_along(Cindex)]
	paredparams <- res$par[-(seq_along(logRvec))]
	logsel50n <- paredparams[seq_along(yrchangeFS:newOM$OM$SimYear)]
	paredparams <- paredparams[-(seq_along(logsel50n))]
	logsel95n <- paredparams[seq_along(yrchangeFS:newOM$OM$SimYear)]
	logq <- rev(res$par)[1]

	Rvec <- exp(logRvec^2)
	sel50 <- exp(logsel50n)
	sel95 <- exp(logsel95n)
	qpar <- exp(logq)
	HistoricalF <- qpar*tEffort
	
	InitYr <- newOM$OM$InitYear
	
	newOM$OM$RzeroN <- c(rep(Rvec[1],(InitYr-length(Rvec))),Rvec,rev(Rvec)[1])
	
	# Assuming initial F in 50s was .05 of F_2000
	InitF <- .05*HistoricalF[22]
	newOM$OM$HistoricalFn <- c(rep(InitF,(yrchangeFS-1)),seq(from=InitF, to=HistoricalF[1],length=(Eindex[1]-yrchangeFS+1)),HistoricalF[-1])
	
	newOM$OM$sel50n <- c(rep(sel50[1],(yrchangeFS-1)),sel50)
	newOM$OM$sel95n <- c(rep(sel95[1],(yrchangeFS-1)),sel95)

	temp<-miniGeMS(newOM)
	predCatch <- as.vector(temp$trueCatchN)

	par(oldpar)
	par(mfrow=c(2,1),mar=c(1,1,1,1),oma=c(3.3,3.3,1,1),xpd=NA)
	plot(predCatch,type="l",ylab="SYC Catch", xlab="",xaxt="n",xlim=c(1,newOM$OM$SimYear),ylim=c(0,max(c(predCatch,tCatch),na.rm=T)))
	lines(tCatch,x=Cindex,col="red")
	legend("topleft",legend=c("Observed","Calculated"),lty=1,col=c("red","black"))
	
	plot(newOM$OM$RzeroN,type="l",xlab="Year",ylab="Rzero",ylim=range(newOM$OM$RzeroN))
}


SimYear <- newOM$OM$SimYear

logRvec <- res$par[seq_along(Cindex)]
paredparams <- res$par[-(seq_along(logRvec))]
logsel50n <- paredparams[seq_along(yrchangeFS:SimYear)]
paredparams <- paredparams[-(seq_along(logsel50n))]
logsel95n <- paredparams[seq_along(yrchangeFS:SimYear)]
logq <- rev(res$par)[1]
	
Rvec <- exp(logRvec^2)
sel50 <- exp(logsel50n)
sel95 <- exp(logsel95n)
qpar <- exp(logq)
HistoricalF <- qpar*tEffort

#OM 2
smoothingLL <- smoothLL(Rvec,5E-5) + smoothLL(sel50,20) + smoothLL(sel95,40)
#smoothingLL <- smoothLL(Rvec,0) + smoothLL(sel50,0) + smoothLL(sel95,0)

#OM 3
#	smoothingLL <- smoothLL(Rvec,1E-6) + smoothLL(sel50,1) + smoothLL(sel95,1)

InitYr <- newOM$OM$InitYear

newOM$OM$RzeroN <- c(rep(Rvec[1],(InitYr-length(Rvec))),Rvec,rev(Rvec)[1])

# Assuming initial F in 50s was .05 of F_2000
InitF <- .05*HistoricalF[22]
newOM$OM$HistoricalFn <- c(rep(InitF,(yrchangeFS-1)),seq(from=InitF, to=HistoricalF[1],length=(Eindex[1]-yrchangeFS+1)),HistoricalF[-1])

newOM$OM$sel50n <- c(rep(sel50[1],(yrchangeFS-1)),sel50)
newOM$OM$sel95n <- c(rep(sel95[1],(yrchangeFS-1)),sel95)

temp<-miniGeMS(newOM)

estCatch <- temp$trueCatchN[Cindex]
if(length(estCatch) != length(tCatch)) {stop("Nyears of OM does not match Nyears of observed catch.")}
catchLL <- sum(((log(estCatch) - log(tCatch))^2)/log(newOM$OM$CatchCVn*newOM$OM$CatchCVn+1))

estLenFreq <- temp$LenFreq[tLenFreq[,1],]
LenFreqLL <- -10*sum(log(estLenFreq)*tLenFreq[,-1],na.rm=T)

totLL <- catchLL + smoothingLL + LenFreqLL


par(mfcol=c(2,4),mar=c(.2,2,.2,.1))
for(y in 1:4) {

    	barplot(estLenFreq[y,],beside=T,yaxt='n',xaxt='n')
    	if(y==1) mtext(side=2, "Estimated")
        barplot(tLenFreq[y,-1],beside=T,yaxt='n',xaxt='n')
        if(y==1) mtext(side=2,"Truth")
    }

par(oldpar)
par(mfrow=c(1,3))
plot(newOM$OM$sel50n,type="l",ylab="Sel 50")
plot(newOM$OM$sel95n,type="l",ylab="Sel 95")
plot(newOM$OM$HistoricalFn,type="l",ylab="Historical F")


#####
#Nrec<-length(res$par)-1
#InitYr <- CTLfile$OM$InitYear
#Rvec <- exp(res$par[1:Nrec]^2)
paste0("c(",paste(c(rep(Rvec[1],(InitYr-Nyears)),Rvec[1:Nrec],rep(Rvec[Nrec],(CTLfile$OM$SimYear-InitYr))),collapse=","),")")
paste0("c(",paste(res$par[1:Nrec],collapse=","),")")
cat(paste(c(rep(Rvec[1],(InitYr-Nyears)),Rvec,rep(rev(Rvec)[1],(CTLfile$OM$SimYear-InitYr))),collapse="\n"))

cat(paste(newOM$OM$RzeroN,collapse="\n"))
cat(paste(newOM$OM$sel50n,collapse="\n"))
cat(paste(newOM$OM$sel95n,collapse="\n"))
cat(paste(newOM$OM$HistoricalFn,collapse="\n"))


paste0("c(",paste(newOM$OM$RzeroN,collapse=","),")")
paste0("c(",paste(newOM$OM$sel50n,collapse=","),")")
paste0("c(",paste(newOM$OM$sel95n,collapse=","),")")
paste0("c(",paste(newOM$OM$HistoricalFn,collapse=","),")")
cat(paste(c(rep(Rvec[1],(InitYr-Nyears)),Rvec,rep(rev(Rvec)[1],(CTLfile$OM$SimYear-InitYr))),collapse="\n"))


res <- optim(res$par,Find_Rzero,control=list(maxit=100000))



