library(GeMS)

plotdir <- file.path(getwd(),"..","Paper","Figures","Fig3_BioRec.pdf")

colmed <-adjustcolor(RColorBrewer::brewer.pal(3,"Set1"),alpha=.75)
colq <-adjustcolor(RColorBrewer::brewer.pal(3,"Set1"),alpha=.3)

plotRecBiomass <- function(OMfile,EMvec,Rec_ylims=NA,Spbio_ylims=NA,F_ylims=NA,toplot=F,plotlegend=F,EMnames=NA,clrs=colmed,clrsq=colq,MSEdir=getwd(),plotname,...) {
	OMlist <- ReadCTLfile(OMfile)
	OMres <- PullTimevary(OMlist,MSEdir,EMvec,convRuns=temp)

	if(toplot) {
		if(is.na(EMnames[1])) EMnames <- EMvec

		Spbio_truth <- apply(OMres$TrueSpbio,2,median,na.rm=T)[OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)]
		Spbio_estimates <- apply(OMres$EstSpbio,c(2,3),median,na.rm=T)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]
		Spbio_up <- apply(OMres$EstSpbio,c(2,3),quantile,na.rm=T,probs=.25)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]
		Spbio_down <- apply(OMres$EstSpbio,c(2,3),quantile,na.rm=T,probs=.75)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]


		if(is.na(Spbio_ylims[1])) {Spbio_ylims <- range(c(Spbio_truth,Spbio_estimates),na.rm=T)}

		plot(0,type='n',ylim=Spbio_ylims,xlim=c(OMlist$OM$start_assessment,OMlist$OM$SimYear),xlab="",xaxt='n',...)

		lines(y=Spbio_truth,x=OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),lwd=2,lty=2)
		
		for(em in seq_along(EMvec)) {
			polygon(y=c(Spbio_up[,em],rev(Spbio_down[,em])),x=c(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),(OMlist$OM$SimYear-1):OMlist$OM$start_assessment),col=clrsq[em],border=NA)
			lines(y=Spbio_estimates[,em],x=OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),lwd=2,col=clrs[em])
#			lines(y=Spbio_down[,em],x=OMlist$OM$start_assessment:OMlist$OM$SimYear,lwd=1,col=clrs[em])
		}
		if(plotlegend) {
			legend("topleft",c("Truth",EMnames),lwd=2,col=c("black",clrs),lty=c(2,rep(1,length(EMvec))),bty='n')
			mtext("Biomass",side=2,xpd=NA,line=2.5)
		}
		mtext(plotname,side=3,line=1)

		Rec_truth <- apply(OMres$TrueRec,2,median,na.rm=T)[OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)]
		Rec_estimates <- apply(OMres$Recruitment,c(2,3),median,na.rm=T)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]
		Rec_up <- apply(OMres$Recruitment,c(2,3),quantile,na.rm=T,probs=.25)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]
		Rec_down <- apply(OMres$Recruitment,c(2,3),quantile,na.rm=T,probs=.75)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]

		if(is.na(Rec_ylims[1])) {Rec_ylims <- range(c(Rec_truth,Rec_estimates),na.rm=T)}

		plot(0,type='n',ylim=Rec_ylims,xlim=c(OMlist$OM$start_assessment,(OMlist$OM$SimYear-1)),xlab="",xaxt='n',...)		
		lines(y=Rec_truth,x=OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),lwd=2,lty=2)
		for(em in seq_along(EMvec)) {
			polygon(y=c(Rec_up[,em],rev(Rec_down[,em])),x=c(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),(OMlist$OM$SimYear-1):OMlist$OM$start_assessment),col=clrsq[em],border=NA)
			lines(y=Rec_estimates[,em],x=OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),lwd=2,col=clrs[em])
		}
		if(plotlegend) {
			mtext("Recruitment",side=2,xpd=NA,line=2.5)
		}


		F_truth <- apply(OMres$TrueFmort,2,median,na.rm=T)[OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)]
		F_estimates <- apply(OMres$FishMort,c(2,3),median,na.rm=T)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]
		F_up <- apply(OMres$FishMort,c(2,3),quantile,na.rm=T,probs=.25)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]
		F_down <- apply(OMres$FishMort,c(2,3),quantile,na.rm=T,probs=.75)[(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1)),]

		if(is.na(F_ylims[1])) {F_ylims <- range(c(F_truth,F_estimates),na.rm=T)}

		plot(0,type='n',ylim=F_ylims,xlim=c(OMlist$OM$start_assessment,(OMlist$OM$SimYear-1)),xlab="",...)		
		lines(y=F_truth,x=OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),lwd=2,lty=2)
		for(em in seq_along(EMvec)) {
			polygon(y=c(F_up[,em],rev(F_down[,em])),x=c(OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),(OMlist$OM$SimYear-1):OMlist$OM$start_assessment),col=clrsq[em],border=NA)
			lines(y=F_estimates[,em],x=OMlist$OM$start_assessment:(OMlist$OM$SimYear-1),lwd=2,col=clrs[em])
		}
		if(plotlegend) {
			mtext("Fishing Mortality",side=2,xpd=NA,line=2.5)
		}


	}

	return(list(OM=OMlist,OMres=OMres))
}

temp <- matrix(rep(TRUE,200),ncol=2)
OM1file <- "SYC_1_noTV_2"
EM1list <- c("SYC_1_noTV_2","SYC_1_noTV_3")
OM1 <- ReadCTLfile(OM1file)

OM1res <- PullTimevary(OM1,getwd(),EM1list,convRuns=temp)

#############

OM2file <- "SYC_2_allGAMs_2"
EM2list <- c("SYC_2_allGAMs_2","SYC_2_allGAMs_3")
OM2 <- ReadCTLfile(OM2file)

OM2res <- PullTimevary(OM2,getwd(),EM2list,convRuns=temp)

############

OM3file <- "SYC_3_changeM_2"
EM3list <- c("SYC_3_changeM_2","SYC_3_changeM_3")
OM3 <- ReadCTLfile(OM3file)

OM3res <- PullTimevary(OM3,getwd(),EM3list,convRuns=temp)

############
res1 <- plotRecBiomass(OM1file,EM1list,plotname="")
res2 <- plotRecBiomass(OM2file,EM2list,plotname="")
res3 <- plotRecBiomass(OM3file,EM3list,plotname="")

Recylims <- range(c(0,
				 res1$OMres$Recruitment,res1$OMres$TrueRec,
				 res2$OMres$Recruitment,res2$OMres$TrueRec,
				 res3$OMres$Recruitment,res3$OMres$TrueRec),na.rm=T)

Spbioylims <- range(c(0,
				 res1$OMres$True_Spbio,res1$OMres$EstSpbio,
				 res2$OMres$True_Spbio,res2$OMres$EstSpbio,
				 res3$OMres$True_Spbio,res3$OMres$EstSpbio),na.rm=T)

#Fylims <- range(c(0,
#				 res1$OMres$TrueFmort,res1$OMres$FishMort,
#				 res2$OMres$TrueFmort,res2$OMres$FishMort,
#				 res3$OMres$TrueFmort,res3$OMres$FishMort),na.rm=T)

Fylims <- range(c(0,
				 6),na.rm=T)

#tiff(plotdir,width=7,height=7,units="in",res=800, compression = "lzw")
pdf(plotdir,width=7,height=7)
par(mfcol=c(3,3),oma=c(4,4,3,.2),mar=c(.2,.2,.2,.2))
res1 <- plotRecBiomass(OM1file,EM1list,plotname="Time-Invariant",Rec_ylims=Recylims,Spbio_ylims=Spbioylims,F_ylims=Fylims,toplot=T,plotlegend=T,EMnames=c("Fixed M","Estimated M"))
res2 <- plotRecBiomass(OM2file,EM2list,plotname="Increasing M",Rec_ylims=Recylims,Spbio_ylims=Spbioylims,F_ylims=Fylims,toplot=T,yaxt='n')
res3 <- plotRecBiomass(OM3file,EM3list,plotname="Decreasing M",Rec_ylims=Recylims,Spbio_ylims=Spbioylims,F_ylims=Fylims,toplot=T,yaxt='n')
par(mfcol=c(1,1))
mtext("Year",side=1,xpd=NA,line=3)
dev.off()


