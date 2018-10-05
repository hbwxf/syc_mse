library(GeMS)

plotdir <- file.path(getwd(),"Paper","Figures","Fig4_BioRec.tiff")

colmed <-adjustcolor(RColorBrewer::brewer.pal(3,"Set1"),alpha=.75)

plotRecBiomass <- function(OMfile,EMvec,Rec_ylims=NA,Spbio_ylims=NA,toplot=F,plotlegend=F,EMnames=NA,clrs=colmed,MSEdir=getwd(),...) {
	OMlist <- ReadCTLfile(OMfile)
	OMres <- PullTimevary(OMlist,MSEdir,EMvec)

	if(toplot) {
		if(is.na(EMnames[1])) EMnames <- EMvec

		Spbio_truth <- apply(OMres$TrueSpbio,2,median,na.rm=T)
		Spbio_estimates <- apply(OMres$EstSpbio,c(2,3),median,na.rm=T)
		if(is.na(Spbio_ylims[1])) {Spbio_ylims <- range(c(Spbio_truth,Spbio_estimates),na.rm=T)}

		plot(0,type='n',ylim=Spbio_ylims,xlim=c(1,OMlist$OM$SimYear),xlab="",xaxt='n',...)

		lines(y=Spbio_truth,x=1:OMlist$OM$SimYear,lwd=2,lty=2)
		for(em in seq_along(EMvec)) {
			lines(y=Spbio_estimates[,em],x=1:OMlist$OM$SimYear,lwd=2,col=clrs[em])
		}
		if(plotlegend) {
			legend("topleft",c("Truth",EMnames),lwd=2,col=c("black",clrs),lty=c(2,rep(1,length(EMvec))))
			mtext("Biomass",side=2,xpd=NA,line=2.5)
		}


		Rec_truth <- apply(OMres$TrueRec,2,median,na.rm=T)
		Rec_estimates <- apply(OMres$Recruitment,c(2,3),median,na.rm=T)

		if(is.na(Rec_ylims[1])) {Rec_ylims <- range(c(Rec_truth,Rec_estimates),na.rm=T)}

		plot(0,type='n',ylim=Rec_ylims,xlim=c(1,OMlist$OM$SimYear),xlab="",...)		
		lines(y=Rec_truth,x=1:OMlist$OM$SimYear,lwd=2,lty=2)
		for(em in seq_along(EMvec)) {
			lines(y=Rec_estimates[,em],x=1:(OMlist$OM$SimYear-1),lwd=2,col=clrs[em])
		}
		if(plotlegend) {
			mtext("Recruitment",side=2,xpd=NA,line=2.5)
		}


	}

	return(list(OM=OMlist,OMres=OMres))
}

OM1file <- "SYC_1_noTV_2"
EM1list <- c("SYC_1_noTV_2","SYC_1_noTV_3")
OM1 <- ReadCTLfile(OM1file)

OM1res <- PullTimevary(OM1,getwd(),EM1list)

#############

OM2file <- "SYC_2_allGAMs_2"
EM2list <- c("SYC_2_allGAMs_2","SYC_2_allGAMs_3")
OM2 <- ReadCTLfile(OM2file)

OM2res <- PullTimevary(OM2,getwd(),EM2list)

############

OM3file <- "SYC_3_changeM_2"
EM3list <- c("SYC_3_changeM_2","SYC_3_changeM_3")
OM3 <- ReadCTLfile(OM3file)

OM3res <- PullTimevary(OM3,getwd(),EM3list)

############
res1 <- plotRecBiomass(OM1file,EM1list)
res2 <- plotRecBiomass(OM2file,EM2list)
res3 <- plotRecBiomass(OM3file,EM3list)

Recylims <- range(c(0,
				 res1$OMres$Recruitment,res1$OMres$TrueRec,
				 res2$OMres$Recruitment,res2$OMres$TrueRec,
				 res3$OMres$Recruitment,res3$OMres$TrueRec),na.rm=T)

Spbioylims <- range(c(0,
				 res1$OMres$True_Spbio,res1$OMres$EstSpbio,
				 res2$OMres$True_Spbio,res2$OMres$EstSpbio,
				 res3$OMres$True_Spbio,res3$OMres$EstSpbio),na.rm=T)

tiff(plotdir,width=6,height=4,units="in",res=300)
par(mfcol=c(2,3),oma=c(4,4,.2,.2),mar=c(.2,.2,.2,.2))
res1 <- plotRecBiomass(OM1file,EM1list,Rec_ylims=Recylims,Spbio_ylims=Spbioylims,toplot=T,plotlegend=T,EMnames=c("Fixed M","Estimated M"))
res2 <- plotRecBiomass(OM2file,EM2list,Rec_ylims=Recylims,Spbio_ylims=Spbioylims,toplot=T,yaxt='n')
res3 <- plotRecBiomass(OM3file,EM3list,Rec_ylims=Recylims,Spbio_ylims=Spbioylims,toplot=T,yaxt='n')
par(mfcol=c(1,1))
mtext("Year",side=1,xpd=NA,line=3)
dev.off()


