library(GeMS)
plotdir <- file.path(getwd(),"..","Paper","Figures","Fig5_NatM.tiff")
p1 <- PlotParam("NatM",OMfile="SYC_1_noTV_3",EMvec="SYC_1_noTV_3",plotnames="EM Estimate",dolegend=T,xlab='n',yaxt='n')
p2 <- PlotParam("NatM",OMfile="SYC_2_allGAMs_3",EMvec="SYC_2_allGAMs_3")
p3 <- PlotParam("NatM",OMfile="SYC_3_changeM_3",EMvec="SYC_2_allGAMs_3")

ylims <- range(p1$truth,p2$truth,p3$truth,
			   p1$estimates$Quant95,p2$estimates$Quant95,p3$estimates$Quant95,
			   p1$estimates$Quant05,p2$estimates$Quant05,p3$estimates$Quant05
				)

tiff(plotdir,,width=6,height=4,units="in",res=300)
par(mfrow=c(1,3),oma=c(4,4,.2,.2),mar=c(.2,.2,.2,.2))

PlotParam("NatM",OMfile="SYC_1_noTV_3",EMvec="SYC_1_noTV_3",plotnames="Estimate",ylims=ylims,dolegend=T,legpos="bottomleft",ylab='',xlab='')
mtext(side=2,"Natural Mortality",line=2)
PlotParam("NatM",OMfile="SYC_2_allGAMs_3",EMvec="SYC_2_allGAMs_3",ylims=ylims,yaxt='n',ylab='',xlab='')
PlotParam("NatM",OMfile="SYC_3_changeM_3",EMvec="SYC_2_allGAMs_3",ylims=ylims,yaxt='n',ylab='',xlab='')

par(mfrow=c(1,1),xpd=NA)
mtext(side=1,"Year",line=3)
dev.off()
