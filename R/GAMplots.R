cd ~/Box\ Sync/China_LQ/syc_mse
rt

plotdir <- file.path(getwd(),"Paper/Figures")
library(tidyverse)
theme_set(theme_classic())

params <- c("Linf","k","t0","M","a","b","Mat50","Mat95")
paramnames <- c("L[infinity]",
				"Kappa",
				"t[0]",
            	"M", 
            	"alpha", 
            	"beta",
            	"Maturity[50%]",
            	"Maturity[95%]")

parnames<-c(expression(L[infinity]),
			expression(Kappa),
			expression(t[0]),
            "M", 
            expression(alpha), 
            expression(beta),
            expression(Maturity[50*'%']),
            expression(Maturity[95*'%'])
            )

dat <- read_csv("Data/SYC_params.csv")

dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

Mtib <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%"M_change") %>%
			mutate(Parameter=factor("M", levels = params))

levels(dat_long$Parameter) <- parnames
#levels(Mtib$Parameter) <- parnames

tiff(file.path(plotdir,"Fig2_ParamPlots.tiff"),width=6,height=4,res=300,units="in")
ggplot(data=dat_long,mapping=aes(x=tYear,y=Value)) +
	geom_line() +
	facet_wrap(~Parameter, scales="free_y",nrow=2,ncol=4,labeller="label_parsed") +
	ylab("") +
	xlab("Year") +
	scale_x_continuous(breaks=as.numeric(round(quantile(dat_long$tYear)))[c(1,3,5)]) +
	geom_line(data=Mtib,
			  mapping=aes(x=tYear,y=Value)) +
	theme(strip.background = element_blank(),plot.margin=unit(c(.1,.5,.1,.1),"cm"))
dev.off()

############

rparams <- c("Rzero_noTV","Rzero_allGAMs","Rzero_changeM")

rdat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%rparams) %>%
			mutate(Parameter=factor(Parameter, levels = rparams))

tiff(file.path(plotdir,"SupFigS2_Recruitment.tiff"),width=5,height=4,res=300,units="in")
ggplot(data=rdat_long,mapping=aes(x=tYear,y=Value,colour=Parameter)) +
	geom_line() +
	scale_colour_discrete(name ="Operating Model",
                            breaks=rparams,
                            labels=c("Constant", "Increasing M", "Decreasing M")) +
	ylab("Virgin Recruitment") +
	xlab("Year")
dev.off()

###########

params <- c("Linf","k","t0","M","a","b","Mat50","Mat95","Rzero_allGAMs")


dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

ggplot(data=dat_long,mapping=aes(x=Year,y=Value)) +
	geom_line() +
	facet_wrap(~Parameter, scales="free_y") +
	theme(text = element_text(size=20))

