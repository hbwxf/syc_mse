cd Box\ Sync/GeMS/General-MSE/syc_mse
R

library(tidyverse)
theme_set(theme_classic())

params <- c("Linf","k","t0","M","a","b","Mat50","Mat95")
paramnames <- c("a) L[infinity]","b) Kappa","t[0]",
            	"M", "alpha", "beta",
            	"Maturity[50%]", "Maturity[95%]"))
dat <- read_csv("SYC_params.csv")

dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

Mtib<-tibble(Value=c(rep(0.277333514,40),seq(from=0.277333514,to=0.2,length.out=55),0.2,0.2,0.2),Parameter="M",tYear=filter(dat_long,Parameter=="M")$tYear)  %>%
			mutate(Parameter=factor(Parameter, levels = params))

tiff(file.path(plotdir,"Fig2_ParamPlots.tiff"),width=6,height=4,res=600,units="in")
ggplot(data=dat_long,mapping=aes(x=tYear,y=Value)) +
	geom_line() +
	facet_wrap(~Parameter, scales="free_y",nrow=2,ncol=4) +
	ylab("") +
	xlab("Year") +
	scale_x_continuous(breaks=range(dat_long$tYear)) +
	geom_line(data=Mtib,
			  mapping=aes(x=tYear,y=Value))
dev.off()

############

params <- c("Rzero_noTV","Rzero_allGAMs","Rzero_changeM")

dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

ggplot(data=dat_long,mapping=aes(x=Year,y=Value,colour=Parameter)) +
	geom_line() +
#	theme(text = element_text(size=20)) +
	scale_colour_discrete(name  ="Operating Model",
                            breaks=params,
                            labels=c("Constant", "Increasing M", "Decreasing M")) +
	ylab("Virgin Recruitment")

###########

params <- c("Linf","k","t0","M","a","b","Mat50","Mat95","Rzero_allGAMs")


dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

ggplot(data=dat_long,mapping=aes(x=Year,y=Value,colour=Parameter)) +
	geom_line() +
	facet_wrap(~Parameter) +
	theme(text = element_text(size=20))