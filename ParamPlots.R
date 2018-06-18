cd Box\ Sync/GeMS/General-MSE/syc_mse
R

library(tidyverse)

params <- c("Linf","k","t0","M","a","b","Mat50","Mat95","Rzero_allGAMs")
dat <- read_csv("SYC_params.csv")

dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

ggplot(data=dat_long,mapping=aes(x=Year,y=Value,colour=Parameter)) +
	geom_line() +
	facet_wrap(~Parameter) +
	theme(text = element_text(size=20))

############

params <- c("Rzero_noTV","Rzero_allGAMs","Rzero_changeM")
dat <- read_csv("SYC_params.csv")

dat_long <- dat %>%
			gather(Parameter, Value, -Year, -tYear) %>%
			filter(Parameter%in%params) %>%
			mutate(Parameter=factor(Parameter, levels = params))

ggplot(data=dat_long,mapping=aes(x=Year,y=Value,colour=Parameter)) +
	geom_line() +
	theme(text = element_text(size=20)) +
	scale_colour_discrete(name  ="Operating Model",
                            breaks=params,
                            labels=c("Non Time-Varying", "All GAMs", "Declining M")) +
	ylab("Virgin Recruitment")