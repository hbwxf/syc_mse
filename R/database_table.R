library(readr)
library(tidyr)
library(dplyr)

plotdir <- file.path(getwd(),"..","Paper","Figures","SupTableS1_Database.csv")

datfile <- read_csv("SYC_database.csv")
length(unique(datfile$Reference))

newdat <- datfile %>%
			group_by(Reference) %>%
			filter(Begin_study_date==min(Begin_study_date) | End_study_date==max(End_study_date)) %>%
			mutate(Begin=min(Begin_study_date),End=max(End_study_date)) %>%
			unite(Years, Begin, End, sep = " ~ ") %>%
			ungroup() %>%
			group_by(Reference,Years) %>%
			summarise(Parameters=paste(unique(Parameter_name),collapse="; "),Samples=sum(Sample_size))

write_csv(newdat,plotdir)