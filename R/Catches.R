dir.MSE <- file.path(getwd(),"Data")

library(tidyverse)

catchdat <- read.csv(file.path(dir.MSE,"SYC_catches.csv"),header=T)
tCatch <- catchdat$Larimichthys.polyactis
FAOtot <- c(63800,81500,82100,79900,80300,82100,110300,124300,105200,118000,131300,114000,100300,107700,111700,134500,133500,114500,114600,89600,104900,85400,62000,77200,76030,54855,58150,35914,55188,80124,90781,76244,52097,41701,31119,41656,33536,44648,46460,38807,52895,85553,103667,110544,143504,181553,280036,156195,185971,225513,262090,218139,233274,245382,284936,293674,342364,373249,388018,407081,438837,458801,437613,399600,370520,411735)

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


preddat <- read_csv(file.path(dir.MSE,"SYC_predators.csv"))

newdat <- preddat %>%
			gather(key="Species", value="Catch", 3:5) %>%
			add_row(Province="ECS",Catch=tCatch,Year=catchdat$Year,Species="Small_yellow_croaker") %>%
			group_by(Year,Species) %>%
			drop_na() %>%
			summarise(Total=sum(Catch))

ggplot(newdat) +
geom_line(aes(x=Year,y=Total,colour=Species))