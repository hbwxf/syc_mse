cd ~/Box\ Sync/China_LQ/AquaFish
rt

library(tidyverse)
library(readxl)

catchfiles <- c("Data/fishing_fish1_province.xlsx",
				"Data/fishing_fish2_province.xlsx",
				"Data/fishing_fish3_province.xlsx")
ECSProvinces <- c("Zhejiang","Fujian","Shanghai","Jiangsu","Shandong")

chr2num <- function(datf) {
			temp<-dim(datf)[2]
			if(temp<=1) return(NA)
			datf <- datf %>%
					mutate_at(vars(2:temp),parse_number)

			return(datf)
}

extract_sheets <- function(dat) {
	res<- dat %>%
			excel_sheets() %>%
			set_names() %>%
			map(read_excel,path=dat)

	yrs <- as.numeric(names(res))
	for(i in seq_along(res)) {
		temp<-grep("X_",names(res[[i]]))
		if(sum(temp)>0) res[[i]]<-res[[i]][,-temp]
		temp<-dim(res[[i]])[2]+1
		if(sum(temp)>1) {
			res[[i]][,temp] <- yrs[i]
			names(res[[i]])[temp] <- "Year"
		}
		temp<-grep("__",names(res[[i]]))
		if(sum(temp)>0) cat(i)
	}

	resdf <- res %>%
				map_df(chr2num)
	names(resdf)<-str_replace_all(names(resdf)," ","_")
	resdf$Province<-resdf$Province %>%
					str_replace_all(" Province","") %>%
					str_replace_all("Black Dragon","Heilongjiang")

	return(resdf)
}

catch <- catchfiles %>%
				map_df(extract_sheets)

SYCtotal<- catch %>%
			group_by(Year) %>%
			summarise(Catch=sum(small_yellow_croaker,na.rm=T))

ggplot(SYCtotal,aes(x=Year,y=Catch)) +
	geom_col()

ECScatch <- catch %>%
			filter(Province %in% ECSProvinces) %>%
			group_by(Year) %>%
			summarise(Catch=sum(small_yellow_croaker,na.rm=T))

ggplot(ECScatch,aes(x=Year,y=Catch)) +
	geom_col()


FAOtot <- c(63800,81500,82100,79900,80300,82100,110300,124300,105200,118000,131300,114000,100300,107700,111700,134500,133500,114500,114600,89600,104900,85400,62000,77200,76030,54855,58150,35914,55188,80124,90781,76244,52097,41701,31119,41656,33536,44648,46460,38807,52895,85553,103667,110544,143504,181553,280036,156195,185971,225513,262090,218139,233274,245382,284936,293674,342364,373249,388018,407081,438837,458801,437613,399600,370520,411735)

catchtib <- tibble(Catch=FAOtot,Year=1950:2015,Source="FAO Global") %>%
				add_row(Catch=SYCtotal$Catch,Year=SYCtotal$Year,Source="China") %>%
				add_row(Catch=ECScatch$Catch,Year=ECScatch$Year,Source="East China Sea & Yellow Sea")
ggplot(data = catchtib) + 
  geom_col(mapping = aes(x = Year, y = Catch,fill=Source),position="identity") +
  theme(text = element_text(size=20)) +
  scale_y_continuous(name = "Catch (t)") +
  theme_classic()

 ECScatch2use <- ECScatch %>%
 				 filter(Year>=1986 & Year<=2013)

write_csv(ECScatch2use,"~/Box\ Sync/China_LQ/syc_mse/Data/ECS_SYC_catches.csv")

