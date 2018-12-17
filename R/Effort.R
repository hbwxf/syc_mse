cd ~/Box\ Sync/China_LQ/AquaFish
rt

library(tidyverse)
library(readxl)

effortfile <- "Data/vessels_fishmari_province.xlsx"
ECSProvinces <- c("Zhejiang","Fujian Province","Jiangsu Province","Shanghai","Shandong")

hp2kw <- function(datf) {
			temp<-dim(datf)[2]
			if(temp<=1) return(NA)
			datf <- datf %>%
					mutate_at(vars(2:temp),parse_number)


			if("Horsepower" %in% names(datf)) {
				temp<-which(names(datf)%in%"Horsepower")
				datf[,temp] <- datf[,temp]/1.34102
				names(datf)[temp] <- "Kilowatts"
			}

			return(datf)
}



effort <- effortfile %>%
				excel_sheets() %>%
				set_names() %>%
				map(read_excel,path=effortfile)

yrs <- as.numeric(names(effort))
for(i in seq_along(effort)) {
	temp<-dim(effort[[i]])[2]+1
	if(sum(temp)>1) {
		effort[[i]][,temp] <- yrs[i]
		names(effort[[i]])[temp] <- "Year"
	}
}

ECSeffort <- effort %>% 
			map_dfr(hp2kw) %>%
			filter(province %in% ECSProvinces) %>%
			group_by(Year) %>%
			summarise(Ships=sum(Ships),Tons=sum(Tons),Kilowatts=sum(Kilowatts)) 

# Fill in missing years
Yrvec <- min(ECSeffort$Year):max(ECSeffort$Year)
missingYears <- Yrvec[!Yrvec %in% ECSeffort$Year]
temp <- ECSeffort
for(y in missingYears) {
	temp <- temp %>% full_join(
	(ECSeffort[ECSeffort$Year==(y+1),]+ECSeffort[ECSeffort$Year==(y-1),])/2
	)
}

ECSeffort <- temp %>%
				arrange(Year) %>%
				filter(Year>=1986 & Year<=2013)


ggplot(ECSeffort) +
	geom_line(aes(x=Year,y=Tons)) +
	geom_line(aes(x=Year,y=Kilowatts)) +
	geom_line(aes(x=Year,y=Ships))



write_csv(ECSeffort,"~/Box\ Sync/China_LQ/syc_mse/Data/ECS_Effort.csv")