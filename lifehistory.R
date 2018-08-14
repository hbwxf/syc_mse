cd ~/Box\ Sync/China_LQ/lifehistory
R

############
# For SYC GAM parameter plots
# Based on lifehistory plot.R from lifehistory repo
############

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(mgcv)


data <- read_excel("Database China Life History.xlsx")
plotdir<-"~/Box\ Sync/China_LQ/syc_mse/Paper/Figures"

#==change scientific notation
chk<-grep('\\*',data$Value)
for(x in chk)
  data$Value[x]<-eval(parse(text=data$Value[x]))

#==shift to cm
data$Value[which(data$Units=="mm")]<-as.numeric(data$Value[which(data$Units=="mm")])/10

#==deal with changes in alpha from shifts from g to kg
inds<-which(data$Parameter_name=="a" & as.numeric(data$Value)>0.003)
data$Value[inds] <- as.numeric(data$Value[inds])/1000


#==count the number of entries for a given species   
count_entries<-function(species,parameter)
{
  temp<-data %>% filter(Species == species & Parameter_name == parameter)
  return(nrow(temp))
}

spec_name<-"Larimichthys polyactis"
counted<-count_entries(species=spec_name,parameter="Linf")

#==plot species with more entries than X (here 3)
use_spec<-spec_name[which(as.numeric(counted)>3)]
unq_par<-c("Linf","k","t0","M","a","b")
parnames<-c("L[infinity]","Kappa","t[0]",
            "M", "alpha", "beta")
parnames<-c(expression(L[infinity]),expression(Kappa),expression(t[0]),
            "M", expression(alpha), expression(beta))

use_LME<-c("East China Sea","Yellow Sea","East China Sea, Yellow Sea")

predpars <- array(dim=c(length(use_spec), (max(as.numeric(data$End_study_date),na.rm=T)-min(as.numeric(data$End_study_date),na.rm=T)+1), length(unq_par)),
                  dimnames = list(use_spec,seq(from=min(as.numeric(data$End_study_date),na.rm=T),to=max(as.numeric(data$End_study_date),na.rm=T)),unq_par))
x<-1
tiff(file.path(plotdir,"Fig1_LifeHistoryPlots.tiff"),width=6,height=4,res=600,units="in")
par(mfrow=c(2,3),mar=c(.1,3,.1,.1),oma=c(5,0,1,1))
temp<-data[data$Species==use_spec[x] & !is.na(match(data$Large_marine_ecosystem,use_LME)),]
for(y in 1:length(unq_par)) {
  temp2<-temp[temp$Parameter_name==unq_par[y],]
  if(nrow(temp2)>0)
  {
   ymin<-0
   if(min(as.numeric(temp2$Value),na.rm=T)<0)
     ymin<-min(as.numeric(temp2$Value),na.rm=T)
   obs_n<-sum(!is.na(as.numeric(temp2$Value)))
   if(obs_n>3)
   {
    temp2$Value <- as.numeric(temp2$Value)
    temp2$End_study_date <- as.numeric(temp2$End_study_date)
   mod<-gam(Value~s(End_study_date,k=3),data=temp2)
   incol<-'grey'

   styr <- min(as.numeric(temp$End_study_date),na.rm=T)
   endyr <- max(as.numeric(temp$End_study_date),na.rm=T)
   tempnew <- list(End_study_date=seq(from=styr, to = endyr))
   stindex <- styr - min(as.numeric(data$End_study_date),na.rm=T)+1
   endindex <- endyr - min(as.numeric(data$End_study_date),na.rm=T)+1
   preds<- predict(mod,newdata=tempnew,se=TRUE)
   predpars[x,stindex:endindex,y] <- preds$fit

#   plot(mod,residuals=TRUE,cex=4,shade.col=incol,shade=T,xlim=c(1958,2016),xaxt='n',yaxt='n')
   plot(x=styr:endyr, preds$fit,type="l",lwd=2,
        xaxt='n',yaxt='n',xlab=NA,ylab=NA,ylim=range(c(preds$fit+(2*preds$se.fit),preds$fit-(2*preds$se.fit))))
   lines(x=styr:endyr, preds$fit+(2*preds$se.fit), lty=2)
   lines(x=styr:endyr, preds$fit-(2*preds$se.fit), lty=2)
   points(y=temp2$Value, x=temp2$End_study_date, cex=.6, pch=19)
   axis(side=2)
   }else
    plot(0,type='n',xlim=c(1958,2016),xaxt='n',yaxt='n',bty='n')

# HOLY F*K TOOK FOREVER TO GET THIS RIGHT
    legend(legend=bquote(.(letters[y]) * ")" ~~ .(parse(text=parnames[y])[[1]])),
              "topleft",box.col=NA,cex=1.3,bg="transparent")

  }     
  if(nrow(temp2)<1)
    plot(0,type='n',xlim=c(1958,2016),xaxt='n',yaxt='n',bty='n')
  if(y %in% 4:6)
    axis(side=1)
  }

par(mfrow=c(1,1))
mtext(side=1, "Year", line=3.5)
dev.off()

write.csv()
