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
library(readr)


data <- read_excel("Database China Life History.xlsx")

dir.syc <- "~/Box\ Sync/China_LQ/syc_mse"
plotdir<-file.path(dir.syc,"Paper/Figures")

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
unq_par<-c("Linf","k","t0","M","a","b","F")
parnames<-c(expression(L[infinity]),expression(Kappa),expression(t[0]),
            "M", expression(alpha), expression(beta),
            "F",expression(Mat[50]),expression(Mat[95]))

use_LME<-c("East China Sea","Yellow Sea","East China Sea, Yellow Sea")

predpars <- array(dim=c(length(use_spec), 
                        (max(as.numeric(data$End_study_date),na.rm=T)-min(as.numeric(data$End_study_date),na.rm=T)+1),
                        (length(unq_par)+4)),
                  dimnames = list(use_spec,
                                  seq(from=min(as.numeric(data$End_study_date),na.rm=T),to=max(as.numeric(data$End_study_date),na.rm=T)),
                                  c(unq_par,"Mat50","Mat95","L50","L95")))



x<-1
tiff(file.path(plotdir,"Fig1_LifeHistoryPlots.tiff"),width=6,height=6,res=300,units="in")
par(mfrow=c(3,3),mar=c(.1,2.5,.1,.1),oma=c(4,0,1,1))
temp<-data[data$Species==use_spec[x] & !is.na(match(data$Large_marine_ecosystem,use_LME)),]
styr <- min(as.numeric(temp$End_study_date),na.rm=T)
Fstyr<-styr-1
endyr <- max(as.numeric(temp$End_study_date),na.rm=T)

for(y in 1:length(unq_par)) {
  temp2<-temp[temp$Parameter_name==unq_par[y],]
  styr <- min(as.numeric(temp$End_study_date),na.rm=T)
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

   if(unq_par[y]=="F") styr<-styr-1
   tempnew <- list(End_study_date=seq(from=styr, to = endyr))
   stindex <- styr - min(as.numeric(data$End_study_date),na.rm=T)+1
   endindex <- endyr - min(as.numeric(data$End_study_date),na.rm=T)+1
   preds<- predict(mod,newdata=tempnew,se=TRUE)
   predpars[x,stindex:endindex,y] <- preds$fit

#   plot(mod,residuals=TRUE,cex=4,shade.col=incol,shade=T,xlim=c(1958,2016),xaxt='n',yaxt='n')
   plot(0,type='n',xlim=c(Fstyr,endyr),xaxt='n',yaxt='n',xlab=NA,ylab=NA,ylim=range(c(preds$fit+(2*preds$se.fit),preds$fit-(2*preds$se.fit))))
   lines(x=styr:endyr, preds$fit,type="l",lwd=2)
   lines(x=styr:endyr, preds$fit+(2*preds$se.fit), lty=2)
   lines(x=styr:endyr, preds$fit-(2*preds$se.fit), lty=2)
   points(y=temp2$Value, x=temp2$End_study_date, cex=.6, pch=19)
   axis(side=2)
   }else
    plot(0,type='n',xlim=c(styr,endyr),xaxt='n',yaxt='n',bty='n')

# HOLY MOLY TOOK FOREVER TO GET THIS RIGHT
    legend(legend=bquote(.(letters[y]) * ")" ~~ .(parse(text=parnames[y])[[1]])),
              "topleft",box.col=NA,cex=1.3,bg="transparent")

  }     
  if(nrow(temp2)<1)
    plot(0,type='n',xlim=c(styr,endyr),xaxt='n',yaxt='n',bty='n')
  if(y %in% 7:9)
    axis(side=1)
  styr <- min(as.numeric(temp$End_study_date),na.rm=T)
  }

#--------------------
# Maturity Parameters
#--------------------
params <- read_csv(file.path(dir.syc,"Data/NewMaturity.csv"))
newyears <- list(Year=styr:endyr)

A50.gam <- gam(Age~s(Year,k=3), data=params %>% filter(Proportion==0.5))
A50.preds <- predict(A50.gam,newdata=newyears,se=TRUE)

A95.gam <- gam(Age~s(Year,k=3), data=params %>% filter(Proportion==0.95))
A95.preds <- predict(A95.gam,newdata=newyears,se=TRUE)

matparnames<-c(expression(Mat[50]),expression(Mat[95]))
plot(0,type='n',xlim=c(Fstyr,endyr),xaxt='n',yaxt='n',xlab=NA,ylab=NA,ylim=range(c(A50.preds$fit+(2*A50.preds$se.fit),A50.preds$fit-(2*A50.preds$se.fit))))
lines(x=styr:endyr, A50.preds$fit,type="l",lwd=2)
lines(x=styr:endyr, A50.preds$fit+(2*A50.preds$se.fit), lty=2)
lines(x=styr:endyr, A50.preds$fit-(2*A50.preds$se.fit), lty=2)
points(y=filter(params,Proportion==0.5)$Age, x=filter(params,Proportion==0.5)$Year, cex=.6, pch=19)
legend(legend=bquote(.(letters[y+1]) * ")" ~~ .(parse(text=parnames[y+1])[[1]])),
              "topleft",box.col=NA,cex=1.3,bg="transparent")
axis(side=1)
axis(side=2)

plot(0,type='n',xlim=c(Fstyr,endyr),xaxt='n',yaxt='n',xlab=NA,ylab=NA,ylim=range(c(A95.preds$fit+(2*A95.preds$se.fit),A95.preds$fit-(2*A95.preds$se.fit))))
lines(x=styr:endyr, A95.preds$fit,type="l",lwd=2)
lines(x=styr:endyr, A95.preds$fit+(2*A95.preds$se.fit), lty=2)
lines(x=styr:endyr, A95.preds$fit-(2*A95.preds$se.fit), lty=2)
points(y=filter(params,Proportion==0.95)$Age, x=filter(params,Proportion==0.95)$Year, cex=.6, pch=19)
legend(legend=bquote(.(letters[y+2]) * ")" ~~ .(parse(text=parnames[y+2])[[1]])),
              "topleft",box.col=NA,cex=1.3,bg="transparent")
axis(side=1)
axis(side=2)


par(mfrow=c(1,1))
mtext(side=1, "Year", line=2.5)
dev.off()


stindex <- styr - min(as.numeric(data$End_study_date),na.rm=T)+1
endindex <- endyr - min(as.numeric(data$End_study_date),na.rm=T)+1

predpars[1,stindex:endindex,(y+1)] <- A50.preds$fit
predpars[1,stindex:endindex,(y+2)] <- A95.preds$fit


#-----------------------
# Selectivity Parameters
#-----------------------
seldat <- read_csv(file.path(dir.syc,"Data/Selectivity.csv"))

MeshSeries <- list(Mesh_mm=seq(from=80, to=40, length=(endyr-(styr-1)+1)))

L50.gam <- gam(L50_cm~s(Mesh_mm,k=3), data=seldat)
L50.preds <- predict(L50.gam,newdata=MeshSeries,se=TRUE)

L95.gam <- gam(L95_cm~s(Mesh_mm,k=3), data=seldat)
L95.preds <- predict(L95.gam,newdata=MeshSeries,se=TRUE)

stindex <- stindex-1

predpars[1,stindex:endindex,(y+3)] <- L50.preds$fit
predpars[1,stindex:endindex,(y+4)] <- L95.preds$fit

write.csv(predpars[1,,],file.path(dir.syc,"Data/SYC_GAMoutput.csv"))





