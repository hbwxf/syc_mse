rm(list=ls())

GeMS.dir <- "C:/GeMS"
Cur.dir<-"C:/syc_mse"

source(file.path(GeMS.dir,"run_GeMS.R"))

#==production model
OMNames<-c("Master_CTL","Master_estInit_CTL")

run_GeMS(GeMSDir=GeMS.dir, CurDir=Cur.dir,
         CreateFolderNameList=OMNames)



#==age structured
OMNames<-c("Static","Time varying")

run_GeMS(GeMSDir=GeMS.dir, CurDir=Cur.dir,
         CreateFolderNameList=OMNames)

#==Lee Qi's stuff

OMNames<-c("SYC_1_noTV_1",'SYC_2_allGAMs_1','SYC_3_changeM_1')
OMNames<-c("SYC_1_noTV_2","SYC_1_noTV_3")
OMNames<-c("SYC_2_allGAMs_2","SYC_2_allGAMs_3")
OMNames<-c("SYC_3_changeM_2","SYC_3_changeM_3")

OMNames<-c("SYC_2_allGAMs_1")
