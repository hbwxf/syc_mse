rm(list=ls())

GeMS.dir <- "C:/GeMS"
Cur.dir<-"C:/syc_mse/"

source(file.path(GeMS.dir,"run_GeMS.R"))

#==production model
OMNames<-c("Master_CTL","Master_estInit_CTL")

run_GeMS(GeMSDir=GeMS.dir, CurDir=Cur.dir,
         CreateFolderNameList=OMNames)



#==age structured
OMNames<-c("Static","Time varying")

run_GeMS(GeMSDir=GeMS.dir, CurDir=Cur.dir,
         CreateFolderNameList=OMNames)
