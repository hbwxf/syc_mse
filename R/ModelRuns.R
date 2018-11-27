#cd Box\ Sync/GeMS/General-MSE/syc_mse
#rt

MSEdir <- MSEDir <- CurDir <- getwd()

library(GeMS)

###########

CTLNameList<-CTLName<-CTLNames<-CreateFolderName<-CreateFolderNameList<-c("SYC_1_noTV_1")
CTLNameList<-CTLName<-CTLNames<-CreateFolderName<-CreateFolderNameList<-c("SYC_2_allGAMs_1")
CTLNameList<-CTLName<-CTLNames<-CreateFolderName<-CreateFolderNameList<-c("SYC_3_changeM_1")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
run_GeMS(CreateFolderNameList,MSEdir,silent=T)


#########

CreateFolderNameList<-c("SYC_1_noTV_2","SYC_2_allGAMs_2","SYC_3_changeM_2")
CreateFolderNameList<-c("SYC_1_noTV_3","SYC_2_allGAMs_3","SYC_3_changeM_3")
run_GeMS(CreateFolderNameList,runparallel=T,cores=3,silent=T)

out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1CreateFolderNameList,MSEdir,
                      plotNames=c("Time-Invariant",
                                  "Increasing M", 
                                  "Decreasing M"))

#########

prodCTLNames<-c("SYC_1_noTV_1",
                "SYC_2_allGAMs_1",
                "SYC_3_changeM_1")
run_GeMS(prodCTLNames,runparallel=T,cores=3,silent=T)

out <- Inout <- ReadCTLfile(prodCTLNames[1])
ProductionModelOutput(Inout,prodCTLNames,MSEdir,
                      plotNames=c("Time-Invariant",
                                  "Increasing M",
                                  "Decreasing M"))

######################
# Age-structured plots
######################

CTLNames <- CreateFolderNameList<-c("SYC_1_noTV_2","SYC_1_noTV_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

CTLNames <- CreateFolderNameList<-c("SYC_2_allGAMs_2","SYC_2_allGAMs_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

CTLNames <- CreateFolderNameList<-c("SYC_3_changeM_2","SYC_3_changeM_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                 plotNames=c("Fixed M", "Estimated M"),
                 Nruns=5)

CTLNames<-CTLNameList<-CreateFolderNameList<-c("SYC_1_noTV_2","SYC_1_noTV_3",
            "SYC_2_allGAMs_2","SYC_2_allGAMs_3",
            "SYC_3_changeM_2","SYC_3_changeM_3")
out <- Inout <- ReadCTLfile(CreateFolderNameList[1])
AgeStructureComp(Inout,RetroPeels=1,CreateFolderNameList,MSEdir,
                plotNames=c("Time-Invariant;\nFixed M", "Time-Invariant;\nEstimated M",
                            "Increasing M;\nFixed M", "Increasing M;\nEstimated M",
                            "Decreasing M;\nFixed M", "Decreasing M;\nEstimated M"),
                Nruns=5,plottiff=F)




