# Figure out selectivity parameters
# Assume mesh size decreases linearly over time
# Beginning mesh size largest from studies
# Doesn't take into account 

library(tidyverse)
library(mgcv)
seldat <- read_csv(file.path(dir.syc,"Data/Selectivity.csv"))

MeshSeries <- list(Mesh_mm=seq(from=80, to=40, length=50))

L50.gam <- gam(L50_cm~s(Mesh_mm,k=3), data=seldat)
L50.preds <- predict(L50.gam,newdata=MeshSeries,se=TRUE)

L95.gam <- gam(L95_cm~s(Mesh_mm,k=3), data=seldat)
L95.preds <- predict(L95.gam,newdata=MeshSeries,se=TRUE)
