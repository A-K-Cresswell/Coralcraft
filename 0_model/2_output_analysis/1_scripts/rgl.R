

# RGL Scripts

# world with colony id
# world with dead cells 
# world with colonies + dead cells


#############################
timesteps = 52*5
runs = 100
ws=100

library(animation)
library(scatterplot3d)
library(rgl)
library(magick)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(png)
library(raster)

## Read metrics code before proceeding
work.dir = paste("~/GitHub/Coralcraft")
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
script = paste(met.wd,"1_MetricsScripts", sep="/") 
output = paste(met.wd, "1_Outputs", sep="/") 

#### read in scenario file ####
setwd(work.dir)
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth forms.csv")

#set new working directories for specific scenario
sc.lab = as.vector(subset(scens.id, scenario %in% c("slo"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-01-20", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

# Plot annual alive / dead colony with rgl
for (ts in seq(52, timesteps, by = 52)){
  print (ts)
  load(file = paste("worlds",ts,runs))
  open3d()
  loc = which(world>0,arr.ind = TRUE) # put world == ft if just want to plot one type
  ids = world[which(world>0)]
  ifdead = dead[which(world>0)]
  spheres3d(loc[,1],loc[,2],loc[,3],color=c("green", "black")[ifdead +1])
}

##################################################################################


# Plot final world colonyid with rgl ----
open3d()
#uM <- load(file = "rgl rotation.rdata")
#par3d(userMatrix = uM)
loc = which(world > 0,arr.ind = TRUE) # put world == ft if just want to plot one type
ids = world[which(world>0)]
ifdead = dead[which(world>0)]
# ftss = unlist(sapply(ids , function(thisid) {
#   colonymap$ft[colonymap$colonyid == thisid]
# }))
spheres3d(loc[,1],loc[,2],loc[,3],color=rainbow((length(unique(ids))))[ids])# ,color=colpal[ftss]#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
# par3d(userMatrix = uM3, zoom=1)
# shade3d(qmesh3d(verticesfloor, indices), color='black')
# shade3d(qmesh3d(verticesback, indices), color='darkgrey')
# shade3d(qmesh3d(verticesside, indices), color='grey')
# 
# rgl.snapshot(paste(version,"colonyid.png"))


# Plot dead world with rgl ----
open3d()
#uM <- load(file = "rgl rotation.rdata")
#par3d(userMatrix = uM)
loc = which(dead == 1,arr.ind = TRUE) # put world == ft if just want to plot one type
ids = world[which(dead == 1)]
ifdead = dead[which(dead == 1)]
# ftss = unlist(sapply(ids , function(thisid) {
#   colonymap$ft[colonymap$colonyid == thisid]
# }))
spheres3d(loc[,1],loc[,2],loc[,3],color="grey")# ,color=colpal[ftss]#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
# par3d(userMatrix = uM3, zoom=1)
# shade3d(qmesh3d(verticesfloor, indices), color='black')
# shade3d(qmesh3d(verticesback, indices), color='darkgrey')
# shade3d(qmesh3d(verticesside, indices), color='grey')

# Plot alive / dead colony with rgl
open3d()
#uM <- load(file = "rgl rotation.rdata")
#par3d(userMatrix = uM)
loc = which(world>0,arr.ind = TRUE) # put world == ft if just want to plot one type
ids = world[which(world>0)]
ifdead = dead[which(world>0)]
# ftss = unlist(sapply(ids , function(thisid) {
#   colonymap$ft[colonymap$colonyid == thisid]
# }))
spheres3d(loc[,1],loc[,2],loc[,3],color=c("green", "black")[ifdead +1])#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
# par3d(userMatrix = uM3, zoom=1)
# shade3d(qmesh3d(verticesfloor, indices), color='black')
# shade3d(qmesh3d(verticesback, indices), color='darkgrey')
# shade3d(qmesh3d(verticesside, indices), color='grey')
rgl.snapshot(paste(version,"colonyid_1.png"))





