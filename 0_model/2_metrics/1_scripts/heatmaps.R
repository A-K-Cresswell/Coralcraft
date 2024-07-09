


#### To create heat maps


##############################################
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
library(ggpubr)

colpal = c("firebrick4", "navy", "plum4",
           "tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "skyblue3", "orchid1","orange")

## Read metrics code before proceeding
work.dir = paste("~/1_Writing/1_Chapter 1_Coralcraft/00_model")
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
script = paste(met.wd,"1_MetricsScripts", sep="/") 
output = paste(met.wd, "1_Outputs", sep="/") 

#simpsons ----
simp = function(x) 1- (x(x-1) / sum(x)(sum(x)-1))
simp = function(x) {
  N=sum(x)
  1- sum (   x*(x-1) / (N*(N-1)))
}
simp2 = function(x) {
  N=sum(x)
  1- sum (   (x / N)^2)
}
# use simp2 

#####################################################################################################

#### read in scenario file ####
setwd(work.dir)
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth forms.csv")

# # to select growth forms from scenarios (needed for simulation code)
# sel.fts <- scens.csv[scens] 
# sel.fts <- as.numeric(na.omit(as.vector(unlist(sel.fts))))
# labels = as.vector(subset(fts, id %in% sel.fts))
###################################################################################################

### Load Individual Scenarios ####
sc.lab = as.vector(subset(scens.id, scenario %in% c("all.10"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-03", "v2", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

timesteps = 52*5
run = 1
ws=100

load(file = paste(id, scens, "worlds",timesteps,run, sep = "_")) ## change ts & run according

image(world[,,1])
world11 = world==5
image(apply(world11,c(1,2),any ))

dim(world)
dim(dead)
world2 = world > 0 | dead > 0
dim(world2)
image(world2[,,1])

world2[11,40,]

image(apply(world2,c(1,2),any ))

which(apply(world2,c(1,2),any ), arr.ind=T)
sum(cumsum(world2[1,44,])==0)

countcoveredpixels = function(slice){
  covered = 0
  if (any(slice)) covered = sum(cumsum(slice)==0)
  covered
}

countcoveredpixels(world2[1,44,])

allcovered = apply(world2,c(1,2),countcoveredpixels )
image(allcovered)

#################################################

### covered cells from sides

countcoveredpixels2 = function(slice){
  covered = 0
  if (any(slice)) {
    slicei = which(rev(slice))
    top = max(slicei)
    covered = sum(!rev(slice)[1:top])
  }
  covered
}


### Heat maps for side view ----
par(mfrow=c(2,3))
image(world2[,,1], main="covered @ lvl 1")
allcovered = apply(world2,c(1,2),countcoveredpixels )
image(allcovered, main="sheltered pixels")
az=which(world2[,,1],arr.ind=T)
#points(az[,1]/100,az[,2]/100,pch=16,cex=0.1)
allcovered = apply(world2,c(1,3),countcoveredpixels )
image(allcovered, main = "sideview N-S")
allcovered = apply(world2,c(1,3),countcoveredpixels2 )
image(allcovered, main = "sideview S-N")
allcovered = apply(world2,c(2,3),countcoveredpixels )
image(allcovered, main = "sideview E-W")
allcovered = apply(world2,c(2,3),countcoveredpixels2 )
image(allcovered, main = "sideview W-E")

getnumcoveredcellssides = function(run,ts){
  print(ts)
  world2 = world > 0 | dead > 0
  allcovered = apply(world2,c(2,3),countcoveredpixels2 )
  c1 = sum(allcovered)
  allcovered = apply(world2,c(1,3),countcoveredpixels )
  c2 = sum(allcovered)
  allcovered = apply(world2,c(1,3),countcoveredpixels2 )
  c3 = sum(allcovered)
  allcovered = apply(world2,c(2,3),countcoveredpixels )
  c4 = sum(allcovered)
  (c1+c2+c3+c4)/4
}
######################################################


#Visibility - top view

     = array(0,c(ws,ws,ws))

visibility = array(0,c(ws,ws,ws))
visibility[,,ws]=1
for (layer in ws:2){
  newlayer  = visibility[,,layer]
  newlayer[world2[,,(layer-1)]]=0  
  newlayer= rbind(newlayer[2:ws,],newlayer[1,])
  visibility[,,(layer-1)]=newlayer
}
visibility2 = visibility2 + visibility

visibility = array(0,c(ws,ws,ws))
visibility[,,ws]=1
for (layer in ws:2){
  newlayer  = visibility[,,layer]
  newlayer[world2[,,(layer-1)]]=0  
  newlayer= rbind(newlayer[ws,],newlayer[1:(ws-1),])
  visibility[,,(layer-1)]=newlayer
}
visibility2 = visibility2 + visibility

visibility = array(0,c(ws,ws,ws))
visibility[,,ws]=1
for (layer in ws:2){
  newlayer  = visibility[,,layer]
  newlayer[world2[,,(layer-1)]]=0  
  newlayer= cbind(newlayer[,2:ws],newlayer[,1])
  visibility[,,(layer-1)]=newlayer
}
visibility2 = visibility2 + visibility

visibility = array(0,c(ws,ws,ws))
visibility[,,ws]=1
for (layer in ws:2){
  newlayer  = visibility[,,layer]
  newlayer[world2[,,(layer-1)]]=0  
  newlayer= cbind(newlayer[,ws],newlayer[,1:(ws-1)])
  visibility[,,(layer-1)]=newlayer
}
visibility2 = visibility2 + visibility

visibility = array(0,c(ws,ws,ws))
visibility[,,ws]=1
for (layer in ws:2){
  newlayer  = visibility[,,layer]
  newlayer[world2[,,(layer-1)]]=0  
  visibility[,,(layer-1)]=newlayer
}
visibility2 = visibility2 + visibility

visibility2 = visibility2 /5

###################################################
sw=0.5
visibility = array(0,c(ws,ws,ws))
visibility[,,ws]=1
for (layer in ws:2){
  newlayer  = visibility[,,layer]
  newlayer[world2[,,(layer-1)]]=0  
  sideways = newlayer * sw
  newlayer = newlayer - sideways
  thismove = rbind(sideways[2:ws,],sideways[1,])
  newlayer = newlayer + 0.25*thismove
  thismove = rbind(sideways[ws,],sideways[1:(ws-1),])
  newlayer = newlayer + 0.25*thismove
  thismove = cbind(sideways[,2:ws],sideways[,1])
  newlayer = newlayer + 0.25*thismove
  thismove = cbind(sideways[,ws],sideways[,1:(ws-1)])
  newlayer = newlayer + 0.25*thismove
  visibility[,,(layer-1)]=newlayer
}

bylayer = apply(visibility,3,sum)
plot(bylayer)
bylayer = apply(visibility2,3,sum)
lines(bylayer,col='red')

par(mfrow=c(2,4))

image(visibility[,,1])
image(visibility[,,5])
image(visibility[,,10])
image(visibility[,,15])
image(visibility2[,,1])
image(visibility2[,,5])
image(visibility2[,,10])
image(visibility2[,,15])

sheltered = 1-visibility
sheltered[world2] = 0
sheltered2 = 1-visibility2
sheltered2[world2] = 0

mx = max(sheltered[,,1])
image(sheltered[,,1],zlim=c(0,mx))
image(sheltered[,,5],zlim=c(0,mx))
image(sheltered[,,10],zlim=c(0,mx))
image(sheltered[,,12],zlim=c(0,mx))
image(sheltered2[,,1],zlim=c(0,mx))
image(sheltered2[,,5],zlim=c(0,mx))
image(sheltered2[,,10],zlim=c(0,mx))
image(sheltered2[,,12],zlim=c(0,mx))

### shelter v height
shh=apply(sheltered,3,mean)
plot(shh,t='l')
shh2=apply(sheltered2,3,mean)
lines(shh,col='red')

mean(sheltered[,,1])  ## percnt of shelter at the bottom
mean(sheltered) ## proportion of all cells sheltered