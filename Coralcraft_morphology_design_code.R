# Coralcraft - A functional-structural coral model
# Code for the design of coral morphologies

# Authors
# Michael Renton
# Anna K Cresswell

# For more information contact annacresswell@gmail.com

# Appendix B of Cresswell et al. 2019, "Modelling the effects of hydrodynamic disturbance intensity and frequency on the morphological diversity and structural complexity of coral communities"

#library(rgl)

# This script is a source file. It introduces the possible morphologies, and, based on the morphologies, specifies the possible cells the coral may grow into, as referenced to a starting cell.

#----------------------------------------------------------------------------------------
ws = 100
ftcelllist=list() # make an empty list
allcells = data.frame(expand.grid(x=(-ws/2):(ws/2),y=(-ws/2):(ws/2),z=1:ws)) #ws/2 puts in the middle of the world
head(allcells)

allcells$dist1 = sqrt((allcells$x)^2 + (allcells$y)^2 + (allcells$z)^2)
allcells$dist2 = sqrt((allcells$x)^2 + (allcells$y)^2)

### 1. Encrusting
thiscelllist = subset(allcells , z==1)
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[1]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 2. Hemispherical
thiscelllist = allcells 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[2]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 3. Tabular
thiscelllist = subset(allcells, (dist2 <= 3 & z<12) | z==12 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[3]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 4. Branching
ok = allcells$dist2 < 1.5
for (bp in c(10,20,30,40,50)){
  z2 = allcells$z - bp
  ok2 = sqrt((allcells$x+(z2))^2 + (allcells$y+(z2))^2) <1.5  & z2>0
  ok3 = sqrt((allcells$x-(z2))^2 + (allcells$y+(z2))^2) <1.5  & z2>0
  ok4 = sqrt((allcells$x+(z2))^2 + (allcells$y-(z2))^2) <1.5  & z2>0
  ok5 = sqrt((allcells$x-(z2))^2 + (allcells$y-(z2))^2) <1.5  & z2>0
  ok = (ok|ok2|ok3|ok4|ok5)
}
thiscelllist = subset(allcells, ok ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[4]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 5. Corymbose
ok = allcells$dist2 < 1.6
ang1 = 2*pi/5
for (ang in seq(ang1 ,2*pi,by=ang1 )){
  dd = allcells$z * tan(pi/8)
  x11 = dd * sin(ang) 
  y11 = dd * cos(ang) 
  ok2 = sqrt((allcells$x-x11)^2 + (allcells$y-y11)^2) < 1.5
  ok = (ok|ok2)
}
ang1 = 2*pi/9
for (ang in seq(ang1/3 ,2*pi,by=ang1 )){
  dd = allcells$z * tan(pi/4)
  x11 = dd * sin(ang) 
  y11 = dd * cos(ang) 
  ok2 = sqrt((allcells$x-x11)^2 + (allcells$y-y11)^2) < 1.5
  ok = (ok|ok2)
}
ang1 = 2*pi/13
for (ang in seq(2*ang1/3 ,2*pi,by=ang1 )){
  dd = allcells$z * tan(3*pi/8)
  x11 = dd * sin(ang) 
  y11 = dd * cos(ang) 
  ok2 = sqrt((allcells$x-x11)^2 + (allcells$y-y11)^2) < 1.5
  ok = (ok|ok2)
}
thiscelllist = subset(allcells, ok ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[5]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

#* More colony shapes have and can be designed here. Please contact authors for more information on other morphologies explored so far.


### trim to a maximum radius of 49
for (i in 1:length(ftcelllist)){
  ftcelllist[[i]] = subset(ftcelllist[[i]], pr<49)
}

library(Matrix)
#### turn into arrays
for (i in 1:length(ftcelllist)){
  m1 = array(0,dim=c(ws,ws,ws))
  inds = as.matrix((ftcelllist[[i]])[,1:3])
  inds[,1:2] = inds[,1:2] + ws/2
  m1[inds] = 999-ftcelllist[[i]]$pr
  ftcelllist[[i]] = m1
}


# Plot each for in rgl at set radius
#colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet")

# for (i in (1:5)) {
# tcl = ftcelllist[[i]] # choose one to plot
# max(tcl)
# inds = which(tcl>960,arr.ind=TRUE) # if limit to >960, will have a radius of ~40
# inds
# open3d()
# spheres3d(inds [,1],inds [,2],inds [,3], col=colpal[i])#,alpha=0.9,coralpolyps$sz*0.7)
# }
save(ftcelllist,file="ftcelllist")
# *note you can use your mouse to move around the colony in the rgl window





