# Coralcraft - A functional-structural coral model
# Code for design of coral morphologies

# Authors
# Michael Renton
# Anna K Cresswell
# Daphne Oh

library(rgl)
setwd("~/GitHub/Coralcraft") #set work directory here

# This script is a source file. It introduces all possible functional forms, and, based on the functional type, specifies the possible cells the functional type may grow into, as referenced to a starting cell.

#----------------------------------------------------------------------------------------
ws = 100

ftcelllist=list() # make an empty list
#allcells = data.frame(expand.grid(x=(-ws/2):(ws/2),y=(-ws/2):(ws/2),z=1:depth)) #ws/2 puts in the middle of the world
allcells = data.frame(expand.grid(x=(-ws/2):(ws/2),y=(-ws/2):(ws/2),z=1:ws))
head(allcells)

allcells$dist1 = sqrt((allcells$x)^2 + (allcells$y)^2 + (allcells$z)^2) 
allcells$dist2 = sqrt((allcells$x)^2 + (allcells$y)^2) #radius

### 1. encrusting
thiscelllist = subset(allcells , z==1)
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[1]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 2. flexi - hemispherical
r=1.5
thiscelllist = allcells 
thiscelllist$pr = sqrt(thiscelllist$x^2 + thiscelllist$y^2 + r * thiscelllist$z^2)
ftcelllist[[2]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 3. digitate
r=2
thiscelllist = subset(allcells, (x%%4 %in% 0:1 & y%%4 %in% 0:1) | z==1 ) #grid of colony
thiscelllist$pr = sqrt(thiscelllist$x^2 + thiscelllist$y^2 + r * thiscelllist$z^2) #shape of single colony
ftcelllist[[3]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 4. corymbose 
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
hedgehogforlater = thiscelllist
ftcelllist[[4]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 5. tabular
thiscelllist = subset(allcells, (dist2 <= 3 & z<12) | (z==12 & dist2 <= 10 )  | z==13 | z==14 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[5]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 6. mushroom
thiscelllist = subset(allcells, (dist2 <= 10 & z<10) | (z>=10) )
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[6]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

### 7. columnar
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
thiscelllist = subset(allcells, ok & z <= 10 ) #adjusts height of base
slice = subset(thiscelllist,z==10) #slices where base grows upright
for (nz in 11:50) { #vertical growth of coral after slice
  newslice = slice
  newslice$z = nz
  thiscelllist = rbind(thiscelllist,newslice)
}
thiscelllist$pr = sqrt((thiscelllist$x)^2 + (thiscelllist$y)^2 + (thiscelllist$z)^2) 
ftcelllist[[7]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 8. foliose
thiscelllist = NULL
gapsize = 8
ok = allcells$x==0 & allcells$y==0 & allcells$z==0
for (ddd in seq(0,24,by=gapsize )) {
  ok2 = with(allcells,  (abs(dist2-z-ddd)<=1 & z<(ddd+6) & z>1) | (z==1 & dist2<=ddd & dist2>(ddd-gapsize))  ) 
  ok = ok | ok2
}
thiscelllist = subset(allcells, ok )
thiscelllist = subset(thiscelllist , x > (-50) & y > (-50))
thiscelllist$pr = (thiscelllist$dist2)
ftcelllist[[8]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

head(thiscelllist )
  m1 = array(0,dim=c(ws,ws))
  aslice= (subset(thiscelllist,x==0))
  inds = as.matrix(aslice[,2:3])
  inds[,1] = inds[,1] + ws/2
  m1[inds] = 999-aslice$pr
image(log(m1))

### 9. bushy
thiscelllist = ftcelllist[[4]]
thiscelllist$z=thiscelllist$z+6
thiscelllist=subset(thiscelllist,z<=100)
slice = subset(thiscelllist,z==7)
for (nz in 1:6){
  newslice=slice
  newslice$z=nz
  thiscelllist=rbind(thiscelllist,newslice)
}
thiscelllist$pr = with(thiscelllist,sqrt(x^2+y^2+z^2))
ftcelllist[[9]] = thiscelllist  
dim(thiscelllist)

### 10. branching 
ok = allcells$dist2 < 1.5
ang = 0
for (bp in seq(5,60,by=3)){
  z2 = allcells$z - bp
  ang = ang + 4
  for (rrr in 1:30){
    xx = rrr * cos(ang)
    yy = rrr * sin(ang)
    zz = rrr * sin(pi/4) + bp
    ok2 = sqrt( (allcells$x-xx)^2 + (allcells$y-yy)^2 + (allcells$z-zz)^2) <1.5
    ok = ok|ok2
  }
}
thiscelllist = subset(allcells, ok ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[10]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### trim to a maximum radius of 40
for (i in 1:length(ftcelllist)){
  ftcelllist[[i]] = subset(ftcelllist[[i]], pr<50)
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

# i = 8
# tcl = ftcelllist[[i]]
# min(tcl[tcl>0])
# max(tcl)
# inds = which(tcl>0,arr.ind=TRUE) 
# open3d()
# spheres3d(inds [,1],inds [,2],inds [,3], col=colpal[i])

#par(mfrow=c(2,2))
#for (i in 7:10){
#tcl = ftcelllist[[i]]
#plot(tcl[tcl>0],ylim=c(940,1000))
#image(log(tcl[50,,]))
#}
tcl = ftcelllist[[8]]
#plot(tcl[tcl>0],ylim=c(940,1000))
image(log(tcl[49,,]))

# 
# tcl = ftcelllist[[5]]
# max(tcl)
# inds = which(tcl>1010,arr.ind=TRUE) # if limit to >960, will have a radius of ~40
# inds
# spheres3d(inds [,1],inds [,2],inds [,3], col='red')#,alpha=0.9,coralpolyps$sz*0.7)

# # set the working directory to save individual coral growth forms
# setwd("C:/Users/OH012/OneDrive - CSIRO/Documents/GitHub/Coralcraft/Coral Morphologies")
# 
# Plot each for in rgl at set radius (you will need to load the rgl package to do this)
colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "cadetblue1", "orchid1","orange")

for (i in (1:10)) {
 tcl = ftcelllist[[i]] # choose one to plot
 max(tcl)
 inds = which(tcl>920,arr.ind=TRUE) # if limit to >960, will have a radius of ~40
 inds
 open3d()
 spheres3d(inds [,1],inds [,2],inds [,3], col=colpal[i])#,alpha=0.9,coralpolyps$sz*0.7)
 #rgl.snapshot(paste(i,".png"))
}

save(ftcelllist,file="ftcelllist")

