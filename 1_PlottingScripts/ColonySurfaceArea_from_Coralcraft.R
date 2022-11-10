library(cowplot)
## Functional-structural coral model (FSCM) ---- NextGen
## Last modified 21/9/18 MR

# For export naming ----
#version = "FSCM - ng011027"
#version = "FSCM - ng181003"
version = paste(Sys.Date())

# working directorys ----
work.dir=("C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael") # Base direction For Anna PC
#work.dir=".." # Base directory For Michael
model = paste(work.dir,"1_Model Scripts", sep="/")
plots = paste(work.dir,"1_Plots", sep="/")
output = paste(work.dir, "1_outputs", sep="/")


#### colonyid ft age    res row col bd stillalive
library(rgl)

# This script is a source file. It introduces all possible functional forms, and, based on the functional type, specifies the possible cells the functional type may grow into, as referenced to a starting cell.
ws = 100
#----------------------------------------------------------------------------------------
ftcelllist=list() # make an empty list
allcells = data.frame(expand.grid(x=(-ws/2):(ws/2),y=(-ws/2):(ws/2),z=1:ws)) #ws/2 puts in the middle of the world
head(allcells)

allcells$dist1 = sqrt((allcells$x)^2 + (allcells$y)^2 + (allcells$z)^2)
allcells$dist2 = sqrt((allcells$x)^2 + (allcells$y)^2)

### 1. encrusting
thiscelllist = subset(allcells , z==1)
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[1]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

# 2. Hemisphere
thiscelllist = allcells 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[2]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 3. flexi - hemisphere
r=2
thiscelllist = allcells 
thiscelllist$pr = sqrt(thiscelllist$x^2 + thiscelllist$y^2 + r * thiscelllist$z^2)
ftcelllist[[3]] = thiscelllist[-c(4:5)] 
dim(thiscelllist)

### 4. column
thiscelllist = subset(allcells, dist2 <= 4) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[4]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 5. cone
thiscelllist = subset(allcells, abs(dist2-z)<=0.75 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[5]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 7. short tabular
thiscelllist = subset(allcells, (dist2 <= 3 & z<8) | z==8 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[6]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 6. medium tabular
thiscelllist = subset(allcells, (dist2 <= 3 & z<12) | z==12 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[7]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 8. tall tabular 
thiscelllist = subset(allcells, (dist2 <= 3 & z<16) | z==16 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[8]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 9. branching
ok1 = allcells$dist2 < 2
ok2 = sqrt((allcells$x+(allcells$z-10))^2 + (allcells$y+(allcells$z-10))^2) <2  & allcells$z >=10
ok3 = sqrt((allcells$x-(allcells$z-10))^2 + (allcells$y+(allcells$z-10))^2) <2  & allcells$z >=10
ok4 = sqrt((allcells$x+(allcells$z-10))^2 + (allcells$y-(allcells$z-10))^2) <2  & allcells$z >=10
ok5 = sqrt((allcells$x-(allcells$z-10))^2 + (allcells$y-(allcells$z-10))^2) <2  & allcells$z >=10
thiscelllist = subset(allcells, (ok1|ok2|ok3|ok4|ok5) ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[9]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 10. branching 2
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
ftcelllist[[10]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 11. branching 3
ok = (allcells$x == 0 & allcells$y == 0)
ok1 = (allcells$y == 0 & allcells$x == allcells$z)
ok2 = (allcells$x == 0 & allcells$y == allcells$z)
ok3 = (allcells$y == 0 & allcells$x == -allcells$z)
ok4 = (allcells$x == 0 & allcells$y == -allcells$z)
ok = ok | ok1 |ok2 | ok3 | ok4
ok1 = (allcells$y == 0 & allcells$x == allcells$z-1) # also need the z = -1 because otherwise branches will be too skinny. Also it won't look for neighbours that are directly at 45 degrees, must be thick enough to have an option next to it.
ok2 = (allcells$x == 0 & allcells$y == allcells$z-1)
ok3 = (allcells$y == 0 & allcells$x == -allcells$z-1)
ok4 = (allcells$x == 0 & allcells$y == -allcells$z-1)
ok = ok | ok1 |ok2 |ok3|ok4
thiscelllist = subset(allcells, ok) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[11]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 12. digitate
thiscelllist = subset(allcells, (x%%4==0 & y%%4==0) | z==1 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[12]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 13. digitate2 / waffle says libby
thiscelllist = subset(allcells, (x%%4==0 & y%%4==0) | (z==1 & (x%%4==0 | y%%4==0))) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[13]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 14. hedgehog
ok = allcells$dist2 < 2
ang1 = 2*pi/7
for (ang in seq(ang1 ,2*pi,by=ang1 )){
	dd = allcells$z * tan(pi/3)
	x11 = dd * sin(ang) 
	y11 = dd * cos(ang) 
	ok2 = sqrt((allcells$x-x11)^2 + (allcells$y-y11)^2) < 2
	ok = (ok|ok2)
}
for (ang in seq(ang1/2 ,2*pi,by=ang1 )){
	dd = allcells$z * tan(pi/6)
	x11 = dd * sin(ang) 
	y11 = dd * cos(ang) 
	ok2 = sqrt((allcells$x-x11)^2 + (allcells$y-y11)^2) < 2
ok = (ok|ok2)
}
thiscelllist = subset(allcells, ok ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[14]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)


### 15. hedgehog 2
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
ftcelllist[[15]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### trim to a maximum radius of 40
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

# Plotting
setwd(model)


colpal = c("tomato", "gold", "#7FC97F", "#BEAED4" , "#FDC086", "#FFFF99" , "springgreen4", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "blueviolet", "grey")


library(rgl)
# plotting parameters ----
r3dDefaults$windowRect = c(50,50,700,700) # increase size of rgl window for better resolution when saving

# To save with new orientation of rgl window:
# Open rgl and move to desired orientation then save with below
# uM <- par3d()$userMatrix
# write.csv(uM, "uM_sideview.csv", row.names = F) # should only need to do this once
uM2 <- read.csv("uM.csv") # use pre-saved rgl orientation
uM2 <- data.matrix(uM2) # use pre-saved rgl orientation

uM3 <- read.csv("uM_topview.csv") # use pre-saved rgl orientation
uM3 <- data.matrix(uM3) # use pre-saved rgl orientation

uM4 <- read.csv("uM_sideview.csv") # use pre-saved rgl orientation
uM4 <- data.matrix(uM4) # use pre-saved rgl orientation

# Plot the forms in rgl ----
setwd(plots)
for (ft in 1:length(ftcelllist)){
  thisarray = ftcelllist[[ft]]
  tsize=30
  thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE) 
  par3d(userMatrix = uM2, zoom=0.6)
  rgl.lines(c(100, 100), c(100, 100), c(0,50), color = "white")
  rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
  rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
  spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color=colpal[ft])#,alpha=0.9,coralpolyps$sz*0.7)
  rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}


setwd(plots)

names = c("a) Encrusting", "b) Massive", "c) Submassive", 
          "d) Columnar", "e) Laminar", "f) Tabular (short)",
          "g) Tabular (medium)", "h) Tabular (tall)", "i) Arborescent",
          "j) Caespitose", "k) Branching?", "l) Digitage fused",
          "m) Digitage grid", "n) Corymbose 1", "o) Corymnose 2")
#for (ft in 1:length(ftcelllist)){
for (ft in c(1,2,3,4,5,9,10,11,14,15)){
  thisarray = ftcelllist[[ft]]
  tsize=30
  thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE)  ##
  #rot = ifelse(ft %in% c(1,2,3,4,5,9,10,11,14,15), uM2, ifelse(ft %in% c(13,14), uM3, uM4))
  # rot = if(ft %in% c(1,2,3,4,5,9,10,11,14,15)) uM2
  # rot = if(ft %in% c(13,14)) uM3
  # rot = if(ft %in% c(6,7,8)) uM4
  par3d(userMatrix = uM2, zoom=0.6)
  rgl.lines(c(100, 100), c(100, 100), c(0,50), color = "white")
  rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
  rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
  spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color=colpal[ft])#,alpha=0.9,coralpolyps$sz*0.7)
  #text3d(50,50,45,paste(names[ft]), color = "black", cex = par3d(3), font = 5)
  rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}


for (ft in c(6,7,8)){
  thisarray = ftcelllist[[ft]]
  tsize=30
  thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE)  ##
  par3d(userMatrix = uM4, zoom=0.6)
  rgl.lines(c(100, 100), c(100, 100), c(0,50), color = "white")
  rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
  rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
  spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color = colpal[ft])#color = rainbow(length(ftcelllist))[ft])#,alpha=0.9,coralpolyps$sz*0.7)
  #  text3d(50,50,40,paste(names[ft]), color = "black", cex = par3d(3), font = 5)
  rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}
# how to specify x limits so all cells will be the same size?

for (ft in c(12,13)){
  thisarray = ftcelllist[[ft]]
  tsize=30
  thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE)  ##
  par3d(userMatrix = uM3, zoom=0.6)
  rgl.lines(c(100, 100), c(100, 100), c(-15,35), color = "white")
  rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
  rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
  spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color=colpal[ft])#,alpha=0.9,coralpolyps$sz*0.7)
  # text3d(50,50,55,paste(names[ft]), color = "black", cex = par3d(3), font = 5)
  rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}

# library(RColorBrewer)
# n <- 15
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

library(rgl)
# plotting parameters ----
r3dDefaults$windowRect = c(50,50,700,700) # increase size of rgl window for better resolution when saving

# To save with new orientation of rgl window:
# Open rgl and move to desired orientation then save with below
# uM <- par3d()$userMatrix
# write.csv(uM, "uM_sideview.csv", row.names = F) # should only need to do this once
uM2 <- read.csv("uM.csv") # use pre-saved rgl orientation
uM2 <- data.matrix(uM2) # use pre-saved rgl orientation

uM3 <- read.csv("uM_topview.csv") # use pre-saved rgl orientation
uM3 <- data.matrix(uM3) # use pre-saved rgl orientation

uM4 <- read.csv("uM_sideview.csv") # use pre-saved rgl orientation
uM4 <- data.matrix(uM4) # use pre-saved rgl orientation


setwd(plots)

names = c("a) Encrusting", "b) Massive", "c) Submassive", 
          "d) Columnar", "e) Laminar", "f) Tabular (short)",
          "g) Tabular (medium)", "h) Tabular (tall)", "i) Arborescent",
          "j) Caespitose", "k) Branching?", "l) Digitage fused",
          "m) Digitage grid", "n) Corymbose 1", "o) Corymnose 2")

graphlabs = c("Encrusting", "Massive", "Submassive", 
          "Columnar", "Laminar", "Tabular (short)",
          "Tabular (medium)", "Tabular (tall)", "Arborescent",
          "Caespitose", "Branching", "Digitage fused",
          "Digitage grid", "Corymbose 1", "Corymnose 2")
#for (ft in 1:length(ftcelllist)){
for (ft in c(1:15)){
thisarray = ftcelllist[[ft]]
tsize=30
thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE)  ##
#rot = ifelse(ft %in% c(1,2,3,4,5,9,10,11,14,15), uM2, ifelse(ft %in% c(13,14), uM3, uM4))
# rot = if(ft %in% c(1,2,3,4,5,9,10,11,14,15)) uM2
# rot = if(ft %in% c(13,14)) uM3
# rot = if(ft %in% c(6,7,8)) uM4
par3d(userMatrix = uM2, zoom=0.6)
rgl.lines(c(100, 100), c(100, 100), c(0,50), color = "white")
rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color=colpal[ft])#,alpha=0.9,coralpolyps$sz*0.7)
#text3d(50,50,45,paste(names[ft]), color = "black", cex = par3d(3), font = 5)
rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}


for (ft in c(6,7,8)){
  thisarray = ftcelllist[[ft]]
  tsize=30
  thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE)  ##
  par3d(userMatrix = uM4, zoom=0.6)
  rgl.lines(c(100, 100), c(100, 100), c(0,50), color = "white")
  rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
  rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
  spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color = colpal[ft])#color = rainbow(length(ftcelllist))[ft])#,alpha=0.9,coralpolyps$sz*0.7)
#  text3d(50,50,40,paste(names[ft]), color = "black", cex = par3d(3), font = 5)
  rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}
# how to specify x limits so all cells will be the same size?

for (ft in c(12,13)){
  thisarray = ftcelllist[[ft]]
  tsize=30
  thiscelllist2 = which(thisarray>(998-tsize),arr.ind=TRUE)  ##
  par3d(userMatrix = uM3, zoom=0.6)
  rgl.lines(c(100, 100), c(100, 100), c(-15,35), color = "white")
  rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
  rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")
  spheres3d(thiscelllist2[,1],thiscelllist2[,2],thiscelllist2[,3], color=colpal[ft])#,alpha=0.9,coralpolyps$sz*0.7)
 # text3d(50,50,55,paste(names[ft]), color = "black", cex = par3d(3), font = 5)
  rgl.snapshot(paste("functional type standardised size", ft, ".png"))
  rgl.close()
}
# how to specify x limits so all cells will be the same size?

### Plot CSF ----
  for (ft in c(1,2,7,10,15)) {
    tsize = 30
    print(ft)
    print(paste("radius", tsize))
    thisarray = ftcelllist[[ft]]
    world=array(0,dim=c(100,100,100))
    world[thisarray>(998-tsize)]=1
    #print(sum(world))
    color=colpal[ft]
    color=colpal[ft]
    bottomview = world[,,1]
    sideview = apply(world , c(2,3), max)
    
    png(paste(ft,"csf plot.png"), width=3,height=6,units="in",res=600)
    par(mfrow=c(2,1))
    par(mar=c(1.5,1.6,0.1,1.5))
    image(bottomview, col = c("white", color), xaxt='n', yaxt='n')
    mtext(side = 1, text = "x", line = 0.5, cex = 2)
    mtext(side = 2, text = "y", line = 0.5, cex = 2)
    box()
    image(sideview, col = c("white", color), xaxt='n', yaxt='n')
    mtext(side = 1, text = "x", line = 0.5, cex = 2)
    mtext(side = 2, text = "z", line = 0.5, cex = 2)
    box()
    dev.off()
  }


# Calculate CSF for the large forms of each shape and plot as function of size ----
growthform = c(1:15) #c(1,2,7,10,15)
CSF = as.data.frame(growthform)
CSF$theintegral = NA
CSF$d1 = NA
CSF$d2 = NA

CSF$cells = NA # column for number of voxels 
CSF$cover = NA # column for % cover
CSF$rugosity = NA # column for number of colonies
CSF$SA = NA
CSF$SAcol = NA
CSF$contour = NA



csfsaveall = NULL
for(tsize in c(1:49)) { 
for (ft in c(1:15)){
  print(ft)
  print(paste("radius", tsize))
  CSF$radius = tsize
  thisarray = ftcelllist[[ft]]
  world=array(0,dim=c(100,100,100))
  world[thisarray>(998-tsize)] = 1 
  profile = (apply(world == 1, c(2,3), any))
  howmanyeachlayer = sapply(1:ws, function(i) sum(profile[,i]))
  CSF$theintegral[CSF$growthform == ft] = sum(howmanyeachlayer * 1:ws)
  basal = (world[,,1] == 1)
  CSF$d1[CSF$growthform == ft] = max(rowSums(basal))
  CSF$d2[CSF$growthform == ft] = max(colSums(basal))
  
  
  # Calculate colony linear rugosity in this timestep ----
  #tsize 50
  profile1=array(1,dim=c(100,100))
  profile = profile1*profile
  world_LR=array(0,dim=c(100,100,100))
  world_LR[50,,] = profile
  currentcells = which(world_LR!=0, arr.ind=TRUE)
  up = down = left = right = back = front = currentcells 
  down[,3]=down[,3]-1  
  up[,3]=up[,3]+1
  left[,2]=left[,2]-1
  right[,2]=right[,2]+1
  back[,1]=back[,1]+1
  front[,1]=front[,1]-1	
  down=down[down[,3]>0,]
  up=up[up[,3]<=ws,]
  left[,2][left[,2]==0]=ws
  right[,2][right[,2]>ws]=1
  front[,1][front[,1]==0]=ws
  back[,1][back[,1]>ws]=1
  potneighbs = rbind(up,down,left,right,back,front)
  ifempty = world_LR[potneighbs]==0 
  totalSA=sum(ifempty) #this is surface area of the profile, so subject the back and front surface?
  
  contour = totalSA-(2*sum(profile)) + (2*tsize-sum(basal[,50])) 
  CSF$contour[CSF$growthform == ft] = contour
  
  
  
  
  #volume and cover
  flatworld = matrix(0,nrow=ws,ncol=ws)
  for (iii in 1:ws) for (jjj in 1:ws) {
    cc = world[iii,jjj,]
    occup = which(cc>0)
    if (length(occup)>0) {
      flatworld[iii, jjj] = cc[max(occup)]
    } 
  }
  
  CSF$cells[CSF$growthform == ft] = sum(world == 1) # want to see where world is the colony id not the functional type #includes dead
  CSF$cover[CSF$growthform == ft] = sum(flatworld == 1)/(ws*ws)*100 #does not include dead
  
  # surface and linear rugosity
  currentcells = which(world!=0, arr.ind=TRUE)
  up = down = left = right = back = front = currentcells 
  down[,3]=down[,3]-1  
  up[,3]=up[,3]+1
  left[,2]=left[,2]-1
  right[,2]=right[,2]+1
  back[,1]=back[,1]+1
  front[,1]=front[,1]-1	
  down=down[down[,3]>0,]
  up=up[up[,3]<=ws,]
  left[,2][left[,2]==0]=ws
  right[,2][right[,2]>ws]=1
  front[,1][front[,1]==0]=ws
  back[,1][back[,1]>ws]=1
  potneighbs = rbind(up,down,left,right,back,front)
  ifempty = world[potneighbs]==0 
  totalSA=sum(ifempty)
#empty cells
  emptyz1 = sum(world[,,1]==0)
  colonySA = totalSA #only the colony
  totalSA = totalSA + emptyz1 #add on the empty ground in the environment
  
  
  # Calculate linear rugosity of world in this timestep ----
  htworld = apply(world,c(1,2), function (x) max(which(x>0)))
  htworld[htworld == -Inf] = 0
  #image(htworld)
  thisrugos = mean(sapply(1:ws,  function(i) sum(abs(diff(htworld[,i])))+ws ) )

  CSF$rugosity[CSF$growthform == ft] = thisrugos
  CSF$SA[CSF$growthform == ft] = totalSA
  CSF$SAcol[CSF$growthform == ft] = colonySA
  
  
}
  csfsaveall = rbind(csfsaveall, CSF)
}
setwd(data)
write.csv(csfsaveall, "Coralcraft_all size-variable relationship data.csv")
csfsaveall$csf = 16/(csfsaveall$d1^2*csfsaveall$d2*pi)*csfsaveall$theintegral
csfsaveall$growthform = as.factor(csfsaveall$growthform)


# Plot for fecundity estimates ----
head(csfsaveall)
csfsaveall$growthform = as.factor(csfsaveall$growthform)
ggplot() +
  geom_line(data = csfsaveall, aes(x = pi*radius^2, y = SAcol, colour = growthform), size = 1) +
  scale_colour_manual("Growth form", labels = graphlabs, values = colpal) +
  labs(x = expression("Colony planar area ("~cm^2~")")) +
  labs(y = expression("Colony surface ("~cm^2~")"))+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  #xlim(0,48) +
  xlim(0,7000) +
  ggtitle("Surface area ~ Planar area")

ggsave("Surface area ~ Planar area.png", width = 25, height = 15, units = "cm", dpi = 300)





# Plot for colony contour (largest) to straight line ----
head(csfsaveall)

csfsaveall$growthform = as.factor(csfsaveall$growthform)
ggplot() +
  geom_line(data = csfsaveall, aes(x = radius*2, y = contour, colour = growthform), size = 1) +
  scale_colour_manual("Growth form", labels = graphlabs, values = colpal) +
  labs(x = expression("Planar distance (cm)")) +
  labs(y = expression("Contour distance (cm)"))+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  #xlim(0,48) +
  ylim(0,250) +
  ggtitle("Planar ~ Contour")

ggsave("Colony Planar to contour distance.png", width = 25, height = 15, units = "cm", dpi = 300)



# Plot through time ----

# Number of colonies ----
setwd(plots)
library(png)
library(jpeg)
library(raster)
library(grid)
library(ggplot2)

encrust <- readPNG("encrust.png")%>%
  rasterGrob(interpolate=TRUE)
hemi <- readPNG("hemi.png")%>%
  rasterGrob(interpolate=TRUE)
tabular <- readPNG("tabular.png")%>%
  rasterGrob(interpolate=TRUE)
branch<- readPNG("branch.png")%>%
  rasterGrob(interpolate=TRUE)
cory<- readPNG("cory.png")%>%
  rasterGrob(interpolate=TRUE)

bks =round(seq(0, max(csfsaveall$csf), length.out = 5),0)
bks =c(0.001, 0.01,0.1,1,10,100,1000)

csfsaveall$growthform = factor(csfsaveall$growthform, levels = c("10",'15', '7','2', '1' ))

labs = c("Encrusting",'Hemispherical', 'Tabular','Corymbose', 'Branching')
labs = rev(labs)
p5 = 
  ggplot() +
  geom_line(data = csfsaveall, aes(x = radius, y = csf, colour = growthform), size = 1.5) + #, position=position_jitter(w=0, h=20)
  scale_colour_manual("Growth form", labels = labs, values = colpal[c(10,15,7,2,1)]) +
  labs(x = "Colony radius (cm)") +
  labs(y = "Colony Shape Factor (CSF)")+
  theme_bw()+
  geom_hline(yintercept=1.5, color = "red", size=1, linetype = 2) + 
  geom_hline(yintercept=20, color = "red", size=1, linetype = 3) + 
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  xlim(0,60) +
  #ylim(0,1) +
  annotation_custom(encrust, ymin = -10, ymax = 4.5, xmin = 50, xmax = 60) +
  annotation_custom(hemi, ymin = -5, ymax =5, xmin = 50, xmax = 60) +
  annotation_custom(tabular, ymin = -2, ymax = 5.5, xmin = 50, xmax = 60) +
  annotation_custom(branch, ymin = -1, ymax = 8, xmin = 49.5, xmax = 60.5) +
  annotation_custom(cory, ymin = -3, ymax = 8, xmin = 50, xmax = 60) +
  #scale_y_continuous(trans="log10") 
  scale_y_log10(breaks=bks,labels=bks)+
  ggtitle("e) Colony shape factor")
  
#ggsave("Radius by CSF_logscale.png", width = 21, height = 15, units = "cm", dpi = 300)



# plot cover, volume, linear rugosity
p1 = ggplot() +
  geom_line(data = csfsaveall, aes(x = radius, y = cover, colour = growthform), size = 1.5) +
  scale_colour_manual("Growth form", labels = labs, values = colpal[c(10,15,7,2,1)], guide = FALSE) +
  labs(x = "Colony radius (cm)") +
  labs(y = "Percentage cover (%)")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  xlim(0,48) +
  ggtitle("a) Percentage cover")

p2 = ggplot() +
  geom_line(data = csfsaveall, aes(x = radius, y = cells, colour = growthform), size = 1.5) +
  scale_colour_manual("Growth form", labels = labs, values = colpal[c(10,15,7,2,1)], guide = FALSE) +
  labs(x = "Colony radius (cm)") +
  ylab(expression(paste("Volume ± CI ( " , m^-3, ")", sep = "")))+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  xlim(0,48) +
  ggtitle("b) Volume")


p4 = ggplot() +
  geom_line(data = csfsaveall, aes(x = radius, y = SA/(100*100), colour = growthform), size = 1.5) +
  scale_colour_manual("Growth form", labels = labs, values = colpal[c(10,15,7,2,1)], guide = FALSE) +
  labs(x = "Colony radius (cm)") +
  labs(y = "Surface rugosity")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  xlim(0,48) +
  ggtitle("d) Surface rugosity")

p3 = ggplot() +
  geom_line(data = csfsaveall, aes(x = radius, y = rugosity/100, colour = growthform), size = 1.5) +
  scale_colour_manual("Growth form", labels = labs, values = colpal[c(10,15,7,2,1)], guide = FALSE) +
  labs(x = "Colony radius (cm)") +
  labs(y = "Linear rugosity")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) +
  xlim(0,48) +
  ggtitle("c) Linear rugosity")


#ggsave("Radius by CSF_logscale.png", width = 21, height = 15, units = "cm", dpi = 300)
r1 = plot_grid(p1, p2, ncol = 2, align = "h", axis = 'b', rel_widths = c(1,1))
r2 = plot_grid(p3, p4, ncol = 2, align = "h", axis = 'b', rel_widths = c(1,1))
r3 = plot_grid(p5,  ncol = 1, align = "h", axis = 'b')
pall = plot_grid(r1, r2,p5, ncol = 1)
pall
ggsave("200221_Radius output function plots R1.png", width = 20, height = 35, units = "cm", pall)


### code from Michael
thisarray = ftcelllist[[7]]

world=array(0,dim=c(100,100,100))

tsize=30

world[thisarray>(998-tsize)]=1

sum(world)

sideview = apply(world , c(2,3), max)

image(sideview)


#--------------------------------------------------------------------
# plot light absorption with rgl ----
par(mfrow=c(1,1))

for (ft in c(1,2,7,10,15)){
  
# Set initial light ----
light = array(rep(light.level * light.atten^((ws-1):0), each=ws*ws), dim=c(ws,ws,ws)) 
image(light[1,,])

# Fill world with coral
print(ft)
thisarray = ftcelllist[[ft]]
world=array(0,dim=c(100,100,100))
tsize=30
world[thisarray>(998-tsize)]=1

#then need to recalculate the light
light[world!=0] = 0 # Set light world to zero where there are coral cells 

light[,,1:(ws-1)] = light[,,2:ws] * light.atten    
lighttoside = light[,,1:(ws-1)] * light.side  
lighttoside1 = lighttoside[c(2:ws,1),,1:(ws-1)] * 0.25
lighttoside2 = lighttoside[c(ws,1:(ws-1)),,1:(ws-1)] * 0.25
lighttoside3 = lighttoside[,c(2:ws,1),1:(ws-1)] * 0.25
lighttoside4 = lighttoside[,c(ws,1:(ws-1)),1:(ws-1)] * 0.25
light[,,1:(ws-1)] = light[,,1:(ws-1)] - lighttoside + lighttoside1 + lighttoside2 + lighttoside3 + lighttoside4
light[,,ws] = light.level

lcol <- colorRampPalette(c('black', 'blue', 'cadetblue1'))
lcol1 <- lcol(99)
image(light[,,1], col =lcol1) #bottom
png("light tabular.png", width=7,height=7,units="in",res=600)
image(light[50,,], col =lcol1, xaxt='n', yaxt='n') #side
dev.off()
png("tabular to overlay.png", width=7,height=7,units="in",res=600)
image(world[50,,], col = c("black", "springgreen4"), xaxt='n', yaxt='n')
dev.off()

png("light only.png", width=7,height=7,units="in",res=600)
image(light[50,,], col =lcol1, xaxt='n', yaxt='n') #side
dev.off()

}
#--------------------------------------------------------------------
# not sure the above is going to work because takes a while for the shading to take place.
#rgl.postscript("graph1test.svg", fmt = "svg")
# 
# 
# shade3d(qmesh3d(verticesfloor, indices),color='black')
# shade3d(qmesh3d(verticesback, indices),color='darkgrey')
# shade3d(qmesh3d(verticesside, indices),color='grey')
# if (save3D == 1) {
#   rgl.snapshot(paste(version, ts + 1000, "final.png"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# tcl = ftcelllist[[5]]
# max(tcl)
# inds = which(tcl>960,arr.ind=TRUE)
# inds
# spheres3d(inds [,1],inds [,2],inds [,3], col='red')#,alpha=0.9,coralpolyps$sz*0.7)
# 
# save(ftcelllist,file="ftcelllist")
# 
# 

# if wanted to arrange plots in R
library(png)
library(grid)
library(gridExtra)

p1 <- readPNG('functional type standardised size 1 .png')
p2 <- readPNG('functional type standardised size 2 .png')
p3 <- readPNG('functional type standardised size 3 .png')
p4 <- readPNG('functional type standardised size 4 .png')
p5 <- readPNG('functional type standardised size 5 .png')
p6 <- readPNG('functional type standardised size 6 .png')
p7 <- readPNG('functional type standardised size 7 .png')
p8 <- readPNG('functional type standardised size 8 .png')
p9 <- readPNG('functional type standardised size 9 .png')
p10 <- readPNG('functional type standardised size 10 .png')
p11<- readPNG('functional type standardised size 11 .png')
p12 <- readPNG('functional type standardised size 12 .png')
p13 <- readPNG('functional type standardised size 13 .png')
p14 <- readPNG('functional type standardised size 14 .png')
p15 <- readPNG('functional type standardised size 15 .png')

grid.arrange(rasterGrob(p1),rasterGrob(p2),rasterGrob(p3),
             rasterGrob(p4),rasterGrob(p5),rasterGrob(p6),
             rasterGrob(p7),rasterGrob(p8),rasterGrob(p9),
             rasterGrob(p10),rasterGrob(p11),rasterGrob(p12),
             rasterGrob(p13),rasterGrob(p14),rasterGrob(p15), ncol=3)
ggsave("181108_Full page plot of 15 fts.png", dpi = 300, width = 20, height = 30, units = "cm")

