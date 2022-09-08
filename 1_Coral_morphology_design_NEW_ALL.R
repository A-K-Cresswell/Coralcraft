# Coralcraft - A functional-structural coral model
# Code for design of coral morphologies

# Authors
# Michael Renton
# Anna K Cresswell

# Appendix B of Cresswell et al. 2019, "Modelling the effects of hydrodynamic disturbance intensity and frequency on the morphological diversity and structural complexity of coral communities"

library(rgl)

# This script is a source file. It introduces all possible functional forms, and, based on the functional type, specifies the possible cells the functional type may grow into, as referenced to a starting cell.

#----------------------------------------------------------------------------------------
ftcelllist=list() # make an empty list
#allcells = data.frame(expand.grid(x=(-ws/2):(ws/2),y=(-ws/2):(ws/2),z=1:depth)) #ws/2 puts in the middle of the world
allcells = data.frame(expand.grid(x=(-ws/2):(ws/2),y=(-ws/2):(ws/2),z=1:ws))
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
thiscelllist = subset(allcells, abs(dist2-z)<=1.5 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[5]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 6. short tabular
thiscelllist = subset(allcells, (dist2 <= 3 & z<8) | z==8 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[6]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 7. medium tabular
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

### 12. digitate
thiscelllist = subset(allcells, (x%%4==0 & y%%4==0) | z==1 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[11]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 13. digitate2 / waffle says libby
thiscelllist = subset(allcells, (x%%4==0 & y%%4==0) | (z==1 & (x%%4==0 | y%%4==0))) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[12]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)


### 11. branching 3
ok = (allcells$x == 0 & allcells$y == 0)
ok1 = (allcells$y == 0 & allcells$x == allcells$z)
ok2 = (allcells$x == 0 & allcells$y == allcells$z)
ok3 = (allcells$y == 0 & allcells$x == -allcells$z)
ok4 = (allcells$x == 0 & allcells$y == -allcells$z)
ok = ok | ok1 |ok2 | ok3 | ok4
ok1 = (allcells$y == 0 & allcells$x == allcells$z-1)
ok2 = (allcells$x == 0 & allcells$y == allcells$z-1)
ok3 = (allcells$y == 0 & allcells$x == -allcells$z-1)
ok4 = (allcells$x == 0 & allcells$y == -allcells$z-1)
ok = ok | ok1 |ok2 |ok3|ok4
thiscelllist = subset(allcells, ok) 
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
hedgehogforlater = thiscelllist
ftcelllist[[15]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 16. new thicker tabular
thiscelllist = subset(allcells, (dist2 <= 3 & z<12) | (z==12 & dist2 <= 10 )  | z==13 | z==14 ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[16]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 17. new massive/mushroom
thiscelllist = subset(allcells, (dist2 <= 10 & z<10) | (z>=10) ) 
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[17]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 18. hedgehog 4: echinopora? 
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
thiscelllist = subset(allcells, ok & z <= 10 ) 
slice = subset(thiscelllist,z==10)
for (nz in 11:100) {
  newslice = slice
  newslice$z = nz
  thiscelllist = rbind(thiscelllist,newslice)
}
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[18]] = thiscelllist[-c(4:5)]  
dim(thiscelllist )

### 19. sheet-cone
thiscelllist = NULL
for (ddd in seq(-30,30,by=10)) {
  thissheet = subset(allcells,  abs(dist2-z-ddd)<=1.5) 
  thiscelllist = rbind(thiscelllist,thissheet)
}
thiscelllist$pr = thiscelllist$z + 0.5*thiscelllist$dist2
ftcelllist[[19]] = thiscelllist[-c(4:5)]  
dim(thiscelllist)

### 20. hedgehog on a stalk
thiscelllist = ftcelllist[[15]]
thiscelllist$z=thiscelllist$z+6
thiscelllist=subset(thiscelllist,z<=100)
slice = subset(thiscelllist,z==7)
for (nz in 1:6){
  newslice=slice
  newslice$z=nz
  thiscelllist=rbind(thiscelllist,newslice)
}
thiscelllist$pr = with(thiscelllist,sqrt(x^2+y^2+z^2))
ftcelllist[[20]] = thiscelllist  
dim(thiscelllist)


### trim to a maximum radius of 40
for (i in 1:length(ftcelllist)){
  ftcelllist[[i]] = subset(ftcelllist[[i]], pr<30)
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

# 
# tcl = ftcelllist[[5]]
# max(tcl)
# inds = which(tcl>999,arr.ind=TRUE) # if limit to >960, will have a radius of ~40
# inds
# spheres3d(inds [,1],inds [,2],inds [,3], col='red')#,alpha=0.9,coralpolyps$sz*0.7)

#Plot each for in rgl at set radius (you will need to load the rgl package to do this)
colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet")

for (i in (15:20)) {
  tcl = ftcelllist[[i]] # choose one to plot
  max(tcl)
  inds = which(tcl>960,arr.ind=TRUE) # if limit to >960, will have a radius of ~40
  inds
  open3d()
  spheres3d(inds [,1],inds [,2],inds [,3], col=colpal[i])#,alpha=0.9,coralpolyps$sz*0.7)
}

save(ftcelllist,file="ftcelllist")






