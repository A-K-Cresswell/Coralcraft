# Coralcraft - A Functional-Structural Coral Model ----
# Code For Design Of Coral Morphologies

# Authors
# Anna K Cresswell
# Daphne Oh
# Michael Renton

library(rgl)

# This script is a source file. It introduces all possible functional forms,
# and, based on the functional type, specifies the possible cells the
# functional type may grow into, as referenced to a starting cell.

# 1. BUILD THE 3D CELL GRID ----
ftcelllist = list() # make an empty list

# allcells generates every candidate (x,y,z) cell around a colony's recruit
# point. x ranges -wx/2+1..wx/2 and y ranges -wy/2+1..wy/2 (one cell short of
# the full -w/2..w/2 span) so that, after the +wx/2 / +wy/2 offset used
# further below, indices always land in the valid 1..wx / 1..wy array range.
# (generating the full -w/2..w/2 span would produce one extra value per axis
# than the array has slots for, and offset that extra value to array index 0,
# which R does not allow - see the offset step below.)
allcells = data.frame(expand.grid(
  x=(-wx/2+1):(wx/2), y=(-wy/2+1):(wy/2), z=1:wz
))
head(allcells)

allcells$dist1 = sqrt((allcells$x)^2 + (allcells$y)^2 + (allcells$z)^2)
allcells$dist2 = sqrt((allcells$x)^2 + (allcells$y)^2) # radius

# 2. DEFINE EACH FUNCTIONAL TYPE'S GROWTH FORM ----

## 1. Encrusting ----
thiscelllist = subset(allcells , z==1)
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[1]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 2. Flexi-Hemispherical ----
r = 1.5
thiscelllist = allcells
thiscelllist$pr = sqrt(thiscelllist$x^2 + thiscelllist$y^2 +
  r * thiscelllist$z^2)
ftcelllist[[2]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 3. Digitate ----
r = 2
# grid of colony
thiscelllist = subset(allcells, (x%%4 %in% 0:1 & y%%4 %in% 0:1) | z==1 )
# shape of single colony
thiscelllist$pr = sqrt(thiscelllist$x^2 + thiscelllist$y^2 +
  r * thiscelllist$z^2)
ftcelllist[[3]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 4. Corymbose ----
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
dim(thiscelllist)

## 5. Tabular ----
thiscelllist = subset(allcells,
  (dist2 <= 3 & z<12) | (z==12 & dist2 <= 10) | z==13 | z==14)
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[5]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 6. Mushroom ----
# stalk (support) radius = colony's max radius minus a 20cm unsupported
# overhang at the canopy edge, representing base erosion
stalkmaxr = maxradius[6] - 20
thiscelllist = subset(allcells, (dist2 <= stalkmaxr & z<10) | (z>=10) )
thiscelllist$pr = thiscelllist$dist1
ftcelllist[[6]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 7. Columnar ----
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
thiscelllist = subset(allcells, ok & z <= 10 ) # adjusts height of base
slice = subset(thiscelllist,z==10) # slice where base grows upright
for (nz in 11:50) { # vertical growth of coral after slice
  newslice = slice
  newslice$z = nz
  thiscelllist = rbind(thiscelllist,newslice)
}
thiscelllist$pr = sqrt((thiscelllist$x)^2 + (thiscelllist$y)^2 +
  (thiscelllist$z)^2)
ftcelllist[[7]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 8. Foliose ----
thiscelllist = NULL
gapsize = 8
ok = allcells$x==0 & allcells$y==0 & allcells$z==0
for (ddd in seq(0,24,by=gapsize )) {
  ok2 = with(allcells, (abs(dist2-z-ddd)<=1 & z<(ddd+6) & z>1) |
    (z==1 & dist2<=ddd & dist2>(ddd-gapsize)))
  ok = ok | ok2
}
thiscelllist = subset(allcells, ok )
thiscelllist = subset(thiscelllist , x > (-50) & y > (-50))
thiscelllist$pr = (thiscelllist$dist2)
ftcelllist[[8]] = thiscelllist[-c(4:5)]
dim(thiscelllist)

## 9. Bushy ----
thiscelllist = ftcelllist[[4]]
thiscelllist$z = thiscelllist$z+6
thiscelllist = subset(thiscelllist,z<=100)
slice = subset(thiscelllist,z==7)
for (nz in 1:6){
  newslice = slice
  newslice$z = nz
  thiscelllist = rbind(thiscelllist,newslice)
}
thiscelllist$pr = with(thiscelllist,sqrt(x^2+y^2+z^2))
ftcelllist[[9]] = thiscelllist
dim(thiscelllist)

## 10. Branching ----
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
dim(thiscelllist)

# 3. TRIM EACH FORM TO ITS MAX RADIUS ----
# maxradius is set in 1_Coralcraft_simulation_code.R (one value per
# functional type, in ftcelllist order); use Inf for no limit
for (i in 1:length(ftcelllist)){
  ftcelllist[[i]] = subset(ftcelllist[[i]], pr<maxradius[i])
}

# 4. CONVERT EACH FORM TO A LOOKUP ARRAY ----
library(Matrix)
for (i in 1:length(ftcelllist)){
  m1 = array(0,dim=c(wx,wy,wz))
  inds = as.matrix((ftcelllist[[i]])[,1:3])
  # x ranges -wx/2+1..wx/2 (see allcells above), so this lands in the valid
  # 1..wx array range with no boundary overflow
  inds[,1] = inds[,1] + wx/2
  # same reasoning for y (-wy/2+1..wy/2 -> 1..wy)
  inds[,2] = inds[,2] + wy/2
  m1[inds] = 999-ftcelllist[[i]]$pr
  ftcelllist[[i]] = m1
}

# 5. PREVIEW ALL GROWTH FORMS ----
library(png)

colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "skyblue3", "orchid1","orange")

# order matches ftcelllist above (and is reused for ftypes$names in
# 1_Coralcraft_simulation_code.R)
ftnames = c("encrusting", "flexihemispherical", "digitate", "corymbose",
            "tabular", "mushroom", "columnar", "foliose", "bushy",
            "branching")

preview.png = "0_model/growth_form_shapes.png"
preview.pdf = "0_model/growth_form_shapes.pdf"

# wide window so all 10 panels are legible
open3d(windowRect = c(50, 50, 1450, 650))
# 2 rows x 5 cols, one panel per functional type
mfrow3d(2, 5, sharedMouse = TRUE)
for (i in 1:length(ftcelllist)) {
  next3d()
  tcl = ftcelllist[[i]] # full max-size shape for this functional type
  # every valid cell, i.e. the shape at its maxradius limit
  inds = which(tcl > 0, arr.ind = TRUE)
  # subsample very large/flat shapes (e.g. mushroom canopy) just for preview
  # speed
  if (nrow(inds) > 3000) inds = inds[sample(1:nrow(inds), 3000), ]
  points3d(inds[,1], inds[,2], inds[,3], col = colpal[i], size = 3)
  title3d(main = ftnames[i], col = "black")
}
rgl.snapshot(preview.png)
close3d()

# also save the same combined image as a one-page PDF
img = readPNG(preview.png)
pdf(preview.pdf, width = 14, height = 6.5)
par(mar = c(0,0,0,0))
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(img, 0, 0, 1, 1)
dev.off()

print(paste("Growth form previews saved to", preview.png, "and", preview.pdf))

# 6. SAVE GROWTH FORM LIST ----
save(ftcelllist,file="ftcelllist")
