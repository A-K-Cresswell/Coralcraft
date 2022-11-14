ts=52*10
run=1

load( file = paste("worlds",ts,run))

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

###############

### covered cells

countcoveredpixels = function(slice){
  covered = 0
  if (any(slice)) {
	slicei = which(slice)
	top = max(slicei)
	covered = sum(!slice[1:top])
	}
  covered
}

getnumcoveredcells = function(run,ts){
  print(ts)
  load( file = paste("worlds",ts,run))
  world2 = world > 0 | dead > 0
  allcovered = apply(world2,c(1,2),countcoveredpixels )
  sum(allcovered)
}
####################

cc=sapply(1:50,function(ts) getnumcoveredcells(1,ts))
plot(cc)

#################################################

############

### covered cells from sides

par(mfrow=c(2,3))
image(world2[,,1])
allcovered = apply(world2,c(1,2),countcoveredpixels )
image(allcovered)
az=which(world2[,,1],arr.ind=T)
#points(az[,1]/100,az[,2]/100,pch=16,cex=0.1)
allcovered = apply(world2,c(1,3),countcoveredpixels )
image(allcovered)
allcovered = apply(world2,c(1,3),countcoveredpixels2 )
image(allcovered)
allcovered = apply(world2,c(2,3),countcoveredpixels )
image(allcovered)
allcovered = apply(world2,c(2,3),countcoveredpixels2 )
image(allcovered)


countcoveredpixels2 = function(slice){
  covered = 0
  if (any(slice)) {
	slicei = which(rev(slice))
	top = max(slicei)
	covered = sum(!rev(slice)[1:top])
	}
  covered
}

getnumcoveredcellssides = function(run,ts){
  print(ts)
  load( file = paste("worlds",ts,run))
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

getnumcoveredcellssides(1,52)/ws/ws
getnumcoveredcellssides(1,104)/ws/ws
getnumcoveredcellssides(1,52*4)/ws/ws
######################################################

## height-volume profile/ top-heaviness?

load( file = paste("worlds",ts,run))
world2 = world > 0 | dead > 0
volbylayer = apply(world2,3,sum)
plot(volbylayer,1:ws,t='l',ylim=c(0,30),ylab="height",xlab="volume")
load( file = paste("worlds",52,run))
world2 = world > 0 | dead > 0
volbylayer = apply(world2,3,sum)
lines(volbylayer,1:ws,col='red')

###########################################

#Visibility - top view

visibility2 = array(0,c(ws,ws,ws))

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
sw=0.7
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

############################################################
## pixel to pixel
############################################################

isLoS = function(a,b){
	ans = F
	leng = sqrt(sum((a-b)^2))
	npts = round(leng*10)
	ptmat = cbind(seq(a[1],b[1],length.out=npts) , seq(a[2],b[2],length.out=npts), seq(a[3],b[3],length.out=npts) )
	ptmat = round(ptmat)
	if (!any(world2[ptmat])) ans = T
	ans
}

### test from ocean floor to mid point of top

resmat = matrix(NA, nrow=ws,ncol=ws)
for (i in 1:ws) for (j in 1:ws){
	resmat[i,j] = isLoS(c(i,j,1) , c(50,50,100))	
}
image(resmat)

### test from ocean floor to all of top

resmat = array(NA, c(ws,ws,ws*ws))
iii=0
for (ii in seq(1,ws,by=10)) for (jj in seq(1,ws,by=10)){
iii=iii+1
print(iii)
for (i in 1:ws) for (j in 1:ws){
	resmat[i,j,iii] = isLoS(c(i,j,1) , c(ii,jj,100))	
}
}
dim(resmat)
resmat2 = apply(resmat[,,1:100],c(1,2),mean)
dim(resmat2)
image(resmat2)



