


ts=52*3
run=1

load( file = paste("worlds",ts,run))

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
  if (any(slice)) covered = sum(cumsum(slice)==0)
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

#Visibility 



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

shh=apply(sheltered,3,mean)
plot(shh,t='l')
shh2=apply(sheltered2,3,mean)
lines(shh,col='red')
