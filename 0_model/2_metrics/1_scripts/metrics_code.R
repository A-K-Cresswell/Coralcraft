
#### Metrics code

# LAST UPDATED: 10-07-2024
# Authors: Michael Renton, Daphne Oh

### STRUCTURAL COMPLEXITY METRICS
  ## linear rugosity 
  ## surface rugosity
  ## fractal dimension
### SHELTER METRICS
  ## Shelter volume
  ## Demersal shelter
  ## Pelagic shelter (mean from height 1:15)
  ## Pelagic shelter (height 2cm)
  ## Pelagic shelter (height 5cm)
  ## Pelagic shelter (height 10cm)
  ## Size-dependent shelter
############################################################

#### linear rugosity ----
lin_rugosity = function(run, ts){
  load(file = paste(id, scens, "worlds", ts, run, sep="_"))
  
  world2 = apply(world,c(1,2),function (x) max(which(x>0)))
  world2[world2 == -Inf] = 0
  sum(abs(diff(world2[,ws/2])))+ws 
}

#### surface rugosity  ----
sur_rugosity = function(run, ts){
  load(file = paste(id, scens, "worlds", ts, run, sep="_"))
  
  currentcells = which(world!=0,arr.ind=TRUE)
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
  
  emptyz1 = sum(world[,,1]==0)
  totalSA + emptyz1 
  
}

#### fractal dimension ----
fracdim = function(run,ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  df=data.frame(vs=1,pfill=sum(world>0))
  for (vs in c(2,4,5,10,20)){
    res=rep(F,(ws/vs)^3)
    i=0
    for (sx in seq(1,(ws-vs+1),by=vs)) for (sy in seq(1,(ws-vs+1),by=vs)) for (sz in seq(1,(ws-vs+1),by=vs)) {
      i=i+1
      res[i] = any(world[sx:(sx+vs-1),sy:(sy+vs-1),sz:(sz+vs-1)]>0) 
    }
    pfill=sum(res)
    df=rbind(df,c(vs,pfill))
  }
  ans=NA
  if (all(df$pfill>0)) {
    fm=lm(log(df$pfill)~log(df$vs))
    ans = as.numeric(-coef(fm)[2]) # fractal dimension value 2 < fd <3 
  }
  ans
}

#### shelter volume ----
countcoveredpixels = function(slice){
  covered = 0
  if (any(slice)) {
    slicei = which(slice)
    top = max(slicei)
    covered = sum(!slice[1:top])
  }
  covered
}
shelt_vol = function(run,ts){
  #print(ts)
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0
  allcovered = apply(world2,c(1,2),countcoveredpixels )
  sum(allcovered)
}

#### demersal shelter ----
countcoveredpixels2 = function(slice){
  covered = 0
  if (any(slice)) {
    slicei = which(rev(slice))
    top = max(slicei)
    covered = sum(!rev(slice)[1:top])
  }
  covered
}
dem_shelt = function(run,ts){
  # print(ts)
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
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

#### Pelagic shelter (mean from height 1:15) ----
pel_shelter_1_15 = function(run,ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0
  
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
  
  sw=0.5 ## peripheral angle
  
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
  sheltered2 = 1-visibility2
  sheltered2[world2] = 0
  
  mean(sheltered2[,,0:15]) ## change heights here
}

#### Pelagic shelter (height 2cm) ----
pel_shelter_2 = function(run,ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0

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

  sw=0.5 ## peripheral angle

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
  sheltered2 = 1-visibility2
  sheltered2[world2] = 0

  mean(sheltered2[,,2])
}

#### Pelagic shelter (height 5cm) ----
pel_shelter_5 = function(run,ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0

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

  sw=0.5 ## peripheral angle

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
  sheltered2 = 1-visibility2
  sheltered2[world2] = 0

  mean(sheltered2[,,5])
}

#### Pelagic shelter (height 10cm) ----
pel_shelter_10 = function(run,ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0

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

  sw=0.5 ## peripheral angle

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
  sheltered2 = 1-visibility2
  sheltered2[world2] = 0

  mean(sheltered2[,,10])
}

#### Size-dependent shelter 
size_dep = function(run, ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_")) # to load world file
  
  vs=9 ## change size of predator here
  pts=(vs-1)/2
  buffersize = (vs+1)/2-1
  firstone=buffersize+1
  
  vsg=1 ## change size of prey here
  ptsg=(vsg-1)/2
  buffersizeg = (vsg+1)/2-1
  res=rep(F,(ws-2*buffersize)^3)
  
  i=0
  
  for (sx in seq(firstone,ws-firstone)) for (sy in seq(firstone,ws-firstone)) for (sz in seq(firstone,ws-firstone)) {
    i=i+1
    okforprey = !any(world[(sx-ptsg):(sx+ptsg),(sy-ptsg):(sy+ptsg),(sz-ptsg):(sz+ptsg)]>0) 
    notokforpred = any(world[(sx-pts):(sx+pts),(sy-pts):(sy+pts),(sz-pts):(sz+pts)]>0) 
    res[i] = okforprey & notokforpred
  }
  mean(res)
  # print(mean(res)) ## proportion of the space that is okforprey & notokforpred
}