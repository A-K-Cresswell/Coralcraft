
#### Code for metrics

## total covered cells (shelter volume)
## mean coverage from side view
## mean coverage from top (1:15)
## mean coverage from top (1:20)
## mean sheltered cells from z=2
## mean sheltered cells from z=5
## mean sheltered cells from z=10
## top-heaviness
## fractal dimensions
## little fisheys

############################################################

#### total covered cells (volume of space below coral cover) ####
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
  #print(ts)
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0
  allcovered = apply(world2,c(1,2),countcoveredpixels )
  sum(allcovered)
}

#### mean cover from side ####
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

#### mean sheltered cells from top (1:15) ####
meantopview_1_15 = function(run,ts){
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
  
  mean(sheltered2[,,0:15])
}

#### mean sheltered cells from top (1:20) ####
meantopview_1_20 = function(run,ts){
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

  mean(sheltered2[,,0:20])
}

#### mean sheltered cells from z=2 ####
meantopview_2 = function(run,ts){
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

#### mean sheltered cells from z=5 ####
meantopview_5 = function(run,ts){
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

#### mean sheltered cells from z=10 ####
meantopview_10 = function(run,ts){
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

#### mean sheltered cells from z=15 ####
meantopview_15 = function(run,ts){
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
  
  mean(sheltered2[,,15])
}
 
#### top-heaviness ----
meantopheavy = function(run,ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_"))
  world2 = world > 0 | dead > 0
  volbylayer = apply(world2,3,sum)
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

#### juvies (prey 1 pred 9) ----
juv_1 = function(run, ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_")) # to load world file
  
  vs=9 ## size of bad fish
  pts=(vs-1)/2
  buffersize = (vs+1)/2-1
  firstone=buffersize+1
  
  vsg=1 ## size of good fish
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

#### juvies (prey 1 pred 21) ----
juv_1_21 = function(run, ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_")) # to load world file
  
  vs=21 ## size of bad fish
  pts=(vs-1)/2
  buffersize = (vs+1)/2-1
  firstone=buffersize+1
  
  vsg=1 ## size of good fish
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

#### juvies (prey 3 pred 9) ----
juv_9 = function(run, ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_")) # to load world file
    
    vs=9 ## size of bad fish
    pts=(vs-1)/2
    buffersize = (vs+1)/2-1
    firstone=buffersize+1
    
    vsg=3 ## size of good fish
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

#### juvies (prey 3 pred 21) ----
juv_21 = function(run, ts){
  load(file = paste(id, scens, "worlds",ts,run, sep = "_")) # to load world file
  
  vs=21 ## size of bad fish
  pts=(vs-1)/2
  buffersize = (vs+1)/2-1
  firstone=buffersize+1
  
  vsg=3 ## size of good fish
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

