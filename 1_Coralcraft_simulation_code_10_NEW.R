# Coralcraft - A functional-structural coral model
# Model code

# Authors
# Michael Renton
# Anna K Cresswell
# Daphne Oh

# For inquires contact annacresswell@gmail.com

# This code will be kept updated with any new additions at https://github.com/A-K-Cresswell/Coralcraft

# Online Resource 3 of Cresswell et al. 2020, "Frequent hydrodynamic disturbances decrease the morphological diversity and structural complexity of simulated coral communities" published in Coral Reefs

# Set your working directory to a folder containing this script and Online_Resource_1_Coral_morphology_design.R
setwd("C:/Users/OH012/OneDrive - CSIRO/Documents/GitHub/Coralcraft") # Daph's working directory
#setwd("~/GitHub/Coralcraft") # Anna's working directory

colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "skyblue3", "orchid1","orange")
colpal2= c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "skyblue3", "orchid1","orange","grey")

# for export naming ----
version = paste(Sys.Date())

# libaries 
library(rgl)
library(scatterplot3d)

# 1. set parameters ----
# set simulation parameters ----
runs = 1 # how many times to run the simulation
timesteps = 52*3  # the number of timesteps in each simulation, e.g. 52 weeks * 100 years
ws = 100 # world size (cm)
maxdepth = 1 #(m) # this parameter is not used again
mindepth = 0 #(m) # used to calc top of world light level
n.initial.colonies = 10  # how many corals in the beginning - initial size is one block each (i.e. 1cm x 1cm)

# set spawning parameters ----
randomrecruits = 1 # if set to 1 random allocation of growth forms, else allocation a probability of the number of live cells of each colony.
spawn.freq = 1 # 52 is annually --- how frequently (in timesteps) does spawning occur (set to high number for no spawning)
nnewrecruits= 5 # how many new corals each spawn (could make random)

# set disturbance parameters ----
randomdist = "fixed" # fixed or random disturbance intensity and frequency?
# intensity 
if(randomdist == "fixed") disturbance.intensity.low = c(20,20,20) else disturbance.intensity.low = exp(rnorm(1000,log(20),0.5)) # how intense is the disturbance (a smaller number is a bigger disturbance) 
if(randomdist == "fixed") disturbance.intensity.high = c(1.5,1.5,1.5) else disturbance.intensity.high = exp(rnorm(1000,log(1.5),0.5)) # how intense is the disturbance (a smaller number is a bigger disturbance) 
# frequency
frequent = 26
infrequent = 52*5  
freq.low = 99999 # 99999 OR frequent OR infrequent
freq.high = 99999 # 99999 OR frequent OR infrequent

# background mortality paramters ----
background.mort = 0.001 # base probability 99% chance of surviving a year
background.mort2 = 0.005 # lower probability, 95%, for encrusting due to being on the bottom

# light and resources parameters ----
maint = 0.1
surface.light.level = 1
k = log(1)-log(0.72) # k = 0.3285041
light.level = surface.light.level * (exp(1))^(-k*mindepth) # light level at min depth 
light.side = 0.5 # proporation of light travelling sidewards  
light.atten = 0.72^(1/100) # for k =0.64 - 0.9908789 # light.atten = 0.9908789^100 = 0.40000 OR 0.4^(1/100)
res.cap = 6 # resource cap: this means a colony can only store 6 units of energy
start.res = 1 # resources each colony starts with
#0.938005^(1/100)
#Light equation
#Light_depth = Light_surface x e^(-k*depth)


# 2. source growth form information ----
#source('1_Coral_Morphology_10_NEW.R') ## need to run this once to load the growth forms
load(file="ftcelllist") # load growth forms


# make new folder with name giving parameters for automatically saving outputs ----
dir.create(paste(version, randomdist, timesteps, "tss", runs, "runs", spawn.freq, "spawn.freq", nnewrecruits, "nnewrecruits", randomrecruits, "randomrecruits", freq.low, freq.high, "disturbance frequencies", background.mort, "background mort", sep = "_"))
foldername = paste(version, randomdist, timesteps, "tss", runs, "runs", spawn.freq, "spawn.freq", nnewrecruits, "nnewrecruits", randomrecruits, "randomrecruits", freq.low, freq.high, "disturbance frequencies", background.mort, "background mort", sep = "_")
# note, sometimes the long name strings can cause issues, so remove information not needed if start getting errors here

# set the working directory to be this new folder
thisoutput = paste(getwd(), foldername, sep="/")
setwd(thisoutput)


# plotting parameters ----
draw = 0 # if set to 1, will plot in 3D each timestep - not currently set up (see figure script)
save3D = 0 # if set to 1, will save 3D plot in each timestep - not currently set up (see figure script)
drawscatter = 0 # will plot and save a scatterplot - not currently set up (see figure script)
r3dDefaults$windowRect = c(50,50,500,500) # increase size of rgl window for better resolution when saving
#       # To save with new orientation of rgl window:
#       # Open rgl and move to desired orientation then save with below
#       # uM <- par3d()$userMatrix
#       # write.csv(uM, "uM.csv", row.names = F) # should only need to do this once

r1= c(0.70,-0.70,0.01,0)
r2 = c(0.25,0.25,0.93,0)
r3 = c(-0.65, -0.65,0.37,1)
r4 = c(0,0,0,0)
uM3 = rbind(r1,r2,r3,r4)
colnames(uM3) = c("V1", "V2", "V3", "V4")
rownames(uM3) = c("[1,]", "[2,]", "[3,]", "[4,]")
# shade the sides of the world
indices <- c( 1, 2, 3, 4 )
verticesfloor <- c(0, 0, 0, 1.0, 100, 0, 0, 1.0, 100,  100, 0, 1.0, 0,  100, 0, 1.0)
verticesback <- c(0,  100, 0, 1.0,100,  100, 0, 1.0, 100, 100, 100, 1.0, 0, 100, 100, 1.0)
verticesside <- c(100,  100, 0, 1.0, 100,  0, 0, 1.0, 100, 0, 100, 1.0,100, 100, 100, 1.0)

# plot light
light = array(rep(light.level * light.atten^((ws-1):0), each=ws*ws), dim=c(ws,ws,ws)) # this instantly sets up initial light through the world

# image(light[50,,])
# depthp = as.data.frame(seq(-0.01, -10, by =-0.01))
# lightp = as.data.frame(rep(1 * 0.9967203^(seq(1, 1000, by =1))))
# lightplot = cbind(depthp, lightp)
# colnames(lightplot) = c("depth", "light")
# ggplot(lightplot, aes(y = depth, x = light)) +
#   geom_point(shape = 1, colour = "blue") +
#   xlab("Light") +
#   ylab("Depth(m)") +
#   scale_y_continuous(breaks =seq(-0.01, -11, by =-1), labels = seq(0, 10, by =1)) +
#   scale_x_continuous(breaks = seq(0, 1, by =0.1),labels = seq(0.0, 1.0, by =0.1), limits = c(0,1)) 
# ggsave("Light profile k=0.064.png", width = 13, height = 13, units = "cm")# dpi = 300))


for (run in 1:runs){ # multiple simulation runs
  
  #### Set initial conditions ----
  world = array(0,dim=c(ws,ws,ws)) # Create world as cube
  dead = array(0,dim=c(ws,ws,ws)) # Create dead world as cube
  light = array(rep(light.level * light.atten^((ws-1):0), each=ws*ws), dim=c(ws,ws,ws)) # this instantly sets up initial light through the world 
  init.locations = sample(1:(ws*ws), n.initial.colonies) # randomly select locations for the initial colonies
  rows = (init.locations-1)%%ws + 1
  cols = (init.locations-rows)/ws + 1
  for (i in 1:n.initial.colonies) world[rows[i], cols[i], 1] = i # assign colony id to location in world
  
  ftypessaveall = NULL # dataframe for saving the number of colonies, percentage cover, number of voxels, linear and surface rugosity in each timestep
  
  # set up functional types information ----
  nfts = length(ftcelllist) # set number of functional groups (e.g. tabular, columnar, massive, encrusting)
  ftypes = data.frame(names = c("encrusting", "flexi-hemispherical", "digitate", "corymbose", "thick tabular",
                                "mushroom", "fingers", "sheet", "hedgehog", "staghorn"))  #new morphologies
  ftypes$ftnum = 1:nrow(ftypes)
  ftypes$resource.to.growth = 1 # this can be modified to change growth rates
  ftsinc_list = c("encrusting", "flexi-hemispherical", "digitate", "corymbose", "thick tabular",
                  "mushroom", "fingers", "sheet", "hedgehog", "staghorn") # select which functional types to include
  
  ftsinc = subset(ftypes, names %in% ftsinc_list) # subset from the full possible list
  nftsinc = nrow(ftsinc)
 
  # make the colony map ----
  ftlist = as.list(ftsinc$ftnum)
  colonymap = data.frame(
    colonyid = 1:n.initial.colonies,
    #ft =  ((1:n.initial.colonies)-1)%%ftsinc + 1,
    ft = rep(ftsinc$ftnum, length.out = n.initial.colonies), # ORDERED SAMPLE
    #ft =  sample(c(ftsinc$ftnum,ftsinc$ftnum), n.initial.colonies, replace = T), # RANDOM SAMPLE
    age = 1,
    res = start.res)
  
  colonymap$row = rows
  colonymap$col = cols
  colonymap$bd = 1
  colonymap$sz = 1
  
  fatesrec = list()
  nlooplight=50
  #-------------------------------------------------------------------------------------
  
  #### Simulation ----
  for (ts in 1:timesteps){
    print(paste(version, ",", randomdist, ",", timesteps, "tss,", runs, "runs,", spawn.freq, "spawn.freq,", nnewrecruits, "nnewrecruits,", randomrecruits, "randomrecruits", freq.low, freq.high, "disturbance frequencies, ", background.mort, "background mort"))
    print(paste("there are", nrow(colonymap), "colonies in the world", sep = " "))
    print(paste('run', run))
    print(paste('time step', ts)) 
    
    # Light ----
    for (iii in 1:nlooplight){
      light[,,1:(ws-1)] = light[,,2:ws] * light.atten    
      lighttoside = light[,,1:(ws-1)] * light.side  
      lighttoside1 = lighttoside[c(2:ws,1),,1:(ws-1)] * 0.25
      lighttoside2 = lighttoside[c(ws,1:(ws-1)),,1:(ws-1)] * 0.25
      lighttoside3 = lighttoside[,c(2:ws,1),1:(ws-1)] * 0.25
      lighttoside4 = lighttoside[,c(ws,1:(ws-1)),1:(ws-1)] * 0.25
      light[,,1:(ws-1)] = light[,,1:(ws-1)] - lighttoside + lighttoside1 + lighttoside2 + lighttoside3 + lighttoside4
      light[,,ws] = light.level

    ### Light uptake ----
    lightuptake = array(0,dim=c(ws,ws,ws)) # make light uptake world
    lightuptake[world > 0 & !dead] = light[world > 0 & !dead] # Fill with light level only where there are coral cells
    light[world!=0] = 0 # Set light world to zero where there are coral cells (dead or alive) 

    }  ## end the light loop
    
    nlooplight=1 # reset number of times to update light to 1, we want to increase this if a colony dies

    ### Resources per colony ---- 
    if (nrow(colonymap) > 0) { # only run if there are at least 1 colony in the world
      for (i in colonymap$colonyid) { # take each colony separately
        ii = which(colonymap$colonyid == i)
        colonymap$res[ii] = colonymap$res[ii] + sum(lightuptake[world == i & !dead] - maint) # update amount of resources  
      }
      
      colonymap$res[colonymap$res>res.cap] = res.cap # cap resources
      
      ### Death ---- 
      dead[lightuptake < maint & world > 0] = TRUE # voxel dies if not getting enough light to maintain itself 
      colonymap$stillalive=TRUE
      for (i in colonymap$colonyid) { # take each colony separately
        allcellsdead = all(dead[world == i] == 1)
        if (allcellsdead){
          dead[world == i] = 0 # set dead world to zero where disturbed colonies have been removed
          world[world == i] = 0 # set world to zero where disturbed colonies have been removed
          colonymap$stillalive[which(colonymap$colonyid==i)]=FALSE
          print (paste("colony ID",i,"dies from light starvation"))
          thisfatesrec = colonymap[which(colonymap$colonyid==i),]
          thisfatesrec$fate="light"
          thisfatesrec$deathtime=ts
          fatesrec = rbind(fatesrec ,thisfatesrec )
          nlooplight = 50
        }
      }
      
      colonymap = subset(colonymap, stillalive) 	# drop the fully dead colonies out of the colony map 
      # in future versions may want to return these as 'dead structure' for some timesteps
      
      ### Growth ----
      ncolonies = nrow(colonymap)
      if (ncolonies > 0) {
        if (ncolonies == 1) colonygrowthorder = 1 else colonygrowthorder = sample(1:ncolonies) 
        for (i in colonygrowthorder) {
          thiscolony = colonymap[i, ] 
          ngrowth = floor(thiscolony$res / ftsinc$resource.to.growth[which(ftsinc$ftnum == thiscolony$ft)]) 
          
          if (ngrowth > 0) {
            #print(paste("ngrowth:",ngrowth))
            thisid = thiscolony$colonyid 
            currentcells = which(world==thisid & !dead,arr.ind=TRUE)
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
            potneighbs = unique(rbind(up,down,left,right,back,front))
            ifempty = world[potneighbs]==0 
            neighbs = matrix(potneighbs[ifempty,],ncol=3) 
            neighbsinlist = neighbs-matrix(c(thiscolony$row-ws/2,thiscolony$col-ws/2,0),nrow=nrow(neighbs),ncol=3,byrow=TRUE) 
            neighbsinlist[,1:2] = (neighbsinlist[,1:2] - 1)%%ws + 1
            priorities = ftcelllist[[thiscolony$ft]][neighbsinlist] 
            npot = sum(priorities>0) 
            if (npot<=ngrowth) {    ## grow all potentials
              use = which(priorities>0) 
            } else {   ## grow the highest priority ngrowth potentials
              ord = order(priorities ,y=rnorm(length(priorities)), decreasing=TRUE)
              use = ord[1:ngrowth]
            }
            newcells = 	neighbs[use,]
            world[matrix(newcells,ncol=3)] = thisid 
          }}}
      
      
      
      
      # update age of all colonies
      colonymap$age = colonymap$age + 1 # all colonies age each timestep, even if they didn't grow 
    } # end if statement for if there are no colonies in colonymap
    
    # Save information about world occupation by each functional type ----
    ftypessave = data.frame(c(1:nfts,"dead")) # each functional type
    ftypessave$run = run # which model run is it
    ftypessave$cells = NA # column for number of voxels 
    ftypessave$timestep = ts # column for the timestep
    ftypessave$cover = NA # column for % cover
    ftypessave$no.colonies = NA # column for number of colonies
    ftypessave$rugosity = NA # column for number of colonies
    ftypessave$SA = NA
    
    flatworld = matrix(0,nrow=ws,ncol=ws)
    for (iii in 1:ws) for (jjj in 1:ws) {
      cc = world[iii,jjj,]
      occup = which(cc>0)
      if (length(occup)>0) {
        flatworld[iii, jjj] = cc[max(occup)]
        if (dead[iii, jjj, max(occup)] == 1) flatworld[iii, jjj] = -1
      } 
    }
    
    flatworlddead = matrix(0,nrow=ws,ncol=ws)
    for (iii in 1:ws) for (jjj in 1:ws) {
      cc = dead[iii,jjj,]
      occup = which(cc>0)
      if (length(occup)>0) {
        flatworlddead[iii, jjj] = cc[max(occup)]
      } 
    }
    
    for (i in 1:nfts) {
      t1 = subset(colonymap, ft == i)
      ftypessave$no.colonies[i] = nrow(t1)
      if (nrow(t1) > 0) { 
        ftypessave$cells[i] = sum(world %in% t1$colonyid)
        ftypessave$cover[i] = sum(flatworld %in% t1$colonyid & !dead)/(ws*ws)*100 #does not include dead
      }
      else {
        ftypessave$cells[i] = 0
        ftypessave$cover[i] = 0
      }
    }
    
    
    ## calculate total surface area
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
    totalSA = totalSA + emptyz1 # add on empty z = 1 (seafloor cells
    
    ftypessave$cells[which(ftypessave$c.1.nfts...dead.. == "dead")] = sum(dead > 0)
    ftypessave$cover[which(ftypessave$c.1.nfts...dead.. == "dead")] = sum(flatworld %in% 1)/(ws*ws)*100 
    ftypessave$no.colonies[which(ftypessave$c.1.nfts...dead.. =="dead")] = sum(colonymap$stillalive == F) 
    
    
    # Calculate linear rugosity of world in this timestep ----
    htworld = apply(world,c(1,2),function (x) max(which(x>0)))
    htworld[htworld == -Inf] = 0
    thisrugos = mean(sapply(1:ws,  function(i) sum(abs(diff(htworld[,i])))+ws ) )
    
    ftypessave$rugosity = thisrugos
    ftypessave$totalSA = totalSA
    
    ftypessaveall = rbind(ftypessaveall, ftypessave) 

    
    
        # Spawning ----
    if ((ts+spawn.freq/2+6)%%spawn.freq == 0) { # spawing offset to occur in half years + 6 timesteps
      print("SPAWNING")
      reclocations = sample(1:(ws*ws) , nnewrecruits) # random x y locations for new recruit
      rows = (reclocations-1)%%ws + 1
      cols = (reclocations-rows)/ws + 1
      
      nactnewrecruits = 0
      for (i in 1:nnewrecruits) {
        if (world[rows[i], cols[i], 1] == 0) { # if there isn't already a coral there
          nactnewrecruits = nactnewrecruits + 1 # then we get +1 new recruit
          print("recruitment!!")
          if (nrow(colonymap) > 0) {newcolID = max(colonymap$colonyid) + 1 
          } else {newcolID =  1} # to account for if it is the first spawn and there are no other colonies
          world[rows[i], cols[i], 1] = newcolID  # populate world with new colonyID
          # select fts
          if (randomrecruits == 1) {
            if (length(ftsinc$ftnum) == 1) { # if there is only one ft choose that one
              colonymapnew = data.frame(colonyid = newcolID ,
                                        ft = ftsinc$ftnum,
                                        age = 1,
                                        res = start.res)} # resources
            if (length(ftsinc$ftnum) > 1) { # else sample from the available fts
              colonymapnew = data.frame(colonyid = newcolID ,
                                        ft = sample(ftsinc$ftnum, 1), # had to add the other if() for when there is only one colony type
                                        age = 1,
                                        res = start.res)} # resources
          } else {
            if (length(ftsinc$ftnum) == 1) { # if there is only one ft choose that one
              colonymapnew = data.frame(colonyid = newcolID ,
                                        ft = ftsinc$ftnum,
                                        age = 1,
                                        res = start.res)} # resources
            if (length(ftsinc$ftnum) > 1) { #else sample from the available fts
              colonymapnew = data.frame(colonyid = newcolID ,
                                        ft = sample(ftsinc$ftnum, 1, prob = ftypessave$cells[ftsinc$ftnum]), 
                                        age = 1,
                                        res = start.res)} # resources
          }
          
          
          colonymapnew$row = rows[i]
          colonymapnew$col = cols[i]
          colonymapnew$bd=ts
          colonymapnew$sz = 1
          colonymapnew$stillalive = TRUE
          
          colonymap=rbind(colonymap,colonymapnew)
        }
      }
      print(paste(nactnewrecruits ,"actual new recruits"))
      
      #update light post spawning ----
      nlooplight=49
    }

    
    # Natural mortality ----
    if (nrow(colonymap) > 0) {
      probremoveeach = rep(background.mort , length(colonymap$ft))
      probremoveeach[colonymap$ft==1] = background.mort2 #encrusting
      whichremove = runif(length(probremoveeach)) < probremoveeach
      colonyremove = subset(colonymap, whichremove  ) # remove rows where csf > disturbance intensity # csf is its vulnerability. higher is more vulnerable.
      colonymap = subset(colonymap, !whichremove ) 
      # set world to zero and dead to FALSE for these new cells
      # world contains the colony id. If colonyid is in colonyremove world should be set to zero
      for (i in colonyremove$colonyid) {
        print(paste("natural mortality removing", i))
        dead[world == i] = 0 # set dead world to zero where disturbed colonies have been removed
        world[world == i] = 0 # set world to zero where disturbed colonies have been removed
      }
      if (nrow(colonyremove )>0){
        thisfatesrec = colonyremove 
        thisfatesrec$fate="random"
        thisfatesrec$deathtime=ts
        fatesrec = rbind(fatesrec ,thisfatesrec )
        nlooplight = 50
      }
    }
    
    
    
    # Disturbance low intensity fixed OR random ----
  if ((randomdist == "fixed" & ts%%freq.low == 0 & nrow(colonymap) >0 ) | (randomdist != "fixed" & runif(1) < (1/freq.low) & nrow(colonymap) >0) ) { # run this code when timestep is a multiple of set fixed frequency, or in the random case, make a random selection around this
      print(paste("low intensity ", randomdist))
      colonymap$csf = NA
      for (i in colonymap$colonyid) {  # loop through colony ids, these are what is stored in the world
        profile = (apply(world == i, c(2,3), any))
        howmanyeachlayer = sapply(1:ws, function(i) sum(profile[,i]))
        theintegral = sum(howmanyeachlayer * 1:ws)
        basal = (world[,,1] == i)
        d1 = max(rowSums(basal))
        d2 = max(colSums(basal))
        colonymap$csf[colonymap$colonyid == i] = 16/(d1^2*d2*pi)*theintegral
      }
      print(colonymap$csf)
      whichremove = sample(disturbance.intensity.low, 1) < colonymap$csf
      colonyremove = subset(colonymap, whichremove) # remove rows where csf > disturbance intensity # csf is its vulnerability. higher is more vulnerable.
      colonymap = subset(colonymap, !whichremove) 
      colonymap$csf = NULL
      print(nrow(colonymap))
      for (i in colonyremove$colonyid) {
        dead[world == i] = 0 # set dead world to zero where disturbed colonies have been removed
        world[world == i] = 0 # set world to zero where disturbed colonies have been removed
      }
      if (nrow(colonyremove )>0){
        thisfatesrec = colonyremove[,1:9] 
        thisfatesrec$fate="low"
        thisfatesrec$deathtime=ts
        fatesrec = rbind(fatesrec ,thisfatesrec )
        nlooplight = 50 # colonies have been removed to update light 50 times to account for this.
      }
    }
      
      
      
      # Disturbance high intensity fixed OR random ----
  if ((randomdist == "fixed" & (ts+freq.high/2)%%freq.high == 0 & nrow(colonymap) >0) | (randomdist != "fixed" & runif(1) < (1/freq.high) & nrow(colonymap) >0) ) {
 # high intensity disturbances are set to occur at halfway intervals between the low intensity for fixed case
        print(paste("high intensity",randomdist ))
        colonymap$csf = NA
        for (i in colonymap$colonyid) {  # loop through colony ids, these are what is stored in the world
          profile = (apply(world == i, c(2,3), any))
          howmanyeachlayer = sapply(1:ws, function(i) sum(profile[,i]))
          theintegral = sum(howmanyeachlayer * 1:ws)
          basal = (world[,,1] == i)
          d1 = max(rowSums(basal))
          d2 = max(colSums(basal))
          colonymap$csf[colonymap$colonyid == i] = 16/(d1^2*d2*pi)*theintegral
        }
        whichremove = sample(disturbance.intensity.high,1) < colonymap$csf
        colonyremove = subset(colonymap, whichremove) # remove rows where csf > disturbance intensity # csf is its vulnerability. higher is more vulnerable.
        colonymap = subset(colonymap, !whichremove) 
        colonymap$csf = NULL
        for (i in colonyremove$colonyid) {
        dead[world == i] = 0 # set dead world to zero where disturbed colonies have been removed
        world[world == i] = 0 # set world to zero where disturbed colonies have been removed
        }

        if (nrow(colonyremove )>0){
          thisfatesrec = colonyremove[,1:9] 
          thisfatesrec$fate="low"
          thisfatesrec$deathtime=ts
          fatesrec = rbind(fatesrec ,thisfatesrec )
          nlooplight = 50
      }
	}
      
    
  
  # Plot and save rgl within loop? ----
    #Note this will slow the simulation a lot
  if (draw == 1 & run == 1) { # if you want to plot in 3D
    #par3d(userMatrix = uM3, zoom=1)
    loc = which(world > 0,arr.ind = TRUE)
    ids = world[which(world>0)]
    ifdead = dead[which(world>0)]
    ftss = unlist(sapply(ids , function(thisid) {
      colonymap$ft[colonymap$colonyid == thisid]
    }))
    spheres3d(loc[,1],loc[,2],loc[,3],color=colpal[ftss], alpha=5)
    shade3d(qmesh3d(verticesfloor, indices),color='black')
    shade3d(qmesh3d(verticesback, indices),color='darkgrey')
    shade3d(qmesh3d(verticesside, indices),color='grey')
    text3d(-6,-6,0,paste('Time step', ts))
    if (save3D == 1) {
      rgl.snapshot(paste(version, ts, "final.png"))
    }
    rgl.close()
  }
  
    # Plot and save scatterplot within loop? ----
  if (drawscatter == 1 & run == 1) { 
    loc = which(world > 0,arr.ind = TRUE)
    ids = world[which(world>0)]
    ifdead = dead[which(world>0)]
    ftss = unlist(sapply(ids , function(thisid) {
      colonymap$ft[colonymap$colonyid == thisid]
    }))
    ftss[ifdead==1] = 16
    png(paste(timesteps + 1000, ".png"), width = 20, height = 20, units = "cm", res = 200) #change ts to timesteps to save ONLY final plot   
    scatterplot3d(
      loc[,1], loc[,2], loc[,3],
      color=colpal2[ftss], ##[coralpolyps$ft],
      zlim=c(0,ws), xlim = c(0,ws), ylim = c(0,ws),
      pch = 16) 
    dev.off()
  }
    
    
    
# Base R plots in each timestep to track progress ----
	par(mfrow=c(2,2))
	image(light[,,1],zlim=c(0,1), main = "light on seafloor")
	image(dead[,,1], main = "dead cells on seafloor")
	image(world[,,1], main = "live corals on seafloor")
coversummary = with(ftypessaveall,tapply(cover,list(c.1.nfts...dead..,timestep),mean))
dim(coversummary )
matplot(t(coversummary/100),t='l',xlab = "timestep", ylab  ="cover")
legend('topleft',c(as.character(ftypes$names),'dead'),col=1:6,lty=1:6, cex = 0.3)
 print(colonymap) 
 
 ### output colony structure for new metrics
 if (ts%%52==0) save(light,dead,world , file = paste("worlds",ts,run))

  
}# End of simulation loop

# Save scores of cells, cover and no. colonies
 write.csv(ftypessaveall, paste("run", run, ".csv", sep = ""), row.names =F)
paste("ftypessaveall exported")
} # End of multiple runs loop
