# Coralcraft - A Functional-Structural Coral Model ----
# Model Code
# This code works with R Studio v4.0.0-4.4.3

# Authors
# Anna K Cresswell
# Daphne Oh
# Michael Renton


# Set your working directory to where the Coralcraft GitHub folder is

# for model simulation output
sim.wd = paste("0_model/1_simulation_output", sep="/")
out.wd = paste("0_model/2_output_analysis", sep="/") # for output analysis


# For output file folder naming ----
version = paste(Sys.Date())

# Libraries ----
library(rgl)
library(scatterplot3d)

# 1. SET PARAMETERS ----

## 1a. Simulation Parameters ----
runs = 1 # how many times to run the simulation
# the number of timesteps in each simulation, e.g. 52 weeks * 5 years
timesteps = 52*5
wx = 300 # world size in x direction (cm)
wy = 200 # world size in y direction (cm)
wz = 100 # world size in z direction / depth
mindepth = 0 #(m) # used to calc top of world light level
# how many corals to initialise simulation - initial size is one voxel
# (i.e. 1cm x 1cm x 1cm)
n.initial.colonies = 20
# if set to 1, random allocation of functional types to the initial
# colonies, else ordered (cycles through the included functional types in
# order)
randominitial = 0

## 1b. Spawning Parameters ----
# if set to 1, random allocation of growth forms upon spawning, else
# allocation a probability of the total number of live cells of each growth
# form
randomrecruits = 1
# how frequently (in timesteps) does spawning occur (set to high number,
# e.g. 99999 for no spawning)
spawn.freq = 52
nnewrecruits = 5 # how many new corals each spawn

## 1c. Disturbance Parameters ----
randomdist = "fixed" # 'fixed' or 'random' disturbance intensity and frequency
# disturbance intensity
# how intense is the disturbance (a smaller number is a bigger disturbance)
if(randomdist == "fixed") {
  disturbance.intensity.low = c(20,20,20)
} else {
  disturbance.intensity.low = exp(rnorm(1000,log(20),0.5))
}
# how intense is the disturbance (a smaller number is a bigger disturbance)
if(randomdist == "fixed") {
  disturbance.intensity.high = c(1.5,1.5,1.5)
} else {
  disturbance.intensity.high = exp(rnorm(1000,log(1.5),0.5))
}

# disturbance frequency
# 99999 for no disturbances OR number of timesteps between disturbances
freq.low = 99999
# 99999 for no disturbances OR number of timesteps between disturbances
freq.high = 350

## 1d. Background Mortality Parameters ----
background.mort = 0 # base probability 99% chance of surviving a year
# lower probability, 95%, for encrusting due to being on the seafloor
# (scouring etc)
background.mort2 = 0

## 1e. Light And Resources Parameters ----
maint = 0.1
surface.light.level = 1
k = log(1)-log(0.72) # k = 0.3285041
# light level at min depth
light.level = surface.light.level * (exp(1))^(-k*mindepth)
light.side = 0.5 # proportion of light travelling side wards
# for k =0.64 - 0.9908789 # light.atten = 0.9908789^100 = 0.40000 OR
# 0.4^(1/100)
light.atten = 0.72^(1/100)
# resource cap: this means a colony can store maximum 6 units of energy
res.cap = 6
start.res = 1 # resources each colony starts with
#0.938005^(1/100)
#Light equation
#Light_depth = Light_surface x e^(-k*depth)

## 1f. Maximum Colony Size Per Functional Type ----
# max radius (cm) a colony can grow to from its recruit point; use Inf for
# no limit
# order matches functional type list in
# GitHub/Coralcraft/0_model/1_coral_morphology_architecture.R: encrusting,
# flexihemispherical, digitate, corymbose, tabular, mushroom, columnar,
# foliose, bushy, branching
maxradius = c(150, 75, 30, 30, 70, 250, 50, 50, 30, 60)

# 2. BUILD GROWTH FORMS ----
## need to run this once to load the growth forms
source('0_model/1_coral_morphology_architecture.R')

# 3. SPECIFY SCENARIOS ----
scens.id = read.csv("0_model/scenarios_id.csv") # load scenarios/community types
# load morphological composition of scenarios
scens.csv = read.csv("0_model/scenarios.csv", header=T)
# id/name lookup for functional types, derived from ftnames (set in
# 1_coral_morphology_architecture.R) so there is only one place that defines
# functional type order/names - growth_forms.csv is no longer used for this
# (it duplicated the same list)
fts = data.frame(id = 1:length(ftnames), name = ftnames)

## choose which functional types to include, in one of two ways:
## (a) set selected.fts below to a vector of names, e.g.
##     c("encrusting","tabular") - bypasses the scenario csv entirely
## (b) leave selected.fts as NULL to use a named scenario from
##     scenarios_id.csv / scenarios.csv instead (as before)
# e.g. c("encrusting", "tabular"), or NULL to use the scenario csv below
selected.fts = NULL

if (!is.null(selected.fts)) {
  labels = data.frame(name = selected.fts)
  id = "custom"
  scens = paste(selected.fts, collapse="-")
} else {
  ## select scenario
  ## change scenarios here
  sc.label = as.vector(subset(scens.id, scenario %in% c("max.div")))
  id = sc.label$id
  scens = sc.label$scenario

  ## select growth forms from scenarios
  sel.fts <- scens.csv[scens]
  sel.fts <- as.numeric(na.omit(as.vector(unlist(sel.fts))))
  labels = as.vector(subset(fts, id %in% sel.fts))
}

# 4. CREATE SAVE FOLDER ----
# make new folder with name giving parameters for automatically saving outputs
dir.create(paste0(sim.wd, "/", id, scens, timesteps, "tss", runs, "runs",
                   version))
foldername = paste0(sim.wd, "/", id, scens, timesteps, "tss", runs, "runs",
                     version)
# note, sometimes the long name strings can cause issues, so remove
# information not needed if start getting errors here


# 5. SET UP PLOTTING & WORLD GEOMETRY ----

## 5a. Plotting Parameters ----
# if set to 1, will plot in 3D each timestep - not currently set up (see
# figure script)
draw = 1
# if set to 1, will save 3D plot in each timestep - not currently set up
# (see figure script)
save3D = 1
# will plot and save a scatterplot - not currently set up (see figure
# script)
drawscatter = 0
# increase size of rgl window for better resolution when saving with the
# latter two numbers
r3dDefaults$windowRect = c(50,50,750,750)

# To save with new orientation of rgl window:
# Open rgl and move to desired orientation then save with below
# uM <- par3d()$userMatrix
# write.csv(uM, "uM.csv", row.names = F) # should only need to do this once

## 5b. RGL View Orientation ----
r1 = c(0.70,-0.70,0.01,0)
r2 = c(0.25,0.25,0.93,0)
r3 = c(-0.65, -0.65,0.37,1)
r4 = c(0,0,0,0)
uM3 = rbind(r1,r2,r3,r4)
colnames(uM3) = c("V1", "V2", "V3", "V4")
rownames(uM3) = c("[1,]", "[2,]", "[3,]", "[4,]")

## 5c. World Wall Geometry (For Plotting) ----
indices <- c(1, 2, 3, 4)
verticesfloor <- c(0, 0, 0, 1.0,  wx, 0, 0, 1.0,  wx, wy, 0, 1.0,  0, wy, 0,
                    1.0)
verticesback  <- c(0, wy, 0, 1.0,  wx, wy, 0, 1.0,  wx, wy, 50, 1.0,  0, wy,
                    50, 1.0)
verticesside  <- c(wx, wy, 0, 1.0,  wx, 0, 0, 1.0,  wx, 0, 50, 1.0,  wx, wy,
                    50, 1.0)

## 5d. Initial Light Profile ----
# sets up initial light through the world: z depth is wz, x-y layer size is
# wx*wy
light = array(rep(light.level * light.atten^((wz-1):0), each=wx*wy),
               dim=c(wx,wy,wz))

# image(light[50,,])
# depthp = as.data.frame(seq(-0.01, -10, by =-0.01))
# lightp = as.data.frame(rep(1 * 0.9967203^(seq(1, 1000, by =1))))
# lightplot = cbind(depthp, lightp)
# colnames(lightplot) = c("depth", "light")
# ggplot(lightplot, aes(y = depth, x = light)) +
#   geom_point(shape = 1, colour = "blue") +
#   xlab("Light") +
#   ylab("Depth(m)") +
#   scale_y_continuous(breaks =seq(-0.01, -11, by =-1),
#     labels = seq(0, 10, by =1)) +
#   scale_x_continuous(breaks = seq(0, 1, by =0.1),
#     labels = seq(0.0, 1.0, by =0.1), limits = c(0,1))
# ggsave("Light profile k=0.064.png", width = 13, height = 13, units = "cm")
# # dpi = 300))


# =================================================================
# MODEL SIMULATION ----
# =================================================================

for (run in 1:runs){ # multiple simulation runs

  ## Set Initial Conditions ----
  world = array(0,dim=c(wx,wy,wz)) # create world
  dead = array(0,dim=c(wx,wy,wz)) # create 'dead' world
  # sets up initial light through the world
  light = array(rep(light.level * light.atten^((wz-1):0), each=wx*wy),
                 dim=c(wx,wy,wz))
  # randomly select locations for the initial colonies
  init.locations = sample(1:(wx*wy), n.initial.colonies)
  rows = (init.locations-1)%%wx + 1
  cols = (init.locations-rows)/wx + 1
  # assign colony id to location in world
  for (i in 1:n.initial.colonies) world[rows[i], cols[i], 1] = i

  # dataframe for saving the number of colonies, percentage cover, number of
  # voxels, linear and surface rugosity in each timestep
  ftypessaveall = NULL

  ## Set Up Functional Types Information ----
  # set number of functional groups (e.g. tabular, columnar, massive,
  # encrusting)
  nfts = length(ftcelllist)
  # ftnames is defined in 1_coral_morphology_architecture.R - reused here so
  # the functional type name list only exists in one place
  ftypes = data.frame(names = ftnames)
  ftypes$ftnum = 1:nrow(ftypes)
  ftypes$resource.to.growth = 1 # this can be modified to change growth rates
  # names of the functional types included in this run (from the scenario
  # csv, or selected.fts if set in section 3)
  ftsinc_list = labels$name

  # subset ftypes down to just the included functional types
  ftsinc = subset(ftypes, names %in% ftsinc_list)
  # nftsinc = nrow(ftsinc) # unused - not read anywhere else in the script

  ## Make The Colony Map ----
  ftlist = as.list(ftsinc$ftnum)
  colonymap = data.frame(
    colonyid = 1:n.initial.colonies,
    # RANDOM or ORDERED, set by randominitial above
    ft = if (randominitial == 1) {
      sample(ftsinc$ftnum, n.initial.colonies, replace = TRUE)
    } else {
      rep(ftsinc$ftnum, length.out = n.initial.colonies)
    },
    age = 1,
    res = start.res)

  colonymap$row = rows
  colonymap$col = cols
  colonymap$bd = 1
  colonymap$sz = 1

  fatesrec = list()
  nlooplight = 50
  #--------------------------------------------------------------------------

  ## Run Simulation Timesteps ----
  for (ts in 1:timesteps){
    print(paste(version, ",", randomdist, ",", timesteps, "tss,", runs,
                "runs,", spawn.freq, "spawn.freq,", nnewrecruits,
                "nnewrecruits,", randomrecruits, "randomrecruits", freq.low,
                freq.high, "disturbance frequencies, ", background.mort,
                "background mort"))
    print(paste("there are", nrow(colonymap), "colonies in the world",
                sep = " "))
    print(paste('run', run))
    print(paste('time step', ts))

    dist_text = "" # text to be added to plot (kept for console/log readability)
    # clean disturbance label shown as its own italic, non-bold line in the
    # rgl plot (set below if a disturbance occurs this timestep)
    dist_label = ""

    ### Light ----
    for (iii in 1:nlooplight){
      light[,,1:(wz-1)] = light[,,2:wz] * light.atten
      lighttoside = light[,,1:(wz-1)] * light.side
      lighttoside1 = lighttoside[c(2:wx,1),,1:(wz-1)] * 0.25
      lighttoside2 = lighttoside[c(wx,1:(wx-1)),,1:(wz-1)] * 0.25
      lighttoside3 = lighttoside[,c(2:wy,1),1:(wz-1)] * 0.25
      lighttoside4 = lighttoside[,c(wy,1:(wy-1)),1:(wz-1)] * 0.25
      light[,,1:(wz-1)] = light[,,1:(wz-1)] - lighttoside + lighttoside1 +
        lighttoside2 + lighttoside3 + lighttoside4
      light[,,wz] = light.level

      ### Light Uptake ----
      lightuptake = array(0,dim=c(wx,wy,wz)) # make light uptake world
      # fill with light level only where there are coral cells
      lightuptake[world > 0 & !dead] = light[world > 0 & !dead]
      # set light world to zero where there are coral cells (dead or alive)
      light[world!=0] = 0

    } ## end the light loop

    # reset number of times to update light to 1, we want to increase this
    # if a colony dies
    nlooplight = 1

    ### Resources Per Colony ----
    # only run if there are at least 1 colony in the world
    if (nrow(colonymap) > 0) {
      for (i in colonymap$colonyid) { # take each colony separately
        ii = which(colonymap$colonyid == i)
        # update amount of resources
        colonymap$res[ii] = colonymap$res[ii] +
          sum(lightuptake[world == i & !dead] - maint)
        # cap resources
        if (colonymap$res[ii] > res.cap & colonymap$ft[ii]!=5) {
          colonymap$res[ii] = res.cap
        }
        # cap resources for tabulars
        if (colonymap$res[ii] > res.cap & colonymap$ft[ii]==5) {
          colonymap$res[ii] = 2*res.cap
        }

      }
      # colonymap$res[colonymap$res>res.cap] = res.cap # cap resources

      ### Death ----
      # voxel dies if not getting enough light to maintain itself
      dead[lightuptake < maint & world > 0] = TRUE
      colonymap$stillalive = TRUE
      for (i in colonymap$colonyid) { # take each colony separately
        allcellsdead = all(dead[world == i] == 1)
        if (allcellsdead){
          # set dead world to zero where disturbed colonies have been removed
          dead[world == i] = 0
          # set world to zero where disturbed colonies have been removed
          world[world == i] = 0
          colonymap$stillalive[which(colonymap$colonyid==i)] = FALSE
          print(paste("colony ID",i,"dies from light starvation"))
          thisfatesrec = colonymap[which(colonymap$colonyid==i),]
          thisfatesrec$fate = "light"
          thisfatesrec$deathtime = ts
          fatesrec = rbind(fatesrec ,thisfatesrec)
          nlooplight = 50
        }
      }

      # drop the fully dead colonies out of the colony map
      colonymap = subset(colonymap, stillalive)

      ### Growth ----
      ncolonies = nrow(colonymap)
      if (ncolonies > 0) {
        if (ncolonies == 1) {
          colonygrowthorder = 1
        } else {
          colonygrowthorder = sample(1:ncolonies)
        }
        for (i in colonygrowthorder) {
          thiscolony = colonymap[i, ]
          ngrowth = floor(thiscolony$res /
            ftsinc$resource.to.growth[which(ftsinc$ftnum == thiscolony$ft)])

          if (ngrowth > 0) {
            #print(paste("ngrowth:",ngrowth))
            thisid = thiscolony$colonyid
            currentcells = which(world==thisid & !dead,arr.ind=TRUE)
            up = down = left = right = back = front = currentcells
            down[,3] = down[,3]-1
            up[,3] = up[,3]+1
            left[,2] = left[,2]-1
            right[,2] = right[,2]+1
            back[,1] = back[,1]+1
            front[,1] = front[,1]-1
            down = down[down[,3]>0,]
            up = up[up[,3]<=wz,]
            left[,2][left[,2]==0] = wy
            right[,2][right[,2]>wy] = 1
            front[,1][front[,1]==0] = wx
            back[,1][back[,1]>wx] = 1
            potneighbs = unique(rbind(up,down,left,right,back,front))
            ifempty = world[potneighbs]==0
            neighbs = matrix(potneighbs[ifempty,],ncol=3)
            # row is x (wx), col is y (wy)
            neighbsinlist = neighbs-matrix(
              c(thiscolony$row-wx/2,thiscolony$col-wy/2,0),
              nrow=nrow(neighbs),ncol=3,byrow=TRUE)
            # mod matches this axis's own size (wx), so wrap-around is
            # correct even when wx != wy
            neighbsinlist[,1] = (neighbsinlist[,1] - 1)%%wx + 1
            # same for y (wy)
            neighbsinlist[,2] = (neighbsinlist[,2] - 1)%%wy + 1
            priorities = ftcelllist[[thiscolony$ft]][neighbsinlist]
            npot = sum(priorities>0)
            if (npot<=ngrowth) {    ## grow all potentials
              use = which(priorities>0)
            } else {   ## grow the highest priority ngrowth potentials
              ord = order(priorities, y=rnorm(length(priorities)),
                decreasing=TRUE)
              use = ord[1:ngrowth]
            }
            newcells = neighbs[use,]
            world[matrix(newcells,ncol=3)] = thisid
          }}}

      # update age of all colonies
      # all colonies age each timestep, even if they didn't grow
      colonymap$age = colonymap$age + 1
    } # end if statement for if there are no colonies in colonymap

    ### Save Information About World Occupation By Each Functional Type ----
    ftypessave = data.frame(c(1:nfts,"dead")) # each functional type
    ftypessave$run = run # which model run is it
    ftypessave$cells = NA # column for number of voxels
    ftypessave$timestep = ts # column for the timestep
    ftypessave$cover = NA # column for % cover
    ftypessave$no.colonies = NA # column for number of colonies
    ftypessave$rugosity = NA # column for rugosity
    ftypessave$SA = NA

    flatworld = matrix(0,nrow=wx,ncol=wy)
    for (iii in 1:wx) for (jjj in 1:wy) {
      cc = world[iii,jjj,]
      occup = which(cc>0)
      if (length(occup)>0) {
        flatworld[iii, jjj] = cc[max(occup)]
        if (dead[iii, jjj, max(occup)] == 1) flatworld[iii, jjj] = -1
      }
    }

    flatworlddead = matrix(0,nrow=wx,ncol=wy)
    for (iii in 1:wx) for (jjj in 1:wy) {
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
        ftypessave$cover[i] = sum(flatworld %in% t1$colonyid & !dead) /
          (wx*wy)*100
      }
      else {
        ftypessave$cells[i] = 0
        ftypessave$cover[i] = 0
      }
    }

    ### Calculate Total Surface Area ----
    currentcells = which(world!=0,arr.ind=TRUE)
    up = down = left = right = back = front = currentcells
    down[,3] = down[,3]-1
    up[,3] = up[,3]+1
    left[,2] = left[,2]-1
    right[,2] = right[,2]+1
    back[,1] = back[,1]+1
    front[,1] = front[,1]-1
    down = down[down[,3]>0,]
    up = up[up[,3]<=wz,]
    left[,2][left[,2]==0] = wy
    right[,2][right[,2]>wy] = 1
    front[,1][front[,1]==0] = wx
    back[,1][back[,1]>wx] = 1
    potneighbs = rbind(up,down,left,right,back,front)
    ifempty = world[potneighbs]==0
    totalSA = sum(ifempty)

    emptyz1 = sum(world[,,1]==0)
    totalSA = totalSA + emptyz1 # add on empty z = 1 (seafloor cells)

    ftypessave$cells[which(ftypessave$c.1.nfts...dead.. == "dead")] =
      sum(dead > 0)
    ftypessave$cover[which(ftypessave$c.1.nfts...dead.. == "dead")] =
      sum(flatworld %in% 1)/(wx*wy)*100
    ftypessave$no.colonies[which(ftypessave$c.1.nfts...dead.. =="dead")] =
      sum(colonymap$stillalive == F)

    ### Calculate Linear Rugosity Of World In This Timestep ----
    htworld = apply(world,c(1,2),function (x) max(which(x>0)))
    htworld[htworld == -Inf] = 0
    # htworld is wx rows x wy cols - loop over each y-column transect (wy
    # of them), baseline per-transect length is wx
    thisrugos = mean(sapply(1:wy,  function(i) sum(abs(diff(htworld[,i])))+wx))

    ftypessave$rugosity = thisrugos
    ftypessave$totalSA = totalSA

    ftypessaveall = rbind(ftypessaveall, ftypessave)

    ### Spawning ----
    # spawning offset to occur in half years + 6 timesteps
    if ((ts+spawn.freq/2+6)%%spawn.freq == 0) {
      print("SPAWNING")
      # random x y locations for new recruit
      reclocations = sample(1:(wx*wy) , nnewrecruits)
      rows = (reclocations-1)%%wx + 1
      cols = (reclocations-rows)/wx + 1

      nactnewrecruits = 0
      for (i in 1:nnewrecruits) {
        # if there isn't already a coral there
        if (world[rows[i], cols[i], 1] == 0) {
          nactnewrecruits = nactnewrecruits + 1 # then we get +1 new recruit
          print("recruitment!!")
          if (nrow(colonymap) > 0) {
            newcolID = max(colonymap$colonyid) + 1
          } else {
            # to account for if it is the first spawn and there are no
            # other colonies
            newcolID = 1
          }
          # populate world with new colonyID
          world[rows[i], cols[i], 1] = newcolID
          # select fts
          if (randomrecruits == 1) {
            # if there is only one ft choose that one
            if (length(ftsinc$ftnum) == 1) {
              colonymapnew = data.frame(
                colonyid = newcolID,
                ft = ftsinc$ftnum,
                age = 1,
                res = start.res) # resources
            }
            # else sample from the available fts - had to add the other
            # if() for when there is only one colony type
            if (length(ftsinc$ftnum) > 1) {
              colonymapnew = data.frame(
                colonyid = newcolID,
                ft = sample(ftsinc$ftnum, 1),
                age = 1,
                res = start.res) # resources
            }
          } else {
            # if there is only one ft choose that one
            if (length(ftsinc$ftnum) == 1) {
              colonymapnew = data.frame(
                colonyid = newcolID,
                ft = ftsinc$ftnum,
                age = 1,
                res = start.res) # resources
            }
            # else sample from the available fts
            if (length(ftsinc$ftnum) > 1) {
              colonymapnew = data.frame(
                colonyid = newcolID,
                ft = sample(ftsinc$ftnum, 1,
                            prob = ftypessave$cells[ftsinc$ftnum]),
                age = 1,
                res = start.res) # resources
            }
          }

          colonymapnew$row = rows[i]
          colonymapnew$col = cols[i]
          colonymapnew$bd = ts
          colonymapnew$sz = 1
          colonymapnew$stillalive = TRUE

          colonymap = rbind(colonymap,colonymapnew)
        }
      }
      print(paste(nactnewrecruits ,"actual new recruits"))

      # update light post spawning
      nlooplight = 49
    }

    ### Natural Mortality ----
    if (nrow(colonymap) > 0) {
      probremoveeach = rep(background.mort , length(colonymap$ft))
      probremoveeach[colonymap$ft==1] = background.mort2 #encrusting
      whichremove = runif(length(probremoveeach)) < probremoveeach
      # remove rows where csf > disturbance intensity # csf is its
      # vulnerability. higher is more vulnerable.
      colonyremove = subset(colonymap, whichremove)
      colonymap = subset(colonymap, !whichremove)
      # set world to zero and dead to FALSE for these new cells
      # world contains the colony id. If colonyid is in colonyremove world
      # should be set to zero
      for (i in colonyremove$colonyid) {
        print(paste("natural mortality removing", i))
        # set dead world to zero where disturbed colonies have been removed
        dead[world == i] = 0
        # set world to zero where disturbed colonies have been removed
        world[world == i] = 0
      }
      if (nrow(colonyremove)>0){
        thisfatesrec = colonyremove
        thisfatesrec$fate = "random"
        thisfatesrec$deathtime = ts
        fatesrec = rbind(fatesrec ,thisfatesrec)
        nlooplight = 50
      }
    }

    ### Disturbance - Low Intensity (Fixed Or Random) ----
    # run this code when timestep is a multiple of set fixed frequency, or
    # in the random case, make a random selection around this
    if ((randomdist == "fixed" & ts%%freq.low == 0 & nrow(colonymap) > 0) |
        (randomdist != "fixed" & runif(1) < (1/freq.low) &
           nrow(colonymap) > 0)) {
      print(paste("low intensity ", randomdist))
      # update the text tracking label
      dist_text = " (low intensity disturbance)"
      dist_label = "Low intensity disturbance"
      colonymap$csf = NA
      # loop through colony ids, these are what is stored in the world
      for (i in colonymap$colonyid) {
        profile = (apply(world == i, c(2,3), any))
        howmanyeachlayer = sapply(1:wz, function(i) sum(profile[,i]))
        theintegral = sum(howmanyeachlayer * 1:wz)
        basal = (world[,,1] == i)
        d1 = max(rowSums(basal))
        d2 = max(colSums(basal))
        colonymap$csf[colonymap$colonyid == i] = 16/(d1^2*d2*pi)*theintegral
      }
      print(colonymap$csf)
      whichremove = sample(disturbance.intensity.low, 1) < colonymap$csf
      # remove rows where csf > disturbance intensity # csf is its
      # vulnerability. higher is more vulnerable.
      colonyremove = subset(colonymap, whichremove)
      colonymap = subset(colonymap, !whichremove)
      colonymap$csf = NULL
      print(nrow(colonymap))
      for (i in colonyremove$colonyid) {
        # set dead world to zero where disturbed colonies have been removed
        dead[world == i] = 0
        # set world to zero where disturbed colonies have been removed
        world[world == i] = 0
      }
      if (nrow(colonyremove)>0){
        thisfatesrec = colonyremove[,1:9]
        thisfatesrec$fate = "low"
        thisfatesrec$deathtime = ts
        fatesrec = rbind(fatesrec ,thisfatesrec)
        # colonies have been removed to update light 50 times to account
        # for this.
        nlooplight = 50
      }
    }

    ### Disturbance - High Intensity (Fixed Or Random) ----
    if ((randomdist == "fixed" & (ts+freq.high/2)%%freq.high == 0 &
         nrow(colonymap) > 0) |
        (randomdist != "fixed" & runif(1) < (1/freq.high) &
         nrow(colonymap) > 0)) {
      # high intensity disturbances are set to occur at halfway intervals
      # between the low intensity for fixed case
      print(paste("high intensity",randomdist))
      # update the text tracking label
      dist_text = " (high intensity disturbance)"
      dist_label = "High intensity disturbance"

      colonymap$csf = NA
      # loop through colony ids, these are what is stored in the world
      for (i in colonymap$colonyid) {
        profile = (apply(world == i, c(2,3), any))
        howmanyeachlayer = sapply(1:wz, function(i) sum(profile[,i]))
        theintegral = sum(howmanyeachlayer * 1:wz)
        basal = (world[,,1] == i)
        d1 = max(rowSums(basal))
        d2 = max(colSums(basal))
        colonymap$csf[colonymap$colonyid == i] = 16/(d1^2*d2*pi)*theintegral
      }
      whichremove = sample(disturbance.intensity.high,1) < colonymap$csf
      # remove rows where csf > disturbance intensity # csf is its
      # vulnerability. higher is more vulnerable.
      colonyremove = subset(colonymap, whichremove)
      colonymap = subset(colonymap, !whichremove)
      colonymap$csf = NULL
      for (i in colonyremove$colonyid) {
        # set dead world to zero where disturbed colonies have been removed
        dead[world == i] = 0
        # set world to zero where disturbed colonies have been removed
        world[world == i] = 0
      }

      if (nrow(colonyremove)>0){
        thisfatesrec = colonyremove[,1:9]
        thisfatesrec$fate = "high"
        thisfatesrec$deathtime = ts
        fatesrec = rbind(fatesrec ,thisfatesrec)
        nlooplight = 50
      }
    }

    ### Plot And Save RGL Within Loop ----
    # note this will slow the simulation a lot
    if (draw == 1 & run == 1) { # if you want to plot in 3D
      # par3d(userMatrix = uM)
      loc = which(world > 0,arr.ind = TRUE)
      ids = world[which(world>0)]
      ifdead = dead[which(world>0)]
      ftss = unlist(sapply(ids , function(thisid) {
        colonymap$ft[colonymap$colonyid == thisid]
      }))

      # 1. clear previous frame
      clear3d()

      # 2. grab your working base view and cleanly spin it 90 degrees
      # around the Z-axis
      # (pi / 2 radians equals exactly 90 degrees)
      rotated_view = rotate3d(par3d("userMatrix"), pi/4, 0, 0, 1)
      par3d(userMatrix = rotated_view)

      # 3. render elements
      spheres3d(loc[,1],loc[,2],loc[,3],color=colpal[ftss], alpha=5)
      shade3d(qmesh3d(verticesfloor, indices),color='black')
      shade3d(qmesh3d(verticesback, indices),color='darkgrey')
      shade3d(qmesh3d(verticesside, indices),color='grey')

      # 4. text overlays
      week_in_year = ((ts-1) %% 52) + 1
      year_num = (ts-1) %/% 52
      timelabel = paste0("Year: ", year_num, ", Week: ", week_in_year)
      text3d(-6,-6,0, timelabel, font = 2) # bold
      # italic, non-bold, only shown when a disturbance occurred this
      # timestep
      if (dist_label != "") text3d(-6,-6,-12, dist_label, font = 3)
      # italic
      text3d(60, 100, 130, "Coralcraft", cex = 1.5, color = "black", font = 3)

      if (save3D == 1) {
        rgl.snapshot(paste0(foldername, "/00", ts, ".png"))
      }
      rgl.close()
    }

    ### Plot And Save Scatterplot Within Loop ----
    if (drawscatter == 1 & run == 1) {
      loc = which(world > 0,arr.ind = TRUE)
      ids = world[which(world>0)]
      ifdead = dead[which(world>0)]
      ftss = unlist(sapply(ids , function(thisid) {
        colonymap$ft[colonymap$colonyid == thisid]
      }))
      ftss[ifdead==1] = 16
      # change ts to timesteps to save ONLY final plot
      png(paste(ts + 1000, ".png"), width = 20, height = 20, units = "cm",
          res = 200)
      scatterplot3d(
        loc[,1], loc[,2], loc[,3],
        color=colpal2[ftss],
        zlim=c(0,wz), xlim = c(0,wx), ylim = c(0,wy),
        pch = 16)
      dev.off()
    }

    ### Base R Plots In Each Timestep (Track Progress) ----
    par(mfrow=c(2,2))
    image(light[,,1],zlim=c(0,1), main = "light on seafloor")
    image(dead[,,1], main = "dead cells on seafloor")
    image(world[,,1], main = "live corals on seafloor")
    coversummary = with(ftypessaveall, tapply(cover,
      list(c.1.nfts...dead.., timestep), mean))
    dim(coversummary)
    matplot(t(coversummary/100),t='l',xlab = "timestep", ylab = "cover")
    legend('topleft', c(as.character(ftypes$names),'dead'), col=1:6,
           lty=1:6, cex = 0.3)
    print(colonymap)

    ### Save World Structure (For Further Analysis) ----
    # save first timestep world files
    if (ts == 1) {
      save(light,dead,world,
           file = paste0(foldername, "/", id, scens, "worlds",ts,run))
    }
    # save world file every 13 timesteps
    if (ts%%13==0) {
      save(light,dead,world,
           file = paste0(foldername, "/", id, scens, "worlds",ts,run))
    }

  } # End of simulation loop

  ## Save Scores Of Cells, Cover And No. Colonies ----
  write.csv(ftypessaveall,
            paste0(foldername, "/", id, scens, "run", run, ".csv"),
            row.names = F)
  paste("ftypessaveall exported")
} # End of multiple runs loop
