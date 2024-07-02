
#r3dDefaults$windowRect = c(50,50,700,700) 
#uM2 <- read.csv("uM.csv")
#uM3 <- data.matrix(uM2)
oopt = ani.options(interval = .02)
lcol <- colorRampPalette(c('black', 'blue', 'cadetblue1'))
lcol1 <- lcol(100)
verticesfloor <- c(  0, 0, 0, 1.0, 100, 0, 0, 1.0, 100,  100, 0, 1.0, 0,  100, 0, 1.0)
verticesback <- c(  0,  100, 0, 1.0,100,  100, 0, 1.0, 100, 100, 100, 1.0, 0, 100, 100, 1.0)
verticesside <- c( 100,  100, 0, 1.0, 100,  0, 0, 1.0, 100, 0, 100, 1.0,100, 100, 100, 1.0)
indices <- c( 1, 2, 3, 4 )
dirs = matrix(c(1, 0,-1, 0, 0, 0, 0,1,0,-1,0, 0, 0,0,0,0,1, -1), ncol = 3) # AKC? Is this used anymore
dirsmat = matrix(dirs,ncol=3) # AKC? Is this used anymore 
dirsmat2 = matrix(unlist(expand.grid(dx=-1:1,dy=-1:1,dz=-1:1)), ncol = 3) # AKC? Is this used anymore
# open3d()
# shade3d(qmesh3d(verticesfloor, indices),color='black')
# shade3d(qmesh3d(verticesback, indices),color='darkgrey')
# shade3d(qmesh3d(verticesside, indices),color='grey')
# # Open rgl and move to desired orientation then save with below
# uM <- par3d()$userMatrix
# write.csv(uM, "uM.csv", row.names = F)
# r3dDefaults$windowRect = c(50,50,700,700) 
# uM2 <- read.csv("uM.csv")
# uM3 <- data.matrix(uM2)


  # Draw ----
  loc = which(world > 0,arr.ind = TRUE)
  if (draw == 1 & dim(loc)[1] > 0) {
  ids = world[which(world>0)]
  ifdead = dead[which(world>0)]
  ftss = unlist(sapply(ids , function(thisid) {
      colonymap$ft[colonymap$colonyid==thisid]
      }))
  ftss[ifdead==1]=6
  scatterplot3d(
    loc[,1],loc[,2],loc[,3],
    #color=c(col2,col1,'red',col3)[fts2],
    color=c('red','blue','orange','green','purple','grey')[ftss], ##[coralpolyps$ft],
    zlim=c(0,maxht),xlim = c(0,ws),ylim = c(0,ws),
    pch = 16)
}

  # Draw and save 3D ----
  if (save3D == 1) {
    open3d()
    par3d(userMatrix = uM3, zoom=1)
    loc = which(world > 0,arr.ind = TRUE)
    ids = world[which(world>0)]
    ifdead = dead[which(world>0)]
    ftss = unlist(sapply(ids, function(thisid) {
      colonymap$ft[colonymap$colonyid == thisid]
    }))
    ftss[ifdead == 1] = 6
    spheres3d(loc[,1],loc[,2],loc[,3],color=rainbow(13)[ftss], alpha=5)#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
    shade3d(qmesh3d(verticesfloor, indices),color='black')
    shade3d(qmesh3d(verticesback, indices),color='darkgrey')
    shade3d(qmesh3d(verticesside, indices),color='grey')
    text3d(-6,-6,0,paste('Time step', ts))
    rgl.snapshot(paste('171205_image',1000 + ts, '.png'))
    rgl.close()
  }

    # Draw ----
    loc = which(world > 0,arr.ind = TRUE)
    if (draw == 1 & dim(loc)[1] > 0) {
      ids = world[which(world>0)]
      ifdead = dead[which(world>0)]
      ftss = unlist(sapply(ids , function(thisid) {
        colonymap$ft[colonymap$colonyid == thisid]
      }))
      ftss[ifdead==1] = 6
      scatterplot3d(
        loc[,1], loc[,2], loc[,3],
        #color=c(col2,col1,'red',col3)[fts2],
        color = c('red','blue','orange','green','purple','grey')[ftss], ##[coralpolyps$ft],
        zlim = c(0,maxht), xlim = c(0,ws), ylim = c(0,ws),
        pch = 16)
    }
    # Draw and save 3D ----
    if (save3D == 1) {
      open3d()
      par3d(userMatrix = uM3, zoom=1)
      loc = which(world > 0,arr.ind = TRUE)
      ids = world[which(world>0)]
      ifdead = dead[which(world>0)]
      ftss = unlist(sapply(ids, function(thisid) {
        colonymap$ft[colonymap$colonyid == thisid]
      }))
      ftss[ifdead == 1] = 6
      spheres3d(loc[,1],loc[,2],loc[,3],color=c("red", "blue", "orange","green",'purple','grey')[ftss], alpha=5)#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
      shade3d(qmesh3d(verticesfloor, indices),color='black')
      shade3d(qmesh3d(verticesback, indices),color='darkgrey')
      shade3d(qmesh3d(verticesside, indices),color='grey')
      text3d(-6,-6,0,paste('Time step', ts, 'REPRODUCTION'))
      rgl.snapshot(paste('171205_image',1000 + ts, '.png'))
      rgl.close()
    }



    # Draw ----
    loc = which(world > 0,arr.ind = TRUE)
    if (draw == 1 & dim(loc)[1] > 0) {
      ids = world[which(world>0)]
      ifdead = dead[which(world>0)]
      ftss = unlist(sapply(ids , function(thisid) {
        colonymap$ft[colonymap$colonyid == thisid]
      }))
      ftss[ifdead==1] = 6
      scatterplot3d(
        loc[,1], loc[,2], loc[,3],
        #color=c(col2,col1,'red',col3)[fts2],
        color = c('red','blue','orange','green','purple','grey')[ftss], ##[coralpolyps$ft],
        zlim = c(0,maxht), xlim = c(0,ws), ylim = c(0,ws),
        pch = 16)
    }




