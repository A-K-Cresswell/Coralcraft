#### Rugosity

## linear rugosity 
## surface rugosity 

# this includes metric code and reading world files
###############################################################################################
timesteps = 52*5
runs = 100
ws=100

library(animation)
library(scatterplot3d)
library(rgl)
library(magick)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(png)
library(raster)
library(ggpubr)

# Set your working directory
hd = paste("D:/GitHub/Coralcraft") #to read in data from hard drive
work.dir = paste(getwd())
sim.wd = paste(hd,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
script = paste(met.wd,"1_MetricsScripts", sep="/") 
output = paste(met.wd, "1_Outputs", sep="/") 
###############################################################################################
setwd(work.dir)
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth forms.csv")
###############################################################################################

## METRICS CODE ##

#### linear rugosity ----
lin_rugosity = function(run, ts){
  load(file = paste(id, scens, "worlds", ts, run, sep="_"))

  world2 = apply(world,c(1,2),function (x) max(which(x>0)))
  world2[world2 == -Inf] = 0
  sum(abs(diff(world2[,ws/2])))+ws 
}

#### surface rugosity metric code ----
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
###################################################################################################

### Load Individual Scenarios ####

# 1_ALL 10  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("all.10"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))) {
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run,ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_1_all.10 = result_list

# 2_bottom_5  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("bot.5"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug =lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_2_bot.5 = result_list

# 3_top_5  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("top.5"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run,ts), 
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_3_top.5 = result_list


#####################################################################################################

### 1 functional type only (20 colonies of 1 fts) [run #9-13 then bind with other file]
# 4_enc  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("enc"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_4_enc = result_list

# 5_fle  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("fle"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_5_fle = result_list

# 6_dig  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("dig"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_6_dig = result_list

# 7_cor  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("cor"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-15", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_7_cor = result_list

# 8_tab  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("tab"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-15", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_8_tab = result_list

# 9_mus  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("mus"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-15", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_9_mus = result_list

# 10_fin  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("fin"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-15", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_10_fin = result_list

# 11_con  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("con"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-15", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_11_con = result_list

# 12_hed ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("hed"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-15", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_12_hed = result_list

# 13_bra ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("bra"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-17", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,100)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))){
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     lin_rug = lin_rugosity(run, ts),
                     sur_rug = sur_rugosity(run, ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_13_bra = result_list

#####################################################################################################

df_metrics = rbind(df_1_all.10, df_2_bot.5, df_3_top.5)
df_metrics = rbind(df_4_enc, df_5_fle, df_6_dig, df_7_cor, df_8_tab,
                   df_9_mus, df_10_fin, df_11_con, df_12_hed, df_13_bra) #1 ft only

# saving outputs
setwd(output)
write.csv(df_metrics, file = "2_df_rugosity.csv", row.names = F)
################################################################

