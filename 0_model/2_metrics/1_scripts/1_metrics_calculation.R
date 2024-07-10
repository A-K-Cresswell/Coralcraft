# Code to calculate metrics from simulation output ("world files")

#################################
version = paste(Sys.Date())
timesteps = 52*5
runs = 100

work.dir = paste("~") #set main work directory (i.e. 0_model)
sim.wd = paste(work.dir,"1_simulations", sep="/") #set simulation wd
sim.output = paste(sim.wd,"3_output", sep="/") #directory to world files
met.wd = paste(work.dir,"2_metrics", sep="/") #set metrics wd
met.script = paste(met.wd,"1_scripts", sep="/") #metrics script
met.output = paste(met.wd, "2_output", sep="/") #metrics output

setwd(met.script) 
source("metrics_code.R")
#####################################################################################################
#### read in scenario files ####
setwd(work.dir)
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth forms.csv")
###################################################################################################

### I WONDER IF I SHOULD LIST OUT ALL SCENARIOS AND SHOW THAT I HAD COMBINED ALL DF USING RBIND??

### DIVERSITY COMMUNITY TYPES 
# 1_ALL 10  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("all.10"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE HERE ACCORDING TO SIM.OUTPUT
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,runs)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))) {
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts)
                     )
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_3_top.5 = result_list


#####################################################################################################

### MONOSPECIFIC COMMUNITY TYPES
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
                     # numcoveredcells= getnumcoveredcells(run, ts),
                     # meansides = getnumcoveredcellssides(run, ts)/ws/ws,
                     # meantop = meantopview(run,ts),
                     # meantop_1 = meantopview_1(run,ts),
                     # meantop_5 = meantopview_5(run,ts),
                     # meantop_10 = meantopview_10(run,ts),
                     meantop_1_15 = meantopview_1_15(run,ts),
                     meantop_1_20 = meantopview_1_20(run,ts))
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
setwd(met.output)
write.csv(df_metrics, file = "df_metrics.csv", row.names = F)




