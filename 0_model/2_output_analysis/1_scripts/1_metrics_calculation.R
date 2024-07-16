# Code to calculate metrics from simulation output ("world files")

# this script was used to analyse output files from structural complexity and shelter investigation (Oh et al. 2024)

#################################
version = paste(Sys.Date())
timesteps = 52*5
runs = 100

library(tidyverse)

## set work directory
work.dir = setwd("./Coralcraft/0_model") #set work directory to location where 0_model is 
sim.wd = paste(work.dir, "1_simulation_output", sep="/") #for model simulation output
out.wd = paste(work.dir,"2_output_analysis", sep="/") #for output analysis
script = paste(out.wd,"1_scripts", sep="/") 
output = paste(out.wd, "2_output", sep="/") 
setwd(work.dir)

#####################################################################################################
#### read in scenario files ####
source(paste0(script, "/metrics_code.R"))
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth_forms.csv")
###################################################################################################

### DIVERSITY COMMUNITY TYPES 
# 1_max.div  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("max.div"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-13", sep = "_") #CHANGE DATE ACCORDING TO OUTPUT FOLDER
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

result_list <- NULL # make empty data frame
for (run in seq(1,runs)){
  
  for (ts in c(1, seq(13, timesteps, by = 13))) {
    df <- data.frame(run = run,
                     ts = ts,
                     id = paste(id, scens, sep="_"),
                     scenarios = name,
                     coral_cover = coralcover(run,ts),
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
df_1_max.div = result_list

# 2_Simple  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("simple"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_2_simple = result_list

# 3_Complex  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("complex"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_3_complex = result_list


#####################################################################################################

### MONOSPECIFIC COMMUNITY TYPES
# 4_encrusting  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("encrusting"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_4_enc = result_list

# 5_hemispherical  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("hemispherical"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_5_hem = result_list

# 6_digitate  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("digitate"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_6_dig = result_list

# 7_corymbose  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("corymbose"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_7_cor = result_list

# 8_tabular  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("tabular"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_8_tab = result_list

# 9_mushroom  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("mushroom"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_9_mus = result_list
 
# 10_columnar  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("columnar"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_10_col = result_list

# 11_foliose  ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("foliose"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_11_fol = result_list

# 12_bushy ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("bushy"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_12_bus = result_list

# 13_branching ----
sc.lab = as.vector(subset(scens.id, scenario %in% c("branching"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$labels
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
                     coral_cover = coralcover(run,ts),
                     linear_rugosity = lin_rugosity(run,ts),
                     surface_rugosity = sur_rugosity(run,ts),
                     fractal_dimension = fracdim(run,ts), 
                     shelter_volume = shelt_vol(run,ts),
                     demersal_shelter = dem_shelt(run,ts),
                     pelagic_shelter_1_15 = pel_shelter_1_15(run,ts),
                     pelagic_shelter_2 = pel_shelter_2(run, ts),
                     size_dependent = size_dep(run,ts))
    result_list = rbind(result_list, df)
  }
  print(run)
}
df_13_bra = result_list

#####################################################################################################

df_metrics = rbind(df_1_max.div, df_2_simple, df_3_complex)
df_metrics_1fts = rbind(df_4_enc, df_5_hem, df_6_dig, df_7_cor, df_8_tab,
                   df_9_mus, df_10_col, df_11_fol, df_12_bus, df_13_bra) # monospecific community types only

# saving outputs
setwd(output)
write.csv(df_metrics, file = "df_metrics.csv", row.names = F)
write.csv(df_metrics_1fts, file = "df_metrics_1fts.csv", row.names = F)




