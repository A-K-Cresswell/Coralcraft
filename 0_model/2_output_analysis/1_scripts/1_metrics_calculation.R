# Code to calculate metrics from simulation output ("world files")

# this script was used to analyse output files from structural complexity and shelter investigation (Oh et al. 2025)

# this script reads output files saved in 1_simulation_output folder. 
# output folders should be named in the following format: [community type id]_[community type]_[timesteps]_tss_[no. of runs]_run

# LAST UPDATED: 24-03-2025
# Author: Daphne Oh

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
# load coral community types as vector
comm.type = as.vector(scens.id$scenario)

for (k in comm.type){
  
  # Create a label for community type
  sc.lab = as.vector(subset(scens.id, scenario %in% k)) ##CHANGE SCENARIO HERE
  id = sc.lab$id
  scens = sc.lab$scenario
  name = sc.lab$labels
  
  # read in data from folder
  foldername = paste(id, scens, timesteps, "tss", runs, "runs", sep = "_") #CHANGE DATE HERE
  readfile = paste(sim.wd, foldername, sep="/")
  setwd(readfile)
  
  result_list <- NULL # make empty data frame
  
  # calculate metrics for each world file
  for (run in seq(1,100)){
    
    for (ts in c(1, seq(13, timesteps, by = 13))) {
      df <- data.frame(run = run,
                       ts = ts,
                       id = paste(id, scens, sep="_"),
                       scenarios = name,
                ### BELOW CODES ARE FOR FUNCTIONS FROM metrics_code.R
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
  
  name = paste("df",id, scens, sep="_")
  name <- result_list
  
}

######################################################################################################################

df_metrics = rbind(df_1_max.div, df_2_simple, df_3_complex)
df_metrics_1fts = rbind(df_4_encrusting, df_5_hemispherical, df_6_digitate, df_7_corymbose, df_8_tabular,
                   df_9_mushroom, df_10_columnar, df_11_foliose, df_12_bushy, df_13_branching) # monospecific community types only

# saving outputs
setwd(output)
write.csv(df_metrics, file = "df_metrics.csv", row.names = F)
write.csv(df_metrics_1fts, file = "df_metrics_1fts.csv", row.names = F)




