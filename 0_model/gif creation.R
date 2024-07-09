library(magick)
library(dplyr)
library(gtools)
library(stringr)

timesteps = 52*5
runs = 1
ws=100

#### Anna's directories
# setwd("C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_Animations/plots for animations/NL-IH")
# setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/SCIENCE ARCHIVE/PHD/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_Animations/plots for animations/NL-NH")

#### Daph's directories
work.dir = paste("~/GitHub/Coralcraft")
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
script = paste(met.wd,"1_MetricsScripts", sep="/") 
output = paste(met.wd, "1_Outputs", sep="/") 
plots = paste(work.dir,"1_Simulations", "1_Plots", "2_Scenarios", sep="/")

#### read in scenario file ####
setwd(work.dir)
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth forms.csv")

#set new working directories for specific scenario
sc.lab = as.vector(subset(scens.id, scenario %in% c("bra"))) ##CHANGE SCENARIO HERE
id = sc.lab$id
scens = sc.lab$scenario
name = sc.lab$names
foldername = paste(id, scens, timesteps, "tss", runs, "runs", "2023-02-06", sep = "_") #CHANGE DATE HERE
readfile = paste(sim.wd, foldername, sep="/")
setwd(readfile)

#### only need to do once #####
# file.rename(list.files(pattern = "00."), str_replace(list.files(pattern = "00."),pattern = "00.", "00"))
###############################
img <- list.files(pattern=".png")
img <- mixedsort(img)

gif <- img %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=20) # animates, can opt for number of loops
  
setwd(preso)
image_write(gif, "1_preso.gif")  # write to current dir


  