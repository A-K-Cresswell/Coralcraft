#### SHELTER METRICS PLOTTING ####

## METRICS
# Juvenile Protection

## SCENARIOS ##
# 1. Diversity (All 10, More Complex, Less Complex)
# 2. Monospecific Communities
# 3. Functional Redundancy
####################################################################
timesteps = 52*5
runs = 100
ws=100

library(animation)
library(scatterplot3d)
library(rgl)
library(magick)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(png)
library(raster)
library(ggpubr)
library(tidyverse)

## set working directory
work.dir = getwd()
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
plots = paste(met.wd,"1_MetricsPlots", sep="/") #for saving plots
output = paste(met.wd, "1_Outputs", sep="/") #for saving outputs
script = paste(met.wd,"1_MetricsScripts", sep="/") #for saving plots
juv.pro = paste(met.wd, "1_Outputs", "1_juv_pro", sep="/") 

## plots work dir
sc = paste(plots, "1_structuralcomplexity", sep="/")
shel = paste(plots, "2_sheltermetrics", sep="/")
cor = paste(plots, "3_corplot", sep="/")

## load csv files & plotting parameters
setwd(script)
source("plot_par.R")
source("read_output.R")
#################################################################################################
## 1 . DIVERSITY ----
juv1_long <- juv1 %>% 
  gather(key=predsize, value=mres, juv_1_9:juv_3_21) %>% 
  mutate(preysize = ifelse(predsize == "juv_1_9", "Small Prey", 
                           ifelse(predsize == "juv_1_21", "Small Prey", "Medium Prey"))) %>% 
  mutate(predsize = replace(predsize, predsize == "juv_1_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_1_21", "Medium Predator"),
         predsize = replace(predsize, predsize == "juv_3_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_3_21", "Medium Predator")) %>% 
  mutate(mres=mres*100)
juv1_long$predsize = factor(juv1_long$predsize, 
                         levels = c("Small Predator", "Medium Predator"))
juv1_long$preysize = factor(juv1_long$preysize, 
                            levels = c("Small Prey", "Medium Prey"))
juv1_long$scenarios = unlist(juv1_long$scenarios) 

# JUVENILE PROTECTION ----
ggplot()+
  stat_summary(data = juv1_long, aes(x=ts, y=mres, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = juv1_long, aes(x=ts, y=mres, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  facet_wrap(predsize~preysize)+
  scale_linetype_manual(values=linetype1)+
  scale_size_manual(values = linesize1)+
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Timestep")+
  labs(y = "Mean % safe space for prey")+
  # ggtitle("For 3cm juvenile fish")+
  theme_bw()+
  coord_cartesian(ylim = c(0,40))

ggsave("1_juv.prot.png", path = shel,
       width=22, height=15, units="cm", dpi=300)

#################################################################################################
## 2 . 1 FTS ONLY 
juv2_long <- juv2 %>% 
  gather(key=predsize, value=mres, juv_1_9:juv_3_21) %>% 
  mutate(preysize = ifelse(predsize == "juv_1_9", "Small Prey", 
                           ifelse(predsize == "juv_1_21", "Small Prey", "Medium Prey"))) %>% 
  mutate(predsize = replace(predsize, predsize == "juv_1_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_1_21", "Medium Predator"),
         predsize = replace(predsize, predsize == "juv_3_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_3_21", "Medium Predator")) %>% 
  mutate(mres=mres*100)
juv2_long$predsize = factor(juv2_long$predsize, 
                            levels = c("Small Predator", "Medium Predator"))
juv2_long$preysize = factor(juv2_long$preysize, 
                            levels = c("Small Prey", "Medium Prey"))
juv2_long$scenarios = unlist(juv2_long$scenarios) 

# JUVENILE PROTECTION ----
ggplot()+
  stat_summary(data = juv2_long, aes(x=ts, y=mres, colour=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = juv2_long, aes(x=ts, y=mres, fill=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  facet_grid(preysize~predsize)+
  scale_linetype_manual(values=linetype2[2:13])+
  scale_size_manual(values = linesize2[2:13])+
  scale_colour_manual(values = colpal2[2:13]) +
  scale_fill_manual(values = colpal2[2:13])+
  labs(x = "Timestep")+
  labs(y = "Mean % safe space for prey")+
  # ggtitle("Juvenile Protection")+
  theme_bw()+
  coord_cartesian(ylim = c(0,45))

ggsave("2_juv.prot_1fts.v2.png", path = shel,
       width=22, height=15, units="cm", dpi=300)

################################################################################################








