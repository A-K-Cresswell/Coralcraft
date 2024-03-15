## Functional-structural coral model (FSCM) ---- temporal dynamics: metrics over time
rm(list=ls())
# COMPARING SCENARIOS

### for presentation - all.10, tabular, branching

#######################################################################################
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
library(tidyverse)

## set working directory
work.dir = getwd()
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
script = paste(met.wd,"1_MetricsScripts", sep="/")
output = paste(met.wd, "1_Outputs", sep="/")
paper = paste(work.dir, "3_paper", sep = "/")
preso = paste(work.dir, "4_preso", sep="/")

## load csv files & plotting parameters
setwd(script)
source("plot_par.R")
source("read_output.R")

#### DATAFRAME COMBINED ----
# LINEAR RUGOSITY ----
df0 = rbind(df1, df2)

df00 = df0 %>% 
  filter(scenarios %in% c("MaxDiv", "Tabular", "Branching"))

df.top <- df00[, names(df00) %in% c("run", "ts", "year", "id", "scenarios", "meantop_2", "meantop_10", "meantop_1_15", "meantop_1_20")]

df.long <- df.top %>% 
  gather(key=height, value=meanshelter, meantop_2:meantop_1_20) %>% 
  mutate(height = replace(height, height == "meantop_2", "2cm"),
         height = replace(height, height == "meantop_10", "10cm"),
         height = replace(height, height == "meantop_1_15", "1-15cm"),
         height = replace(height, height == "meantop_1_20", "1-20cm"))
df.long$height = factor(df.long$height, 
                         levels = c("1-15cm", "1-20cm", "2cm", "10cm"))
df.long = df.long %>%
  filter(height%in%c("1-15cm", "2cm", "10cm"))

juv0 = rbind(juv1_long,juv2_long)

juv00 = juv0 %>% 
  filter(scenarios %in% c("MaxDiv", "Tabular", "Branching"))

lr = ggplot()+
  stat_summary(data = df00, aes(x=year, y=lin_rugos, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = df00, aes(x=year, y=lin_rugos, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype00, name = "Diversity scenarios")+
  scale_size_manual(values = linesize00, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal00, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal00, name = "Diversity scenarios")+
  labs(x = "Year")+
  labs(y = "Linear rugosity")+
  # ggtitle("Linear Rugosity")+
  coord_cartesian(ylim=c(1,6))+
  scale_y_continuous(limits = c(1,6), breaks = seq(0,6, by = 1))+
  theme1 
  # theme(axis.title.x = element_blank())

ggsave(plot = lr, "00_linear.rugosity.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")

# SURFACE RUGOSITY ----
sr = ggplot()+
  stat_summary(data = df00, aes(x=year, y=sur_rugos, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = df00, aes(x=year, y=sur_rugos, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype00, name = "Diversity scenarios")+
  scale_size_manual(values = linesize00, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal00, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal00, name = "Diversity scenarios")+
  labs(x = "Year")+
  labs(y = "Surface rugosity")+
  # ggtitle("Linear Rugosity")+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
  theme1 
  # theme(axis.title.x = element_blank())

##SAVING
ggsave(plot = sr, "00_surface.rugosity.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")

# FRACTAL DIMENSION ----
fd = ggplot()+
  stat_summary(data = df00, aes(x=year, y=fracdim, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = df00, aes(x=year, y=fracdim, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype00, name = "Diversity scenarios")+
  scale_size_manual(values = linesize00, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal00, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal00, name = "Diversity scenarios")+
  labs(x = "Year")+
  labs(y = "Fractal dimension")+
  coord_cartesian(ylim = c(1,3))+
  # ggtitle("Fractal Dimension")+
  # scale_y_continuous(limits = c(1,3))+
  # scale_x_continuous(labels = NULL)
  theme1
ggsave(plot = fd, "00_fractal.dimension.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")

####################################################################################################################
###### SHELTER METRICS
# SHELTER VOLUME ----
sv = ggplot()+
  stat_summary(data = df00, aes(x=year, y=sheltervol, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = df00, aes(x=year, y=sheltervol, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype00, name = "Diversity scenarios")+ 
  scale_size_manual(values = linesize00, name = "Diversity scenarios")+ 
  scale_colour_manual(values = colpal00, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal00, name = "Diversity scenarios")+
  labs(x = "Year")+
  labs(y = expression(paste("Total volume ( ",dm^3,")", sep = "")))+
  # ggtitle("Shelter Volume")+
  scale_y_continuous(limits = c(0,140), breaks = seq(0,140, by = 20))+
  theme1 
  theme(axis.title.x=element_blank())
# scale_x_continuous(labels = NULL)
ggsave(plot = sv, "00_shel.vol.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")

# SHELTER FROM DEMERSAL PREDATORS (SIDE) ----
ds = ggplot()+
  stat_summary(data = df00, aes(x=year, y=meansides, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = df00, aes(x=year, y=meansides, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype00, name = "Diversity scenarios")+ #only for 1fts
  scale_size_manual(values = linesize00, name = "Diversity scenarios")+ #only for 1fts
  scale_colour_manual(values = colpal00, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal00, name = "Diversity scenarios")+
  labs(x = "Year")+
  labs(y = "Mean percent shelter")+
  # ggtitle("Demersal Shelter")+
  coord_cartesian(y=c(0,22))+
  theme1 
  theme(axis.title.x=element_blank())
# scale_y_continuous(limits = c(0,22), breaks = seq(0,20, by = 5))+
# scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))
ggsave(plot = ds, "00_demersal.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")

# SHELTER FROM PELAGIC PREDATORS ----
ps = ggplot()+
  stat_summary(data = df.long, aes(x=year, y=meanshelter, colour=scenarios , group=scenarios, 
                                    linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=F) +
  stat_summary(data = df.long, aes(x=year, y=meanshelter, fill=scenarios , group=scenarios,
                                    linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  # facet_grid(col = vars(height), scales = "free_y")+
  scale_linetype_manual(values=linetype00, name = "Diversity scenarios")+
  scale_size_manual(values = linesize00, name = "Diversity scenarios")+ 
  scale_colour_manual(values = colpal00, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal00, name = "Diversity scenarios")+
  labs(x = "Year",
       y = "Mean percent shelter")+
  coord_cartesian(ylim = c(0,100))+
  # ggtitle("Pelagic Shelter")+
  theme1

# scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))+
# scale_x_continuous(labels = NULL)
ggsave(plot = ps, "00_pelagic.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")


#################################################################################################
## Size-dependent shelter
## 1 . DIVERSITY ----
j1 = ggplot()+
  stat_summary(data = juv00, aes(x=year, y=mres, colour=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = juv00, aes(x=year, y=mres, fill=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  # facet_wrap(predsize~preysize, nrow=1)+
  scale_linetype_manual(values=linetype00)+
  scale_size_manual(values = linesize00)+
  scale_colour_manual(values = colpal00) +
  scale_fill_manual(values = colpal00)+
  labs(x = "Year")+
  labs(y = "Mean percent shelter")+
  # ggtitle("For 3cm juvenile fish")+
  theme1+
  theme(legend.position = "right")+
  coord_cartesian(ylim = c(0,40))
  # theme(axis.text.x = element_blank())
  
  ggsave(plot = j1, "00_size.png", path = preso, bg="white", dpi = 300, width = 20, height = 15, units = "cm")



