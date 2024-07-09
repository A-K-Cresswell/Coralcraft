#### PAPER PLOTTING ####

## STRUCTURAL COMPLEXITY METRICS
# Linear Rugosity 
# Surface Rugosity 
# Fractal Dimensions 

## SHELTER METRICS
# Shelter Volume
# Shelter from Demersal Predators
# Shelter from Transient Predators (Overall, height at 2cm, height at 5cm, height at 10cm)
# Juvenile Protection

## SCENARIOS ##
# 1. Diversity (All 10, More Complex, Less Complex)
# 2. Monospecific Communities
####################################################################
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


## load csv files & plotting parameters
setwd(script)
source("plot_par.R")
source("read_output.R")

################################################################################################################
###### STRUCTURAL COMPLEXITY 
### 1. DIVERSITY 
# LINEAR RUGOSITY ----
p1 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=lin_rugos, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df1, aes(x=year, y=lin_rugos, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  # labs(x = "Year")+
  labs(y = "Linear rugosity")+
  # ggtitle("Linear Rugosity")+
  coord_cartesian(ylim=c(1,6))+
  theme1 +
  theme(axis.title.x = element_blank())
              
# scale_y_continuous(breaks = seq(1,6, by = 1))+
# scale_x_continuous(labels = NULL)

# SURFACE RUGOSITY ----
p11 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=sur_rugos, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df1, aes(x=year, y=sur_rugos, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Surface rugosity")+
  # ggtitle("Surface Rugosity")+
  theme1 +
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))
# scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p111 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=fracdim, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df1, aes(x=year, y=fracdim, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Fractal dimension")+
  coord_cartesian(ylim = c(1,3))+
  # ggtitle("Fractal Dimension")+
  # scale_y_continuous(limits = c(1,3))+
  # scale_x_continuous(labels = NULL)
  theme1


### 2. 1FTS ONLY ----
# LINEAR RUGOSITY ----
p2 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=lin_rugos, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df2, aes(x=year, y=lin_rugos, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype2[2:11], name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2[2:11], name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2[2:11], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:11], name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  # ggtitle("Linear Rugosity")+
  theme2 +
  theme(axis.title.x = element_blank())
# coord_cartesian(ylim=c(1,6)) +
# scale_y_continuous(breaks = seq(1,6, by = 1))+
# scale_x_continuous(labels = NULL)


# SURFACE RUGOSITY ----
p22 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=sur_rugos, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = F) +
  stat_summary(data = df2, aes(x=year, y=sur_rugos, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype2[2:11], name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2[2:11], name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2[2:11], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:11], name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  # ggtitle("Surface Rugosity")+
  theme2 +
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))
# scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p222 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=fracdim, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df2, aes(x=year, y=fracdim, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype2[2:11], name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2[2:11], name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2[2:11], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:11], name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  coord_cartesian(ylim = c(1,3))+
  # ggtitle("Fractal Dimension")+
  theme2
# scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
# scale_x_continuous(labels = NULL)

############################
# SAVING ----
ggarrange(p1,p11,p111,
          nrow=3,ncol=1,
          common.legend = T,
          align = "hv",
          labels = c("A","B","C"),
          font.label = list(size = 16),
          legend = "bottom")

ggsave("1_sc1.v4.png", path = paper, bg = "white", dpi = 300, width = 6.5, height = 13.5)

ggarrange(p2,p22,p222,
              nrow=3,ncol=1,
          align = "hv",
              common.legend = T,
              legend = "bottom")
# ggarrange(A, B, 
#           nrow = 1, ncol = 2, 
#           align = "hv",
#           widths = c(1,1))

ggsave("1_sc2.v4.png", path = paper, bg = "white", dpi = 300, width = 6.5, height = 13.5)
################################################################################################################
###### SHELTER METRICS
### 1. DIVERSITY 
# SHELTER VOLUME ----
s1 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=sheltervol, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df1, aes(x=year, y=sheltervol, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+ 
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+ 
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = expression(paste("Total volume ( ",dm^3,")", sep = "")))+
  # ggtitle("Shelter Volume")+
  scale_y_continuous(limits = c(0,140), breaks = seq(0,140, by = 20))+
  theme1 +
  theme(axis.title.x=element_blank())
# scale_x_continuous(labels = NULL)

# SHELTER FROM DEMERSAL PREDATORS (SIDE) ----
s11 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=meansides, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df1, aes(x=year, y=meansides, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+ #only for 1fts
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+ #only for 1fts
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Mean percent shelter")+
  # ggtitle("Demersal Shelter")+
  coord_cartesian(y=c(0,22))+
  theme1 +
  theme(axis.title.x=element_blank())
# scale_y_continuous(limits = c(0,22), breaks = seq(0,20, by = 5))+
# scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# SHELTER FROM PELAGIC PREDATORS ----
s111 = ggplot()+
  stat_summary(data = df.long1, aes(x=year, y=meanshelter, colour=scenarios , group=scenarios, 
                                    linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=F) +
  stat_summary(data = df.long1, aes(x=year, y=meanshelter, fill=scenarios , group=scenarios,
                                    linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  facet_grid(col = vars(height), scales = "free_y")+
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+ 
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL,
       y = "Mean percent shelter")+
  coord_cartesian(ylim = c(0,100))+
  # ggtitle("Pelagic Shelter")+
  theme1
  
# scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))+
# scale_x_continuous(labels = NULL)

###########################
### 2. 1 FTS ONLY 
# SHELTER VOLUME ----
s2 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=sheltervol, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df2, aes(x=year, y=sheltervol, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2[2:11], name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2[2:11], name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2[2:11], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:11], name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  scale_y_continuous(breaks = seq(0,140, by = 20))+
  # ggtitle("Shelter Volume or Total Cover")+
  theme2 +
  theme(axis.title.x=element_blank())
  
# scale_x_continuous(labels = NULL)

# SHELTER FROM DEMERSAL PREDATORS (SIDE) ----
s22 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=meansides, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df2, aes(x=year, y=meansides, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2[2:11], name = "Monospecific scenarios")+ #only for 1fts
  scale_size_manual(values = linesize2[2:11], name = "Monospecific scenarios")+ #only for 1fts
  scale_colour_manual(values = colpal2[2:11], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:11], name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  # ggtitle("Shelter from Demersal Predators")+
  theme2 +
  theme(axis.title.x=element_blank())
# coord_cartesian(ylim = c(0,22))+
# scale_y_continuous(breaks = seq(0,20, by = 5))+
# scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# SHELTER FROM PELAGIC PREDATORS ----
s222 = ggplot()+
  stat_summary(data = df.long22, aes(x=year, y=meanshelter, colour=scenarios , group=scenarios, 
                                     linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=T) +
  stat_summary(data = df.long22, aes(x=year, y=meanshelter, fill=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, 
               colour = NA, show.legend = T) +
  facet_grid(col = vars(height))+
  scale_linetype_manual(values=linetype2[2:13], name = "Monospecific scenarios")+
  scale_size_manual(values = linesize2[2:13], name = "Monospecific scenarios")+
  scale_colour_manual(values = colpal2[2:13], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:13], name = "Monospecific scenarios")+
  labs(x = NULL,
       y = NULL)+
  # ggtitle("Shelter from Top")+
  coord_cartesian(ylim = c(0,100))+
  theme2

# SAVING----
ggarrange(s1,s11,s111,
          nrow=3,ncol=1,
          common.legend = T,
          labels = c("A","B","C"),
          font.label = list(size = 16),
          legend = "bottom")

ggsave("2_shelter.1.v4.png", path = paper, bg="white", dpi = 300, width = 6.5, height = 13.5)

ggarrange(s2,s22,s222,
              nrow=3,ncol=1,
              common.legend = T,
              legend = "bottom")

ggsave("2_shelter.2.v4.png", path = paper, bg="white", dpi = 300, width = 6.5, height = 13.5)


ggarrange(A, B, 
          nrow = 1, ncol = 2,
          widths = c(1,1),
          align = "hv")



ggsave("2_shelter.2.png", path = paper, bg="white", dpi = 300, width = 6.5, height = 13.5)
  

#################################################################################################
## Size-dependent shelter
## 1 . DIVERSITY ----
j1 = ggplot()+
  stat_summary(data = juv1_long, aes(x=year, y=mres, colour=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = juv1_long, aes(x=year, y=mres, fill=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  facet_wrap(predsize~preysize, nrow=1)+
  scale_linetype_manual(values=linetype1)+
  scale_size_manual(values = linesize1)+
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = NULL)+
  labs(y = NULL)+
  # ggtitle("For 3cm juvenile fish")+
  theme1+
  theme(legend.position = "right")+
  coord_cartesian(ylim = c(0,40))+
  theme(axis.text.x = element_blank())


## 2 . 1 FTS ONLY ----
j2 = ggplot()+
  stat_summary(data = juv2_long, aes(x=year, y=mres, colour=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = juv2_long, aes(x=year, y=mres, fill=scenarios , group=scenarios,
                                     linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  facet_wrap(predsize~preysize, nrow =1)+
  scale_linetype_manual(values=linetype2[2:13])+
  scale_size_manual(values = linesize2[2:13])+
  scale_colour_manual(values = colpal2[2:13]) +
  scale_fill_manual(values = colpal2[2:13])+
  labs(x = NULL)+
  labs(y = NULL)+
  # ggtitle("Juvenile Protection")+
  theme2+
  theme(legend.position = "right")+
  coord_cartesian(ylim = c(0,45))
 
# SAVING----
# ggarrange(j1, j2,
#           legend = "bottom",
#           labels = c("A", "B"),
#           nrow = 1, ncol = 2)
j1
# ggsave("3_juvprotection.1_v2.png", path = paper, bg="white", dpi = 300, width = 10, height = 10)
ggsave("3_juvprotection.1_v3.png", path = paper, bg="white", dpi = 300, width = 15, height = 5)

j2
ggsave("3_juvprotection.2_v3.png", path = paper, bg="white", dpi = 300, width = 15, height = 5)


# 
leg = get_legend(j2 + theme(legend.position = "bottom"))
ggsave("2_legend_bottom.png", plot=leg, path = paper, dpi = 300, width = 8.5, height = 2)


















