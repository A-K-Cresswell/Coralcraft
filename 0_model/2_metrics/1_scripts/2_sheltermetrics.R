#### SHELTER METRICS PLOTTING ####

## METRICS
# Shelter Volume
# Shelter from Demersal Predators
# Shelter from Transient Predators (Overall, height at 2cm, height at 5cm, height at 10cm)
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
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(png)
library(raster)
library(ggpubr)

## set working directory
work.dir = getwd()
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
plots = paste(met.wd,"1_MetricsPlots", sep="/") #for saving plots
output = paste(met.wd, "1_Outputs", sep="/") #for saving outputs
script = paste(met.wd,"1_MetricsScripts", sep="/") #for saving plots

## plots work dir
sc = paste(plots, "1_structuralcomplexity", sep="/")
shel = paste(plots, "2_sheltermetrics", sep="/")
cor = paste(plots, "3_corplot", sep="/")

## load csv files & plotting parameters
setwd(script)
source("plot_par.R")
source("read_output.R")
###############################################################################################
#### PLOTTING 
### 0. COMBINED (DIVERSITY + 1FTS) 
# SHELTER VOLUME ----
s0 = ggplot()+
  stat_summary(data = df00, aes(x=year, y=sheltervol, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df00, aes(x=year, y=sheltervol, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype0)+ 
  scale_size_manual(values = linesize0)+ 
  scale_colour_manual(values = colpal0) +
  scale_fill_manual(values = colpal0)+
  labs(x = "Year")+
  labs(y = expression(paste("Shelter Volume (",dm^3,")", sep = "")))+
  ggtitle("Shelter Volume")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))
# scale_y_continuous(limits = c(0,140), breaks = seq(0,140, by = 20))+
# scale_x_continuous(labels = NULL)

# SHELTER FROM DEMERSAL PREDATORS (SIDE) ----
s00 = ggplot()+
  stat_summary(data = df00, aes(x=year, y=meansides, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df00, aes(x=year, y=meansides, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype0)+ 
  scale_size_manual(values = linesize0)+ 
  scale_colour_manual(values = colpal0) +
  scale_fill_manual(values = colpal0)+
  labs(x = "Year")+
  labs(y = "Mean Percent Shelter")+
  ggtitle("Shelter from Demersal Predators")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))
# scale_y_continuous(limits = c(0,22), breaks = seq(0,20, by = 5))+
# scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# SHELTER FROM TRANSIENT PREDATORS (Overall, height at 2cm, height at 5cm, height at 10cm) ----
s000 = ggplot()+
  stat_summary(data = df.long0, aes(x=year, y=meanshelter, colour=scenarios , group=scenarios, 
                                   linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=F) +
  stat_summary(data = df.long0, aes(x=year, y=meanshelter, fill=scenarios , group=scenarios,
                                   linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  facet_grid(col = vars(height), scales = "free_y")+
  scale_linetype_manual(values=linetype0)+ 
  scale_size_manual(values = linesize0)+ 
  scale_colour_manual(values = colpal0) +
  scale_fill_manual(values = colpal0)+
  labs(x = "Year",
       y = "Mean Percent Shelter")+
  ggtitle("Shelter from Transient Predators")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=12),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
# scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))+
# scale_x_continuous(labels = NULL)

# SAVING----
# Shelter volume
ggsave("00_shelvol.png", plot = s0, path = plots,
       width=22, height=15, units="cm", dpi=300)

# Shelter from demersal predators
ggsave("00_shelside.png", plot = s00, path = plots,
       width=22, height=15, units="cm", dpi=300)

# Shelter from transient predators
ggsave("00_sheltop.png", plot = s000, path = plots,
       width=10, height=25, units="cm", dpi=300)

# Shelter from transient predators (hor)
ggsave("00_sheltop_hor.png", plot = s000, path = plots,
       width=20, height=10, units="cm", dpi=300)

###############################################################################################
### 1. DIVERSITY 
# SHELTER VOLUME ----
s1 = ggplot()+
  stat_summary(data = df11, aes(x=year, y=sheltervol, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df11, aes(x=year, y=sheltervol, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1)+ 
  scale_size_manual(values = linesize1)+ 
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Year")+
  labs(y = expression(paste("Shelter Volume (",m^3,") or Total Cover %", sep = "")))+
  ggtitle("Shelter Volume or Total Cover %")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  scale_y_continuous(limits = c(0,14), breaks = seq(0,14, by = 2))
  # scale_x_continuous(labels = NULL)

# SHELTER FROM DEMERSAL PREDATORS (SIDE) ----
s11 = ggplot()+
  stat_summary(data = df11, aes(x=year, y=meansides, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df11, aes(x=year, y=meansides, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1)+ #only for 1fts
  scale_size_manual(values = linesize1)+ #only for 1fts
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Year")+
  labs(y = "Mean Percent Shelter")+
  ggtitle("Shelter from Demersal Predators")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  coord_cartesian(y=c(0,22))
  # scale_y_continuous(limits = c(0,22), breaks = seq(0,20, by = 5))+
  # scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# SHELTER FROM TRANSITING PREDATORS ----
s111 = ggplot()+
  stat_summary(data = df.long1, aes(x=year, y=meanshelter, colour=scenarios , group=scenarios, 
                                   linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=F) +
  stat_summary(data = df.long1, aes(x=year, y=meanshelter, fill=scenarios , group=scenarios,
                                   linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  facet_grid(col = vars(height), scales = "free_y")+
  scale_linetype_manual(values=linetype1)+
  scale_size_manual(values = linesize1)+ 
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Year",
       y = "Mean Percent Shelter")+
  ggtitle("Shelter from Transient Predators")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=14),
      axis.text = element_text(size=14),
      axis.title = element_text(size=14))+
  coord_cartesian(ylim = c(0,100))
  # scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))+
  # scale_x_continuous(labels = NULL)

# SAVING----
# Shelter volume
ggsave("1_shelvol.v2.png", plot = s1, path = shel,
       width=22, height=15, units="cm", dpi=300)

# Shelter from demersal predators
ggsave("1_shelside.png", plot = s11, path = shel,
       width=22, height=15, units="cm", dpi=300)

# Shelter from transient predators
ggsave("1_sheltop_hor.png", plot = s111, path = shel,
       width=22, height=10, units="cm", dpi=300)

###############################################################################################
### 2. 1 FTS ONLY 
# SHELTER VOLUME ----
s2 = ggplot()+
  stat_summary(data = df22, aes(x=year, y=sheltervol, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df22, aes(x=year, y=sheltervol, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2[2:11])+ 
  scale_size_manual(values = linesize2[2:11])+ 
  scale_colour_manual(values = colpal2[2:11]) +
  scale_fill_manual(values = colpal2[2:11])+
  labs(x = "Year")+
  labs(y = expression(paste("Shelter Volume (",m^3,") ot Total Cover %", sep = "")))+
  ggtitle("Shelter Volume or Total Cover")+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  # coord_cartesian(ylim = c(0,14))
  scale_y_continuous(breaks = seq(0,14, by = 2))
  # scale_x_continuous(labels = NULL)

# SHELTER FROM DEMERSAL PREDATORS (SIDE) ----
s22 = ggplot()+
  stat_summary(data = df22, aes(x=year, y=meansides, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df22, aes(x=year, y=meansides, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2[2:11])+ #only for 1fts
  scale_size_manual(values = linesize2[2:11])+ #only for 1fts
  scale_colour_manual(values = colpal2[2:11]) +
  scale_fill_manual(values = colpal2[2:11])+
  labs(x = "Year")+
  labs(y = "Mean Percent Shelter")+
  ggtitle("Shelter from Demersal Predators")+
  theme_bw()+
  theme(axis.text = element_text(size=14))
  # coord_cartesian(ylim = c(0,22))+
  # scale_y_continuous(breaks = seq(0,20, by = 5))+
  # scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# SHELTER FROM TRANSITTING PREDATORS ----
s222 = ggplot()+
  stat_summary(data = df.long22, aes(x=year, y=meanshelter, colour=scenarios , group=scenarios, 
                                  linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=F) +
  stat_summary(data = df.long22, aes(x=year, y=meanshelter, fill=scenarios , group=scenarios,
                                  linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  facet_grid(col = vars(height))+
  scale_linetype_manual(values=linetype2[2:13])+
  scale_size_manual(values = linesize2[2:13])+
  scale_colour_manual(values = colpal2[2:13]) +
  scale_fill_manual(values = colpal2[2:13])+
  labs(x = "Year",
       y = "Mean Percent Shelter")+
  ggtitle("Shelter from Top")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))+
  coord_cartesian(ylim = c(0,100))

# SAVING----
# Shelter volume
ggsave("2_shelvol.v2.png", plot = s2, path = shel,
       width=22, height=15, units="cm", dpi=300)

# Shelter from demersal predators
ggsave("2_shelside.png", plot = s22, path = shel,
       width=22, height=15, units="cm", dpi=300)

# Shelter from transient predators
ggsave("2_sheltop_hor.png", plot = s222, path = shel,
       width=22, height=10, units="cm", dpi=300)

###############################################################################################
# # Overall ----
# a1=ggplot()+
#   stat_summary(data = df11, aes(x=year, y=meantop, colour=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=F) +
#   stat_summary(data = df11, aes(x=year, y=meantop, fill=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
#   scale_linetype_manual(values=linetype1)+
#   scale_size_manual(values = linesize1)+ 
#   scale_colour_manual(values = colpal1) +
#   scale_fill_manual(values = colpal1)+
#   labs(x = " ",
#        y = " ")+
#   ggtitle("Overall")+
#   theme_bw()+
#   theme(plot.title = element_text(face = "bold", size=14),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14))+
#   scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))+
#   scale_x_continuous(labels = NULL)
# 
# # Mean coverage from z=2 
# b1=ggplot()+
#   stat_summary(data = df11, aes(x=year, y=meantop_2, colour=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=T) +
#   stat_summary(data = df11, aes(x=year, y=meantop_2, fill=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
#   scale_linetype_manual(values=linetype1)+
#   scale_size_manual(values = linesize1)+ 
#   scale_colour_manual(values = colpal1) +
#   scale_fill_manual(values = colpal1)+
#   labs(x = " ")+
#   labs(y = " ")+
#   ggtitle("At Height of 2 cm")+
#   theme_bw()+
#   theme(plot.title = element_text(face = "bold", size=14),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14))+
#   scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20))+
#   scale_x_continuous(labels = NULL)
# 
# # Mean coverage from z=5 
# c1=ggplot()+
#   stat_summary(data = df11, aes(x=year, y=meantop_5, colour=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=T) +
#   stat_summary(data = df11, aes(x=year, y=meantop_5, fill=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
#   scale_linetype_manual(values=linetype1)+
#   scale_size_manual(values = linesize1)+ 
#   scale_colour_manual(values = colpal1) +
#   scale_fill_manual(values = colpal1)+
#   labs(x = " ")+
#   labs(y = " ")+
#   ggtitle("At Height of 5 cm")+
#   theme_bw()+
#   theme(plot.title = element_text(face = "bold", size=14),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14))+
#   scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20))+
#   scale_x_continuous(labels = NULL)
# 
# # Mean coverage from z=10 
# d1=ggplot()+
#   stat_summary(data = df11, aes(x=year, y=meantop_10, colour=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend=T) +
#   stat_summary(data = df11, aes(x=year, y=meantop_10, fill=scenarios , group=scenarios,
#                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
#   scale_linetype_manual(values=linetype1)+
#   scale_size_manual(values = linesize1)+ 
#   scale_colour_manual(values = colpal1) +
#   scale_fill_manual(values = colpal1)+
#   labs(x = "Year")+
#   labs(y = " ")+
#   ggtitle("At Height of 10 cm")+
#   theme_bw()+
#   theme(plot.title = element_text(face = "bold", size=14),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14))+
#   scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20))
# 
# #   Stitching it together ----
# s111 <- ggarrange(a1,
#                  b1,
#                  c1,
#                  d1,
#                  ncol=1, nrow=4,
#                  align = "hv")