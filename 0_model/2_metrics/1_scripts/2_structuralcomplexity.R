#### STRUCTURAL METRICS PLOTTING ####

## METRICS
# Linear Rugosity 
# Surface Rugosity 
# Fractal Dimensions 

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
script = paste(met.wd,"1_MetricsScripts", sep="/")

## plots work dir
sc = paste(plots, "1_structuralcomplexity", sep="/")
shel = paste(plots, "2_sheltermetrics", sep="/")

## load csv files & plotting parameters
setwd(script)
source("plot_par.R")
source("read_output.R")
################################################################################################################
#### PLOTTING 

### 0. COMBINED (DIVERSITY + 1FTS) 
# LINEAR RUGOSITY ----
p0 = ggplot()+
  stat_summary(data = df0, aes(x=year, y=linear_rugosity, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df0, aes(x=year, y=linear_rugosity, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype0)+
  scale_size_manual(values = linesize0)+
  scale_colour_manual(values = colpal0) +
  scale_fill_manual(values = colpal0)+
  labs(x = "Year")+
  labs(y = "Linear Rugosity")+
  ggtitle("Linear Rugosity")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  coord_cartesian(ylim=c(1,6))
# scale_y_continuous(breaks = seq(1,6, by = 1))+
# scale_x_continuous(labels = NULL)

# SURFACE RUGOSITY ----
p00 = ggplot()+
  stat_summary(data = df0, aes(x=year, y=threeD_SA, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df0, aes(x=year, y=threeD_SA, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype0)+
  scale_size_manual(values = linesize0)+
  scale_colour_manual(values = colpal0) +
  scale_fill_manual(values = colpal0)+
  labs(x = "Year")+
  labs(y = "Surface Rugosity")+
  ggtitle("Surface Rugosity")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))
# scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p000 = ggplot()+
  stat_summary(data = df00, aes(x=year, y=fracdim, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df00, aes(x=year, y=fracdim, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype0)+
  scale_size_manual(values = linesize0)+
  scale_colour_manual(values = colpal0) +
  scale_fill_manual(values = colpal0)+
  labs(x = "Year")+
  labs(y = "Fractal Dimension")+
  ggtitle("Fractal Dimension")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))
# scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
# scale_x_continuous(labels = NULL)

# SAVING ----
# Linear Rugosity
ggsave("0_linear.rugosity.png", plot = p0, path = plots,
       width=22, height=15, units="cm", dpi=300)

# Surface Rugosity
ggsave("0_surface.rugosity.png", plot = p00, path = plots,
       width=22, height=15, units="cm", dpi=300)

# Fractal Dimension
ggsave("0_fracdim.png", plot = p000, path = plots,
       width=22, height=15, units="cm", dpi=300)

################################################################################################################
### 1. DIVERSITY 
# LINEAR RUGOSITY ----
p1 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=linear_rugosity, colour=scenarios , group=scenarios,
                              linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df1, aes(x=year, y=linear_rugosity, fill=scenarios , group=scenarios,
                              linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype1)+
  scale_size_manual(values = linesize1)+
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Year")+
  labs(y = "Linear Rugosity")+
  ggtitle("Linear Rugosity")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  coord_cartesian(ylim=c(1,6))
  # scale_y_continuous(breaks = seq(1,6, by = 1))+
  # scale_x_continuous(labels = NULL)

# SURFACE RUGOSITY ----
p11 = ggplot()+
  stat_summary(data = df1, aes(x=year, y=threeD_SA, colour=scenarios , group=scenarios,
                              linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df1, aes(x=year, y=threeD_SA, fill=scenarios , group=scenarios,
                              linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1)+
  scale_size_manual(values = linesize1)+
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Year")+
  labs(y = "Surface Rugosity")+
  ggtitle("Surface Rugosity")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))
  # scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p111 = ggplot()+
  stat_summary(data = df11, aes(x=year, y=fracdim, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df11, aes(x=year, y=fracdim, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype1)+
  scale_size_manual(values = linesize1)+
  scale_colour_manual(values = colpal1) +
  scale_fill_manual(values = colpal1)+
  labs(x = "Year")+
  labs(y = "Fractal Dimension")+
  ggtitle("Fractal Dimension")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))
  # scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
  # scale_x_continuous(labels = NULL)

# SAVING ----
# Linear Rugosity
ggsave("1_linear.rugosity.v2.png", plot = p1, path = sc,
       width=22, height=15, units="cm", dpi=300)

# Surface Rugosity
ggsave("1_surface.rugosity.v2.png", plot = p11, path = sc,
       width=22, height=15, units="cm", dpi=300)

# Fractal Dimension
ggsave("1_fracdim.v2.png", plot = p111, path = sc,
       width=22, height=15, units="cm", dpi=300)
###############################################################################################
### 2. 1FTS ONLY 
# LINEAR RUGOSITY ----
p2 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=linear_rugosity, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df2, aes(x=year, y=linear_rugosity, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2)+
  scale_size_manual(values = linesize2)+
  scale_colour_manual(values = colpal2) +
  scale_fill_manual(values = colpal2)+
  labs(x = "Year")+
  labs(y = "Linear Rugosity")+
  ggtitle("Linear Rugosity")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))
  # coord_cartesian(ylim=c(1,6)) +
  # scale_y_continuous(breaks = seq(1,6, by = 1))+
  # scale_x_continuous(labels = NULL)


# SURFACE RUGOSITY ----
p22 = ggplot()+
  stat_summary(data = df2, aes(x=year, y=threeD_SA, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "line", show.legend = T) +
  stat_summary(data = df2, aes(x=year, y=threeD_SA, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2)+
  scale_size_manual(values = linesize2)+
  scale_colour_manual(values = colpal2) +
  scale_fill_manual(values = colpal2)+
  labs(x = "Year")+
  labs(y = "Surface Rugosity")+
  ggtitle("Surface Rugosity")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.margin = margin(0,0,0,0))+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))
  # scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p222 = ggplot()+
  stat_summary(data = df22, aes(x=year, y=fracdim, colour=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun = mean, geom = "line") +
  stat_summary(data = df22, aes(x=year, y=fracdim, fill=scenarios , group=scenarios,
                                linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA) +
  scale_linetype_manual(values=linetype2)+
  scale_size_manual(values = linesize2)+
  scale_colour_manual(values = colpal2) +
  scale_fill_manual(values = colpal2)+
  labs(x = "Year")+
  labs(y = "Fractal Dimension")+
  ggtitle("Fractal Dimension")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size=14),
      axis.title = element_text(size=14),
      legend.margin = margin(0,0,0,0))
# scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
# scale_x_continuous(labels = NULL)

# SAVING ----
# Linear Rugosity
ggsave("1_linear.rugosity.v2.png", plot = p2, path = sc,
       width=22, height=15, units="cm", dpi=300)

# Surface Rugosity
ggsave("1_surface.rugosity.v2.png", plot = p22, path = sc,
       width=22, height=15, units="cm", dpi=300)

# Fractal Dimension
ggsave("1_fracdim.v2.png", plot = p222, path = sc,
       width=22, height=15, units="cm", dpi=300)
