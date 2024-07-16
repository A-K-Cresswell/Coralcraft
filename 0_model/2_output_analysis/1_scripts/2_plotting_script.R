# Code for plotting

# this script sources read_df.R for data manipulation needed for plots 

#################################
timesteps = 52*5
runs = 100

## set work directory
work.dir = setwd("./Coralcraft/0_model") #set work directory to location where 0_model is 
sim.wd = paste(work.dir, "1_simulation_output", sep="/") #for model simulation output
out.wd = paste(work.dir,"2_output_analysis", sep="/") #for output analysis
script = paste(out.wd,"1_scripts", sep="/") 
output = paste(out.wd, "2_output", sep="/") 
setwd(work.dir)

## load csv files & plotting parameters
source(paste0(script, "/plot_parameters.R"))
source(paste0(script, "/read_df.R"))
################################################################################################################
###### STRUCTURAL COMPLEXITY 
### 1. DIVERSITY COMMUNITY TYPES
# LINEAR RUGOSITY ----
p1 = ggplot()+
  stat_summary(data = df1, aes(x=coral.cover, y=linear_rugosity, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", show.legend = F, alpha = 0.5) +
  stat_summary(data = df1, aes(x=coral.cover, y=linear_rugosity, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon",
               alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Linear rugosity")+
  ggtitle("Linear Rugosity")+
  coord_cartesian(ylim=c(1,8.5))+
  theme1 +
  # theme(axis.title.x = element_blank())+
  geom_line(data=df11, aes(x=coral.cover, y=lin_rug/100, colour=scenarios, group = scenarios), size=size_pt, show.legend = F)
# scale_y_continuous(breaks = seq(1,6, by = 1))+
# scale_x_continuous(labels = NULL)

# SURFACE RUGOSITY ----
p11 = ggplot()+
  stat_summary(data = df1, aes(x=coral.cover, y=surface_rugosity, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha =0.5, show.legend = F) +
  stat_summary(data = df1, aes(x=coral.cover, y=surface_rugosity, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Surface rugosity")+
  ggtitle("Surface Rugosity")+
  theme1 +
  # theme(axis.title.x = element_blank())+
  # coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
  geom_line(data=df11, aes(x=coral.cover, y=sur_rug/10000, colour=scenarios, group = scenarios), size=size_pt, show.legend = F)
# scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p111 = ggplot()+
  stat_summary(data = df1, aes(x=coral.cover, y=fracdim, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha =0.5, show.legend = F) +
  stat_summary(data = df1, aes(x=coral.cover, y=fracdim, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Fractal dimension")+
  coord_cartesian(ylim = c(1,3))+
  ggtitle("Fractal Dimension")+
  # scale_y_continuous(limits = c(1,3))+
  # scale_x_continuous(labels = NULL)
  theme1+
  geom_line(data=df11, aes(x=coral.cover, y=fracdim, colour=scenarios, group = scenarios), size=size_pt, show.legend = F)


### 2. MONOSPECIFIC COMMUNITY TYPES----
# LINEAR RUGOSITY ----
p2 = ggplot()+
  stat_summary(data = df2, aes(x=coral.cover, y=linear_rugosity, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", show.legend = F, alpha = 0.5) +
  stat_summary(data = df2, aes(x=coral.cover, y=linear_rugosity, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", 
               alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype2, name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2, name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2, name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2, name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = "Linear rugosity")+
  ggtitle("-")+
  coord_cartesian(ylim=c(1,8.5))+
  theme2 +
  geom_line(data=df22, aes(x=coral.cover, y=lin_rug/100, colour=scenarios, group = scenarios), size=size_pt, show.legend = F)
# scale_y_continuous(breaks = seq(1,6, by = 1))+
# scale_x_continuous(labels = NULL)

# SURFACE RUGOSITY ----
p22 = ggplot()+
  stat_summary(data = df2, aes(x=coral.cover, y=surface_rugosity, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha = 0.5, show.legend = F) +
  stat_summary(data = df2, aes(x=coral.cover, y=surface_rugosity, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype2, name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2, name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2, name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2, name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = "Surface rugosity")+
  ggtitle("-")+
  theme2 +
  # theme(axis.title.x = element_blank())+
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
  geom_line(data=df22, aes(x=coral.cover, y=sur_rug/10000, colour=scenarios, group = scenarios), size=size_pt, , show.legend = F)
# scale_x_continuous(labels = NULL)

# FRACTAL DIMENSION ----
p222 = ggplot()+
  stat_summary(data = df2, aes(x=coral.cover, y=fracdim, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha = 0.5, show.legend = F) +
  stat_summary(data = df2, aes(x=coral.cover, y=fracdim, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype2[2:11], name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2[2:11], name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2[2:11], name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2[2:11], name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = "Fractal dimension")+
  coord_cartesian(ylim = c(1,3))+
  ggtitle("-")+
  theme2+
  geom_line(data=df22, aes(x=coral.cover, y=fracdim, colour=scenarios, group = scenarios), size=size_pt, , show.legend = F)
# scale_y_continuous(limits = c(1,7), breaks = seq(0,7, by = 1))+
# scale_x_continuous(labels = NULL)

################################################################################################################
###### SHELTER METRICS
### 1. DIVERSITY COMMUNITY TYPES 
# SHELTER VOLUME ----
s1 = ggplot()+
  stat_summary(data = df1, aes(x=coral.cover, y=shelter_volume, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha=0.5, show.legend = T) +
  stat_summary(data = df1, aes(x=coral.cover, y=shelter_volume, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+ 
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+ 
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = expression(paste("Total volume ( ",dm^3,")", sep = "")))+
  ggtitle("Shelter Volume")+
  scale_y_continuous(limits = c(0,140), breaks = seq(0,140, by = 20))+
  theme1 +
  theme(axis.title.x=element_blank()) +
  geom_line(data=df11, aes(x=coral.cover, y=sheltervol, colour=scenarios , group=scenarios), size=size_pt)
# scale_x_continuous(labels = NULL)

# DEMERSAL SHELTER ----
s11 = ggplot()+
  stat_summary(data = df1, aes(x=coral.cover, y=demersal_shelter, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha = 0.5, show.legend = T) +
  stat_summary(data = df1, aes(x=coral.cover, y=demersal_shelter, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+ #only for 1fts
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+ #only for 1fts
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL)+
  labs(y = "Mean percent shelter")+
  ggtitle("Demersal Shelter")+
  coord_cartesian(y=c(0,22))+
  theme1 +
  theme(axis.title.x=element_blank())+
  geom_line(data=df11, aes(x=coral.cover, y=meansides, colour=scenarios , group=scenarios), size=size_pt)
# scale_y_continuous(limits = c(0,22), breaks = seq(0,20, by = 5))+
# scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# PELAGIC SHELTER ----
s111 = ggplot()+
  stat_summary(data = df1, aes(x=coral.cover, y=pelagic_shelter_1_15, colour=scenarios , group=scenarios, 
                                                                 linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha = 0.5, show.legend=F) +
  stat_summary(data = df1, aes(x=coral.cover, y=pelagic_shelter_1_15, fill=scenarios , group=scenarios,
                                                                 linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA, show.legend = F) +
  scale_linetype_manual(values=linetype1, name = "Diversity scenarios")+
  scale_size_manual(values = linesize1, name = "Diversity scenarios")+ 
  scale_colour_manual(values = colpal1, name = "Diversity scenarios") +
  scale_fill_manual(values = colpal1, name = "Diversity scenarios")+
  labs(x = NULL,
       y = "Mean percent shelter")+
  coord_cartesian(ylim = c(0,75))+
  ggtitle("Pelagic Shelter")+
  theme1+
  geom_line(data=subset(df.long1, height %in% "1-15cm"), aes(x=coral.cover, y=meanshelter, colour=scenarios , group=scenarios), size=size_pt) 


# scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))+
# scale_x_continuous(labels = NULL)

###########################
### 2. MONOSPECIFIC COMMUNITY TYPE 
# SHELTER VOLUME ----
s2 = ggplot()+
  stat_summary(data = df2, aes(x=coral.cover, y=shelter_volume, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha=0.5) +
  stat_summary(data = df2, aes(x=coral.cover, y=shelter_volume, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2, name = "Monospecific scenarios")+ 
  scale_size_manual(values = linesize2, name = "Monospecific scenarios")+ 
  scale_colour_manual(values = colpal2, name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2, name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  scale_y_continuous(breaks = seq(0,140, by = 20))+
  ggtitle("-")+
  theme2 +
  theme(axis.title.x=element_blank())+
  geom_line(data = df22, aes(x=coral.cover, y=sheltervol, colour=scenarios , group=scenarios), size = size_pt)

# scale_x_continuous(labels = NULL)

# DEMERSAL SHELTER ----
s22 = ggplot()+
  stat_summary(data = df2, aes(x=coral.cover, y=demersal_shelter, colour=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun = mean, geom = "point", show.legend = T) +
  stat_summary(data = df2, aes(x=coral.cover, y=demersal_shelter, fill=scenarios , group=scenarios,
                               linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, 
               geom = "ribbon", alpha=0.3, colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2, name = "Monospecific scenarios")+ #only for 1fts
  scale_size_manual(values = linesize2, name = "Monospecific scenarios")+ #only for 1fts
  scale_colour_manual(values = colpal2, name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2, name = "Monospecific scenarios")+
  labs(x = NULL)+
  labs(y = NULL)+
  ggtitle("-")+
  theme2 +
  theme(axis.title.x=element_blank())+
  geom_line(data=df22, aes(x=coral.cover, y=meansides, colour=scenarios , group=scenarios), size=size_pt)
# coord_cartesian(ylim = c(0,22))+
# scale_y_continuous(breaks = seq(0,20, by = 5))+
# scale_x_continuous(limits = c(0,5), breaks = seq(0,5, by = 1))

# PELAGIC SHELTER ----
s222 = ggplot()+
  stat_summary(data = df2, aes(x=coral.cover, y=pelagic_shelter_1_15, colour=scenarios , group=scenarios, 
                                                                  linetype=scenarios, size=scenarios), fun = mean, geom = "point", alpha = 0.3, show.legend=T) +
  stat_summary(data = df2, aes(x=coral.cover, y=pelagic_shelter_1_15, fill=scenarios , group=scenarios,
                                                                  linetype=scenarios, size=scenarios), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, 
               colour = NA, show.legend = T) +
  scale_linetype_manual(values=linetype2, name = "Monospecific scenarios")+
  scale_size_manual(values = linesize2, name = "Monospecific scenarios")+
  scale_colour_manual(values = colpal2, name = "Monospecific scenarios") +
  scale_fill_manual(values = colpal2, name = "Monospecific scenarios")+
  labs(x = NULL,
       y = NULL)+
  ggtitle("-")+
  coord_cartesian(ylim = c(0,75))+
  theme2+
  geom_line(data=subset(df.long22, height %in% "1-15cm"), aes(x=coral.cover, y=meanshelter, colour=scenarios , group=scenarios), size=size_pt) 
#################################################################################################