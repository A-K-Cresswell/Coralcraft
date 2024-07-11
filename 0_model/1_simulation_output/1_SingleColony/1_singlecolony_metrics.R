## Functional-structural coral model (FSCM) ---- temporal dynamics: metrics over time
rm(list=ls())
# COMPARING SCENARIOS

# METRICS BELOW: 
# total cover
# Linear rugosity
# 3D rugosity
# no. of colonies

#######################################################################################
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
#library("viridis")

version = paste(Sys.Date(), format ="%Y%m%d")
colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "skyblue3", "orchid1", "orange")


# working directorys ----
work.dir = setwd("~/GitHub/Coralcraft/2_SimulationOutputs/1_Outputs/1_SingleColony")
# model = paste(work.dir,"1_Model Scripts", sep="/") #not sure why this is needed yet
plots = setwd("~/GitHub/Coralcraft/2_SimulationOutputs/1_Plots/1_SingleColony")
# output = paste(work.dir, "1_Outputs", sep="/")
# figscripts = paste(work.dir, "1_Figure Scripts", sep="/")

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
roundUp10<- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# Confidence intervals ----
ci <- function(x) 2*sd(x) / sqrt(length(x))
ci.min <- function(x) (mean(x)) - ci(x)
ci.max <- function(x) (mean(x)) + ci(x)

#simpsons ----
simp = function(x) 1- (x(x-1) / sum(x)(sum(x)-1))
simp = function(x) {
  N=sum(x)
  1- sum (   x*(x-1) / (N*(N-1)))
}
simp2 = function(x) {
  N=sum(x)
  1- sum (   (x / N)^2)
}
# use simp2 

runs = 1   

# 1_encrusting  ----
setwd(paste(work.dir, "20221122_1_encrusting_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_1_encrusting = df
df_1_encrusting$Scenario = "001_encrusting"


# 2_flexi-hem  ----
setwd(paste(work.dir, "20221122_2_flexihem_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_2_flexihem = df
df_2_flexihem$Scenario = "002_flexihem"


# 3_digitate  ----
setwd(paste(work.dir, "20221122_3_digitate_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_3_digitate = df
df_3_digitate$Scenario = "003_digitate"


# 4_corymbose  ----
setwd(paste(work.dir, "20221122_4_corymbose_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_4_corymbose = df
df_4_corymbose$Scenario = "004_corymbose"


# 5_tabular  ----
setwd(paste(work.dir, "20221122_5_tabular_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_5_tabular = df
df_5_tabular$Scenario = "005_tabular"


# 6_mushroom  ----
setwd(paste(work.dir, "20221122_6_mushroom_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_6_mushroom = df
df_6_mushroom$Scenario = "006_mushroom"


# 7_fingers  ----
setwd(paste(work.dir, "20221122_7_fingers_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_7_fingers = df
df_7_fingers$Scenario = "007_fingers"


# 8_cone  ----
setwd(paste(work.dir, "20221122_8_cone_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_8_cone = df
df_8_cone$Scenario = "008_cone"


# 9_hedgehog  ----
setwd(paste(work.dir, "20221122_9_hedgehog_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_9_hedgehog = df
df_9_hedgehog$Scenario = "009_hedgehog"

# 10_staghorn  ----
setwd(paste(work.dir, "20221122_10_staghorn_520_tss", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename) #read file
  df=rbind(df,thisdf)
}
df_10_staghorn = df
df_10_staghorn$Scenario = "010_staghorn"

###################################################################

df_metrics = rbind(df_1_encrusting, df_2_flexihem, df_3_digitate, df_4_corymbose, df_5_tabular,
           df_6_mushroom,df_7_fingers, df_8_cone, df_9_hedgehog, df_10_staghorn)

df_metrics$c.1.nfts...dead.. = as.factor(df_metrics$c.1.nfts...dead..) #change factors to factors
df_metrics$run = as.factor(df_metrics$run) #change factors to factors
df_metrics$Scenario = as.factor(df_metrics$Scenario)
df_metrics = subset(df_metrics, !(c.1.nfts...dead.. == "dead")) #removes dead colonies
df_metrics$c.1.nfts...dead..  = droplevels(df_metrics$c.1.nfts...dead..)


str(df_metrics)
summary(df_metrics)
scenarios = unique(df_metrics$Scenario)
labels = c("encrusting","flexihem", "digitate", "corymbose", "tabular",
           "mushroom", "fingers", "cone", "hedgehog","staghorn")
dat = df_metrics

#look at total cover across the 13 scenarios

# Add totals ----
df2 = dat %>%
  #mutate(SAratio=totalSA/10000) %>%
  group_by(Scenario, timestep) %>%
  summarise(total.cells = sum(cells),
            total.cover = sum(cover)/10000,
            total.colony = sum(no.colonies),
            threeD_SA = mean(totalSA)/10000, #mean because all fts contain the same value in dataframe
            linear_rugosity = mean(rugosity))


#Total percent cover
p1=ggplot()+
  stat_summary(data = df2, aes(x=timestep, y=total.cover, colour=Scenario , group=Scenario ), fun = mean, geom = "line", size=1.2) +
  stat_summary(data = df2, aes(x=timestep, y=total.cover, fill=Scenario , group=Scenario ), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA) +
  scale_colour_manual(values = colpal, label=labels, name = "Growth form") +
  scale_fill_manual(values = colpal, label=labels, name = "Growth form")+
  labs(x = "timesteps")+
  labs(y = "total % cover \u00B1 SE")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) 
#ggsave("total_cover.png", width = 30, height = 15, units = "cm", path=plots)


# Plot number of cells including total ----
p2=ggplot()+
  stat_summary(data = df2, aes(x=timestep, y=total.cells, colour=Scenario , group=Scenario ), fun = mean, geom = "line", size=1.2) +
  stat_summary(data = df2, aes(x=timestep, y=total.cells, fill=Scenario , group=Scenario ), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA) +
  scale_colour_manual(values = colpal, label=labels, name = "Growth form") +
  scale_fill_manual(values = colpal, label=labels, name = "Growth form")+
  labs(x = "timesteps")+
  labs(y = "total cells \u00B1 SE")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm")) 

p = plot_grid(p1, p2, align = "h", ncol = 2, nrow=1)
ggsave("Single Colonies Only.png", width = 40, height = 20, units = "cm", path=plots)

###########  STOP HERE  ##############################

fig_scens = c("1_ALL10", "2_bottom_5", "3_top_5",
               "4_encrusting","5_flexihem", "6_digitate", "7_corymbose", "8_tabular",
               "9_mushroom", "10_fingers", "11_sheet", "12_hedgehog","13_staghorn")

setwd(figscripts)

dist1 = NULL
#dist
scen = scenarios[]
df_metrics$year = df_metrics$timestep/52


for (scen in fig_scens) {
  #dev.off()
  print(scen)
  number = which(scenarios== scen)
  dat = subset(df_metrics, scenarios == scen)
  #disttimes2 = subset(disttimes, Scenario == scen)
  dat$c.1.nfts...dead.. <- factor(dat$c.1.nfts...dead.., levels = c("1", "2","3", 
                                                                    # "4","5", "6", "7","8",
                                                                    # "9","10","11", "12", "13",
                                                                    "dead"))
  
  # Number of colonies ----
  # p1= ggplot()+
  #   stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=no.colonies, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
  #   stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=no.colonies,  group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
  #   scale_colour_manual("Growth form", values = colpal) +
  #   scale_fill_manual("Growth form", values = colpal) +
  #   #geom_point(data = disttimes2, aes(x=x, y=7, shape = symbol), size =2) +
  #   #scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
  #   #scale_fill_manual(labels = c("Low intensity", "High intensity"), values = c("black", "gray35"), name = "Disturbance", guide = FALSE) +
  #   labs(x = "")+
  #   ggtitle(paste(scen)) +
  #   ylab(expression(paste("Colony density +/- CI ( ", m^-2, ")", sep = "")))+
  #   theme_bw()+
  #   theme(legend.text=element_text(size=16))+
  #   theme(legend.title=element_text(size=16))+
  #   #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  #   theme(axis.text  = element_text(size=16),
  #         axis.ticks.x = element_blank(),
  #         plot.title = element_text(size = 18, face = "bold"),
  #         axis.text.x = element_blank(),
  #         legend.direction = "horizontal",
  #         legend.title = element_text(size = 16, face = "bold")) +
  #   theme(axis.title=element_text(size=16),
  #         plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
  #   #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  #   #ylim(0,7)
  #   scale_y_continuous(limits = c(0,7), breaks = seq(0,7, by = 1)) 
  #   #xlim(0,25)

  # Percent cover ----
  p2= ggplot()+
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cover/100, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cover/100, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", alpha=0.3, colour = NA) +
    scale_colour_manual("Growth form", values = colpal) +
    scale_fill_manual("Growth form", values = colpal) +
    #geom_point(data = disttimes2, aes(x=x, y=80, shape = symbol), size =2) +
    #scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
    labs(x = "")+
    ggtitle("") +
    labs(y = "Percentage cover (%) +/- CI")+
    theme_bw()+
    theme(legend.text=element_text(size=16))+
    theme(legend.title=element_text(size=16))+
    #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
    theme(axis.text  = element_text(size=16),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 18, face = "bold"),
          axis.text.x = element_blank()) +
    theme(axis.title=element_text(size=16),
          plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
    #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
    scale_y_continuous(limits = c(0,80), breaks = seq(0,80, by = 10)) +
    xlim(0,25)





  # Plot number of cells including total ----
  # p3= ggplot()+
  #   stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cells, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
  #   stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cells, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
  #   scale_colour_manual("Growth form", values = colpal, guide="none") +
  #   scale_fill_manual("Growth form", values = colpal, guide="none") +
  #   # geom_point(data = disttimes2, aes(x=x, y=25000, shape = symbol), size =2) +
  #   # scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
  #   labs(x = "") +
  #   ylab(expression(paste("Volume +/- CI ( " , m^-3, ")", sep = "")))+
  #   theme_bw()+
  #   ggtitle("") +
  #   theme(legend.text=element_text(size=16))+
  #   theme(legend.title=element_text(size=16))+
  #   #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  #   theme(axis.text  = element_text(size=16),
  #         plot.title = element_text(size = 18, face = "bold"),
  #         axis.ticks.x = element_blank(), # if want to remove x axis units
  #         axis.text.x = element_blank()) +
  #   theme(axis.title=element_text(size=16),
  #         plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
  # 
  #   #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  #   scale_y_continuous(limits = c(0,25000), breaks = seq(0,25000, by = 5000)) 
  #   #xlim(0,25)
  
  #setwd(plots)
  # name = paste("p", scen)
  # print(name)
  # p = plot_grid(p1, p2, p3, align = "h", ncol = 3, rel_widths = c(2/5, 2/5, 2.3/5))
  # ##label = ggdraw() + draw_label("Timestep (weeks)", fontface='bold')
  # #plot_grid(p, label, ncol=1, rel_heights=c(1, 0.1)) # rel_heights values control title margins
  # p
  # #ggsave(paste(scen, "190403_Fixed.png"), width = 30, height = 12, units = "cm")
  # print(number)
  # if (number == 1) plot1 = p
  # if (number == 2) plot2 = p
  # if (number == 3) plot3 = p
  # if (number == 4) plot4 = p
  # if (number == 5) plot5 = p
  # if (number == 6) plot6 = p
  # if (number == 7) plot7 = p
  # if (number == 8) plot8 = p
  # if (number == 9) plot9 = p
  # if (number == 10) plot10 = p
  # if (number == 11) plot11 = p
  # if (number == 12) plot12 = p
  # if (number == 13) plot13 = p
  
}




