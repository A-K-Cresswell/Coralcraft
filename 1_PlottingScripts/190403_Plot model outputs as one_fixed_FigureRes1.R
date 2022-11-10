## Functional-structural coral model (FSCM) ---- NextGen
version = paste(Sys.Date())

# working directorys ----
work.dir = ("C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael")
#work.dir=".." # Base directory For Michael
model = paste(work.dir,"1_Model Scripts", sep="/")
plots = paste(work.dir,"1_Plots", sep="/")
output_fixed = paste(work.dir, "1_outputs/FIXED", sep="/")
output_random = paste(work.dir, "1_outputs/RANDOM", sep="/")
figscripts = paste(work.dir, "1_Figure Scripts", sep="/")

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



# Plot all output fixed and random 
colpal = c("tomato", "gold", "white", "white", "white", "white" , "springgreen4", "white", "white", "dodgerblue", "white", "white", "white", "white", "blueviolet", "grey")

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

runs = 50      
# NL-NH ----
setwd(paste(output_fixed, "NL-NH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_NLNH = df
df_fixed_NLNH$Scenario = "NL-NH"

# IL-NH ----
setwd(paste(output_fixed, "IL-NH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_ILNH = df
df_fixed_ILNH$Scenario = "IL-NH"


# FL-NH ----
setwd(paste(output_fixed, "FL-NH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_FLNH = df
df_fixed_FLNH$Scenario = "FL-NH"




# ROW 2
# NL-IH ----
setwd(paste(output_fixed, "NL-IH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_NLIH = df
df_fixed_NLIH$Scenario = "NL-IH"


# IL-IH ----
setwd(paste(output_fixed, "IL-IH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_ILIH = df
df_fixed_ILIH$Scenario = "IL-IH"
df$c.1.nfts...dead.. = as.factor(df$c.1.nfts...dead..) #change factors to factors
df$run = as.factor(df$run) #change factors to factors

# FL-IH ----
setwd(paste(output_fixed, "FL-IH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_FLIH = df
df_fixed_FLIH$Scenario = "FL-IH"
df$c.1.nfts...dead.. = as.factor(df$c.1.nfts...dead..) #change factors to factors
df$run = as.factor(df$run) #change factors to factors



# ROW 3
# NL-FH ----
setwd(paste(output_fixed, "NL-FH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_NLFH = df
df_fixed_NLFH$Scenario = "NL-FH"
df$c.1.nfts...dead.. = as.factor(df$c.1.nfts...dead..) #change factors to factors
df$run = as.factor(df$run) #change factors to factors

# IL-FH ----
setwd(paste(output_fixed, "IL-FH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_ILFH = df
df_fixed_ILFH$Scenario = "IL-FH"
df$c.1.nfts...dead.. = as.factor(df$c.1.nc.1.nfts...dead..s...dead..) #change factors to factors
df$run = as.factor(df$run) #change factors to factors

# FL-FH ----
setwd(paste(output_fixed, "FL-FH", sep ="/"))
# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) { 
  filename <- paste("run", run, ".csv", sep = "") 
  thisdf=read.csv(filename) 
  df=rbind(df,thisdf) 
}
df_fixed_FLFH = df
df_fixed_FLFH$Scenario = "FL-FH"



df_fixed = rbind(df_fixed_NLNH, df_fixed_ILNH, df_fixed_FLNH, df_fixed_NLIH, df_fixed_ILIH, df_fixed_FLIH, df_fixed_NLFH, df_fixed_ILFH, df_fixed_FLFH)

df_fixed$c.1.nfts...dead.. = as.factor(df$c.1.nfts...dead..) #change factors to factors
df_fixed$run = as.factor(df$run) #change factors to factors
df_fixed$Scenario = as.factor(df_fixed$Scenario) #change factors to factors
df_fixed = subset(df_fixed, c.1.nfts...dead.. %in% c("1", "2","3", "4", "5", "dead"))
df_fixed$c.1.nfts...dead..  = droplevels(df_fixed$c.1.nfts...dead..)


str(df_fixed)
summary(df_fixed)

scenarios = unique(df_fixed$Scenario)

fig_scens = c("NL-NH", "IL-NH", "NL-IH", "IL-IH", "FL-NH", "NL-FH")
#labels = c("NL-NH, No low intensity", "IL-NH", "NL-IH", "IL-IH", "FL-NH", "NL-FH")
# Make 6 x 3 panel figure


setwd(figscripts)
disttimes = read.csv("disturbance timing indicators.csv")
disttimes$symbol = factor(disttimes$symbol, levels = c("L", "H"))
disttimes$x = disttimes$x/52

dist1 = NULL
dist
scen = scenarios[4]

head(df_fixed)
df_fixed$year = df_fixed$timestep/52

for (scen in fig_scens) {
  #dev.off()
  print(scen)
  number = which(scenarios== scen)
  dat = subset(df_fixed, Scenario == scen)
  disttimes2 = subset(disttimes, Scenario == scen)
  dat$c.1.nfts...dead.. <- factor(dat$c.1.nfts...dead.., levels = c("1", "2","3", "4","5", "dead"))
  
  # Number of colonies ----
  p1= ggplot()+
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=no.colonies, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=no.colonies,  group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
    scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
    scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
    geom_point(data = disttimes2, aes(x=x, y=7, shape = symbol), size =2) +
    scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
    #scale_fill_manual(labels = c("Low intensity", "High intensity"), values = c("black", "gray35"), name = "Disturbance", guide = FALSE) +
    labs(x = "")+
    ggtitle(paste(scen)) +
    ylab(expression(paste("Colony density ± CI ( ", m^-2, ")", sep = "")))+
    theme_bw()+
    theme(legend.text=element_text(size=16))+
    theme(legend.title=element_text(size=16))+
    #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
    theme(axis.text  = element_text(size=16),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 18, face = "bold"),
          axis.text.x = element_blank(),
          legend.direction = "horizontal",
          legend.title = element_text(size = 16, face = "bold")) +
    theme(axis.title=element_text(size=16),
          plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
  #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  #ylim(0,7)
    scale_y_continuous(limits = c(0,7), breaks = seq(0,7, by = 1)) +
    xlim(0,25)
  



  # Percent cover ----
  p2= ggplot()+
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cover/100, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cover/100, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", alpha=0.3, colour = NA) +
    scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
    scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
    geom_point(data = disttimes2, aes(x=x, y=80, shape = symbol), size =2) +
    scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
    labs(x = "")+
    ggtitle("") +
    labs(y = "Percentage cover (%) ± CI")+
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
  p3= ggplot()+
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cells, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
    stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cells, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
    scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
    scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
    geom_point(data = disttimes2, aes(x=x, y=25000, shape = symbol), size =2) +
    scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
    labs(x = "") +
    ylab(expression(paste("Volume ± CI ( " , m^-3, ")", sep = "")))+
    theme_bw()+
    ggtitle("") +
    theme(legend.text=element_text(size=16))+
    theme(legend.title=element_text(size=16))+
    #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
    theme(axis.text  = element_text(size=16),
          plot.title = element_text(size = 18, face = "bold"),
          axis.ticks.x = element_blank(), # if want to remove x axis units
          axis.text.x = element_blank()) +
    theme(axis.title=element_text(size=16),
          plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +

  #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  scale_y_continuous(limits = c(0,25000), breaks = seq(0,25000, by = 5000))  +
    xlim(0,25)

  setwd(plots)
  # name = paste("p", scen)
  # print(name)
  p = plot_grid(p1, p2, p3, align = "h", ncol = 3, rel_widths = c(2/5, 2/5, 2.3/5))
  #label = ggdraw() + draw_label("Timestep (weeks)", fontface='bold')
  #plot_grid(p, label, ncol=1, rel_heights=c(1, 0.1)) # rel_heights values control title margins
  p
  ggsave(paste(scen, "190403_Fixed.png"), width = 30, height = 12, units = "cm")
  print(number)
  if (number == 1) plot1 = p
  if (number == 2) plot2 = p
  if (number == 3) plot3 = p
  if (number == 4) plot4 = p
  if (number == 5) plot5 = p
  if (number == 6) plot6 = p
  if (number == 7) plot7 = p
  if (number == 8) plot8 = p
  if (number == 9) plot9 = p

}




# # --------------------------------------------------------------------
p1= ggplot()+
  stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=no.colonies, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=no.colonies,  group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
  scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
  scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
  geom_point(data = disttimes2, aes(x=x, y=7, shape = symbol), size =2) +
  scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
  labs(x = "")+
  ggtitle(paste(scen)) +
  ylab(expression(paste("Colony density ± CI ( ", m^-2, ")", sep = "")))+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16),
        plot.title = element_text(size = 18, face = "bold"))+
  #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
  #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  #ylim(0,7)
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7, by = 1))  +
  xlim(0,25)



# Percent cover ----
p2= ggplot()+
  stat_summary(data = dat, aes(x=year, y=cover/100, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = dat, aes(x=year, y=cover/100, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", alpha=0.3, colour = NA) +
  scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose','Dead'), values = colpal[c(1,2,7,10,15,16)], guide = FALSE) +
  scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose', 'Dead'), values = colpal[c(1,2,7,10,15,16)], guide = FALSE) +
  geom_point(data = disttimes2, aes(x=x, y=80, shape = symbol), size =2) +
  scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
  labs(x = "")+
  ggtitle("") +
  labs(y = "Percentage cover (%) ± CI")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
  #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,80, by = 10)) +
  xlim(0,25)




# Plot number of cells including total ----
p3= ggplot()+
  stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cells, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=year, y=cells, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
  scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
  scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)], guide = FALSE) +
  geom_point(data = disttimes2, aes(x=x, y=25000, shape = symbol), size =2) +
  scale_shape_manual(labels = c("Low intensity", "High intensity"), values = c(1,16), name = "Disturbance", guide = FALSE) +
  labs(x = "") +
  ylab(expression(paste("Volume ± CI ( " , m^-3, ")", sep = "")))+
  theme_bw()+
  ggtitle("") +
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.margin = margin(0, 1, -0.5, 0.2, "cm")) +
  #scale_x_continuous(breaks = seq(0,ts,by = 100)) +
  scale_y_continuous(limits = c(0,25000), breaks = seq(0,25000, by = 5000))  +
  xlim(0,25)

p = plot_grid(p1, p2, p3, align = "h", ncol = 3, rel_widths = c(2/5, 2/5, 2.3/5))
#label = ggdraw() + draw_label("Timestep (weeks)", fontface='bold')
#plot_grid(p, label, ncol=1, rel_heights=c(1, 0.1)) # rel_heights values control title margins
p
# #---------------------------------------------------------------------------------------------
# 
# 
# 
# 
# library(png)
# library(jpeg)
# library(raster)
# library(grid)
# library(ggplot2)
# 
# encrust <- readPNG("encrust.png")%>%
#   rasterGrob(interpolate=TRUE)
# hemi <- readPNG("hemi.png")%>%
#   rasterGrob(interpolate=TRUE)
# tabular <- readPNG("tabular.png")%>%
#   rasterGrob(interpolate=TRUE)
# branch<- readPNG("branch.png")%>%
#   rasterGrob(interpolate=TRUE)
# cory<- readPNG("cory.png")%>%
#   rasterGrob(interpolate=TRUE)
# 
# legend= ggplot()+
#   stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=timestep, y=cells, colour=c.1.nfts...dead.., group=c.1.nfts...dead..), fun.y = mean, geom = "line", size=1.2) +
#   stat_summary(data = subset(dat, c.1.nfts...dead.. != "dead"), aes(x=timestep, y=cells, group=c.1.nfts...dead.., fill = c.1.nfts...dead..), fun.ymin = ci.min, fun.ymax = ci.max, geom = "ribbon", colour=NA,alpha=0.3) +
#   scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)]) +
#   scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal[c(1,2,7,10,15)]) +
#   annotation_custom(cory, ymin = 0.6, ymax = 1.25, xmin = 0, xmax = 2) +
#   annotation_custom(branch, ymin = 1.35, ymax =2.2, xmin = 0, xmax = 2) +
#   annotation_custom(tabular, ymin = 2.1, ymax = 3, xmin = 0, xmax = 2) +
#   annotation_custom(hemi, ymin = 2.6, ymax = 3.3, xmin = 0, xmax = 2) +
#   annotation_custom(encrust, ymin = 3.2, ymax = 4, xmin = 0, xmax = 2) +
#   labs(x = "") +
#   labs(y = "")+
#   theme_bw()+
#   theme(legend.text=element_text(size=16))+
#   theme(legend.title=element_text(size=16, face ="bold"))+
#   theme(axis.line.x = element_line(color = "white"),
#         axis.line.y = element_blank(),
#         axis.text.x = element_text(colour = "white"),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_line(colour = "white"),
#         axis.ticks.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "left",
#         legend.direction = "horizontal") +
#   scale_y_continuous(limits = c(0,5), breaks = seq(0,5, by = 1)) +
#   xlim(0,3) +
#   #theme(legend.key.size = unit(2,"point")) +
#   theme(legend.key.size = unit(1.8,"line"))
# legend
# ggsave("legend.png", width = 30, height = 20, units = "cm")
# plot0 = plot_grid(legend, NULL, NULL, rel_widths = c(0.3,0.3,0.3), ncol = 3)

plot_grid(plot1,plot2,plot4,plot5,plot3,p, ncol = 1)
ggsave("190525_Figure Res1.png", width = 30, height = 55, units = "cm")


