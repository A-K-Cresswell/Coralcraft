# Coralcraft - A functional-structural coral model
# Model code

# Authors
# Michael Renton
# Anna K Cresswell

# For more information contact annacresswell@gmail.com

# Appendix C of Cresswell et al. 2019, "Modelling the effects of hydrodynamic disturbance intensity and frequency on the morphological diversity and structural complexity of coral communities"

# Libraries
# you will need all these libraries installed for the script to run without errors
#install.packages("animation")
#install.packages("scatterplot3d")
#install.packages("webshot")
#install.packages("rgl")
#install.packages("magick")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("png")
#install.packages("raster")

library(animation)
library(scatterplot3d)
library(rgl)
#library(magick)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(png)
#library(raster)
library(webshot)


work.dir = "C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael"
#work.dir = "C:/Users/cre25e/Documents/Coralcraft 100 yr simulations Dec 2019"
#work.dir=".." # Base directory For Michael
model = paste(work.dir,"1_Model Scripts", sep="/")
plots = paste(work.dir,"1_Plots", sep="/")
output = paste(work.dir, "1_outputs", sep="/")

colpal = c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet")
colpal2= c("tomato", "gold", "springgreen4", "dodgerblue", "blueviolet", "grey")
# For export naming ----
version = paste(Sys.Date())

# 1. Set parameters ----
# set simulation parameters ----
runs = 1 # how many times to run the simulation
timesteps = 100*52  # the number of timesteps in each simulation
ws = 100 # world size (cm)
maxdepth = 1 #(m) # this parameter is not used again
mindepth = 0 #(m) # used to calc top of world light level
n.initial.colonies = 10  # how many corals in the beginning - initial size is one block each (i.e. 1cm x 1cm)

# set spawning parameters ----
randomrecruits = 1 # if set to 1 random allocation of growth forms, else allocation a probability of the number of live cells of each colony.
spawn.freq = 52 # how frequently (in timesteps) does spawning occur (set to high number for no spawning)
nnewrecruits= 5 # how many new corals each spawn (could make random)

# set disturbance parameters ----
#random?
randomdist = "fixed" # fixed or random 
# intensity 
if(randomdist == "fixed") disturbance.intensity.low = c(20,20,20) else disturbance.intensity.low = exp(rnorm(1000,log(20),0.5)) # how intense is the disturbance (a smaller number is a bigger disturbance) 
if(randomdist == "fixed") disturbance.intensity.high = c(1.5,1.5,1.5) else disturbance.intensity.high = exp(rnorm(1000,log(1.5),0.5)) # how intense is the disturbance (a smaller number is a bigger disturbance) 
# frequency
frequent = 26
infrequent = 52*5
freq.low = infrequent# 99999 OR frequent OR infrequent
freq.high = infrequent# 99999 OR frequent OR infrequent

# background mortality ----
background.mort = 0.001 #base probability #95% chance of surviving a year
background.mort2 = 0.005 #higher probability for encrusting due to being on the bottom

# light and resources parameters ----
maint = 0.1
surface.light.level = 1
k = log(1)-log(0.72) # k = 0.3285041
light.level = surface.light.level * (exp(1))^(-k*mindepth) # light level at min depth 
light.side = 0.5 # proporation of light travelling sidewards  
light.atten = 0.72^(1/100) # for k =0.64 - 0.9908789 # light.atten = 0.9908789^100 = 0.40000 OR 0.4^(1/100)
res.cap = 6 # resource cap: this means a colony can only store 6 units of energy
start.res = 1 # resources each colony starts with
#0.938005^(1/100)
#0.938005
#Light equation
#Light_depth = Light_surface x e^(-k*depth)


# 2. source growth form information ----
setwd(model)
source('191205_Coralcraft_morphology_design_code.R') ## need to run this once to load the growth forms
load(file="ftcelllist") # load growth forms


# 
# Plot final growth with rgl ----
open3d()
#uM <- load(file = "rgl rotation.rdata")
#par3d(userMatrix = uM)
loc = which(world > 0,arr.ind = TRUE) # put world == ft if just want to plot one type
ids = world[which(world>0)]
ifdead = dead[which(world>0)]
ftss = unlist(sapply(ids , function(thisid) {
  colonymap$ft[colonymap$colonyid == thisid]
}))
spheres3d(loc[,1],loc[,2],loc[,3],color=colpal[ftss])#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
par3d(userMatrix = uM3, zoom=1)
shade3d(qmesh3d(verticesfloor, indices), color='black')
shade3d(qmesh3d(verticesback, indices), color='darkgrey')
shade3d(qmesh3d(verticesside, indices), color='grey')

rgl.snapshot(paste(version,"Finalimagecolouredcolonies.png"))


# spin and create gif of final growth ----
open3d()
loc = which(world > 0,arr.ind = TRUE)
ids = world[which(world>0)]
ifdead = dead[which(world>0)]
ftss = unlist(sapply(ids , function(thisid) {
  colonymap$ft[colonymap$colonyid == thisid]
}))
spheres3d(loc[,1],loc[,2],loc[,3],color=colpal[ftss])
par3d(userMatrix = uM3, zoom=1)
rgl.lines(c(100, 100), c(100, 100), c(-15,35), color = "white")
rgl.lines(c(0, 100), c(0, 0), c(0, 0), color = "white")
rgl.lines(c(0, 0), c(0,100), c(0, 0), color = "white")

movie3d(spin3d(), duration = 8, fps =5, movie = "spinning",dir = thisoutput, type = "gif", convert = NULL)


# plot light absorption with rgl ----

light[,,1:(ws-1)] = light[,,2:ws] * light.atten
lighttoside = light[,,1:(ws-1)] * light.side
lighttoside1 = lighttoside[c(2:ws,1),,1:(ws-1)] * 0.25
lighttoside2 = lighttoside[c(ws,1:(ws-1)),,1:(ws-1)] * 0.25
lighttoside3 = lighttoside[,c(2:ws,1),1:(ws-1)] * 0.25
lighttoside4 = lighttoside[,c(ws,1:(ws-1)),1:(ws-1)] * 0.25
light[,,1:(ws-1)] = light[,,1:(ws-1)] - lighttoside + lighttoside1 + lighttoside2 + lighttoside3 + lighttoside4
light[,,ws] = light.level

### Light uptake ----
lightuptake = array(0,dim=c(ws,ws,ws)) # make light uptake world
lightuptake[world > 0 & !dead] = light[world > 0 & !dead] # Fill with light level only where there are coral cells
#image(world[,30,]>0) #image(light[,30,],col =lcol1)  #image(lightuptake[,30,],col =lcol1)
light[world!=0] = 0 # Set light world to zero where there are coral cells (dead or alive) #AKCnew - is dead world separate now though?


lcol <- colorRampPalette(c('black', 'blue', 'cadetblue1'))
lcol1 <- lcol(100)
png(paste(version,"shading.png"))
image(light[,,1], col =lcol1, xaxt='n', yaxt='n', xlab ="x", ylab = "y") #bottom
dev.off()




lightuptakeperblock  = lightuptake[which(world>0)]
colvals=ceiling(lightuptakeperblock *100*1.8)+1
open3d()
par3d(userMatrix = uM3, zoom=1)
spheres3d(loc[,1],loc[,2],loc[,3],color=lcol1[colvals])#,alpha=0.9,radius=0.9+0.3*runif
shade3d(qmesh3d(verticesfloor, indices), color='black')
shade3d(qmesh3d(verticesback, indices), color='darkgrey')
shade3d(qmesh3d(verticesside, indices), color='grey')
rgl.snapshot(paste(version,"final light absorpotion.png"))


# plot dead and alive with rgl ----
open3d()
spheres3d(loc[,1],loc[,2],loc[,3],color=c("green", "black")[ifdead +1])#,alpha=0.9,coralpolyps$sz*0.7)#+0.3*runif( dim(coralpolyps)[1] )) ## last bit makes circles random sizes
par3d(userMatrix = uM3, zoom=1)
shade3d(qmesh3d(verticesfloor, indices), color='black')
shade3d(qmesh3d(verticesback, indices), color='darkgrey')
shade3d(qmesh3d(verticesside, indices), color='grey')
rgl.snapshot(paste(version,"final dead_alive absorpotion.png"))


# Also plot the temporal output in this script ----

plots = paste(work.dir,"1_Plots", sep="/")

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
roundUp10<- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


# Read in data from each run and join into one dataframe ----
df=NULL # make empty data frame
for (run in 1:runs) {
  filename <- paste("run", run, ".csv", sep = "")
  thisdf=read.csv(filename)
  df=rbind(df,thisdf)
}


str(df)
#change factors to factors
df$c.1.nfts. = as.factor(df$c.1.nfts.)
df$run = as.factor(df$run)

# Add total number of cells ----

# Add totals ----
df2 = df %>%
  group_by(run, timestep) %>%
  summarise(total.cells = sum(cells),
            total.cover = sum(cover),
            total.colony = sum(no.colonies))


df3 = df2 %>%
  group_by(timestep) %>%
  summarise(mean.total.cells = mean(total.cells),
            se.total.cells = se(total.cells),
            mean.total.cover = mean(total.cover),
            se.total.cover = se(total.cover),
            mean.total.colony = mean(total.colony),
            sd.total.colony = se(total.colony)
  )
# Add means of 520 -1040 timesteps.
dfmeans = df %>%
  filter(timestep>520) %>%
  group_by(c.1.nfts...dead..) %>%
  summarise(mean.cover = mean(cover),
            se.cover= se(cover))

#--------------------
str(df3)


# Number of colonies ----
p1= ggplot()+
  stat_summary(data = subset(df, c.1.nfts. != "dead"), aes(x=timestep, y=no.colonies, colour=c.1.nfts., group=c.1.nfts.), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = subset(df, c.1.nfts. != "dead"), aes(x=timestep, y=no.colonies,  group=c.1.nfts., fill = c.1.nfts.), fun.ymin = se.min, fun.ymax = se.max, geom = "ribbon", colour=NA,alpha=0.3) +
  scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal, guide = FALSE) +
  scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal, guide = FALSE) +
  labs(x = "")+
  labs(y = "Number of colonies ± SE")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm"))
#ylim(0,7)
p1
ggsave("Colonies.png", width = 21, height = 21, units = "cm")# dpi = 300))


# Plot number of cells including total ----
p3= ggplot()+
  stat_summary(data = subset(df, c.1.nfts. != "dead"), aes(x=timestep, y=cells, colour=c.1.nfts., group=c.1.nfts.), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = subset(df, c.1.nfts. != "dead"), aes(x=timestep, y=cells, group=c.1.nfts., fill = c.1.nfts.), fun.ymin = se.min, fun.ymax = se.max, geom = "ribbon", colour=NA,alpha=0.3) +
  scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal, guide = FALSE) +
  scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose'), values = colpal, guide = FALSE) +
  labs(x = "") +
  labs(y = "Volume ± SE")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm"))

p3
ggsave("Volume.png", width = 21, height = 21, units = "cm")# dpi = 300))


# Percent cover ----
p2= ggplot()+
  stat_summary(data = df, aes(x=timestep, y=cover, colour=c.1.nfts., group=c.1.nfts.), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = df, aes(x=timestep, y=cover, group=c.1.nfts., fill = c.1.nfts.), fun.ymin = se.min, fun.ymax = se.max, geom = "ribbon", alpha=0.3, colour = NA) +
  scale_colour_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose','Dead'), values = c(colpal, "grey"), guide = FALSE) +
  scale_fill_manual("Growth form", labels = c("Encrusting",'Hemispherical', 'Tabular', 'Branching', 'Corymbose', 'Dead'), values = c(colpal, "grey"), guide = FALSE) +
  labs(x = "")+
  labs(y = "Percent cover (%) ± SE")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm"))


dfmeans = as.data.frame(dfmeans)
p2 + geom_segment(aes(x =521, xend = 1040, y = dfmeans[1,2], yend = dfmeans[1,2]), colour = colpal[1], linetype = 2, size = 2) +
  geom_segment(aes(x =521, xend = 1040, y = dfmeans[8,2], yend = dfmeans[8,2]), colour = colpal[2], linetype = 2, size = 2) +
  geom_segment(aes(x =521, xend = 1040, y = dfmeans[13,2], yend = dfmeans[13,2]), colour = colpal[3], linetype = 2, size = 2) +
  geom_segment(aes(x =521, xend = 1040, y = dfmeans[2,2], yend = dfmeans[2,2]), colour = colpal[4], linetype = 2, size = 2) +
  geom_segment(aes(x =521, xend = 1040, y = dfmeans[6,2], yend = dfmeans[6,2]), colour = colpal[5], linetype = 2, size = 2)

ggsave("Cover.png", width = 21, height = 21, units = "cm")# dpi = 300))


# rugosity
p4= ggplot()+
  stat_summary(data = df, aes(x=timestep, y=rugosity), fun.y = mean, geom = "line", size=1.2) +
  stat_summary(data = df, aes(x=timestep, y=rugosity), fun.ymin = se.min, fun.ymax = se.max, geom = "ribbon", alpha=0.3, colour = NA) +
  labs(x = "")+
  labs(y = "Rugosity ± SE")+
  theme_bw()+
  theme(legend.text=element_text(size=16))+
  theme(legend.title=element_text(size=16))+
  #theme(axis.text  = element_text(angle=90, vjust=0.5, size=12)) +
  theme(axis.text  = element_text(size=16)) +
  theme(axis.title=element_text(size=16),
        plot.margin = margin(1, 1, 0, 0.2, "cm"))
#scale_x_continuous(breaks = seq(0,ts,by = 100)) +
#scale_y_continuous(breaks = seq(0,100, by = 10))
p4
ggsave("Rugosity.png", width = 21, height = 21, units = "cm")# dpi = 300))


p = plot_grid(p1, p2, p3, p4, align = "h", ncol = 4, rel_widths = c(2/5, 2/5, 2/5, 2/5), labels=c('a)', 'b)', 'c)', 'd)'))
label = ggdraw() + draw_label("Timestep (weeks)", fontface='bold')
plot_grid(p, label, ncol=1, rel_heights=c(1, 0.1)) # rel_heights values control title margins
ggsave("all3.png", width = 40, height = 12, units = "cm")


#To get higher resolution, just make your window bigger before creating the movie. You can default to a larger window by running code like

#r3dDefaults$windowRect <- c(100, 100, 1000, 1000)
