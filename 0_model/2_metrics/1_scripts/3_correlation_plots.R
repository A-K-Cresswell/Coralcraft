## CORRELATION OF METRICS ##

rm(list=ls())
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
library(PerformanceAnalytics)
library(psych)
library(Hmisc)
library(corrplot)


## set working directory
work.dir = getwd()
sim.wd = paste(work.dir,"1_Simulations", "1_Outputs", "2_Scenarios", sep="/")
met.wd = paste(work.dir,"2_Metrics", sep="/")
plots = paste(met.wd,"1_MetricsPlots", sep="/") #for saving plots
output = paste(met.wd, "1_Outputs", sep="/") #for saving outputs
script = paste(met.wd,"1_MetricsScripts", sep="/") #for scripts
paper = paste(work.dir, "3_paper", sep = "/") #for saving plots

# load S.E & C.I.
setwd(script)
source("plot_par.R")
source("read_output.R")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
####################################################################################################
# DATAFRAME PREP
df_met = rbind(df1, df2)
df_juv = rbind(juv1, juv2)
df_shel = merge(df_met, df_juv, by = c("run", "ts", "id", "scenarios"))
df_shel$scenarios = factor(df_shel$scenarios, levels = scens.id$labels)

dat <- df_shel %>% 
  group_by(scenarios, run, ts) %>% 
  filter(ts == 260)
#################################################################################################################
### 1_DIVERSITY
dat1 = dat %>% 
  filter(scenarios %in% labs[1:3])

corplot = dat1[, names(dat1) %in% 
                 c("lin_rugos", "linear_100", 
                   "sur_rugos", "fracdim", "numcoveredcells", "meansides", "meantop_1_15", 
                   "juv_1_9", "juv_1_21"
                   ,"juv_3_9","juv_3_21")]
col_order = c("lin_rugos", "linear_100", 
              "sur_rugos", "fracdim", "numcoveredcells", 
              "meansides", "meantop_1_15", "juv_1_9", "juv_1_21"
              ,"juv_3_9","juv_3_21")
corplot <- corplot[, col_order]
colnames(corplot)[which(names(corplot) == "lin_rugos")] <- "linear_rugosity"
colnames(corplot)[which(names(corplot) == "linear_100")] <- "linear_rugosity_100"
colnames(corplot)[which(names(corplot) == "sur_rugos")] <- "surface_rugosity"
colnames(corplot)[which(names(corplot) == "numcoveredcells")] <- "shelter_volume"
colnames(corplot)[which(names(corplot) == "meantop_1_15")] <- "pelagic_shelter"
colnames(corplot)[which(names(corplot) == "meansides")] <- "demersal_shelter"
colnames(corplot)[which(names(corplot) == "fracdim")] <- "fractal_dimension"
colnames(corplot)[which(names(corplot) == "juv_1_9")] <- "S Prey S Pred"
colnames(corplot)[which(names(corplot) == "juv_1_21")] <- "S Prey M Pred"
colnames(corplot)[which(names(corplot) == "juv_3_9")] <- "M Prey S Pred"
colnames(corplot)[which(names(corplot) == "juv_3_21")] <- "M Prey M Pred"

## Heatmap
res<-cor(corplot)
p.mat <- cor.mtest(corplot)

setwd(paper)
png(height=1800, width=1800, file="S1_corrplot.png", type = "cairo")

corrplot(res, 
         method="circle", 
         col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col=colCOR, #Text label color 
         tl.srt=45, 
         tl.cex = 3, cl.cex = 3, number.cex = 3, #Text rotation and size
         p.mat = p.mat, insig = "blank")

dev.off()


## r = -0.08 for fracdim v 3D rugosity
ggplot()+
  stat_summary(data = dat1, aes(x=linear_rugosity, y=meantop_1_15, colour=scenarios, group=scenarios ), fun = mean, geom = "point")+
  stat_summary(data = dat1, aes(x=linear_rugosity, y=meantop_1_15, colour=scenarios , group=scenarios ), fun.min = se.min, fun.max = se.max, geom = "ribbon", alpha=0.3, colour = NA) +
  # stat_cor(data = dat1, aes(x=linear_rugosity, y=meantop_1_15), method = 'pearson')+
  geom_smooth(data = dat1, aes(x=linear_rugosity, y=meantop_1_15), method ="lm", formula = y~x)+
  scale_colour_manual(values = colpal) +
  scale_fill_manual(values = colpal)+
  # labs(y = "S Prey S Pred")+
  # labs(x = "Shelter Volume")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))

ggsave("1_juv.1_9 vs Shel.Vol.png", path = cor,
       width=22, height=15, units="cm", dpi=300)
#################################################################################################################
## 2_1fts: only 1fts scenarios
dat2 = dat %>%
  filter(scenarios %in% labs[4:13])

corplot = dat2[, names(dat2) %in% 
                 c("lin_rugos", "linear_100", "sur_rugos", "fracdim", "numcoveredcells", "meansides", "meantop_1_15", 
                   "juv_1_9", "juv_1_21","juv_3_9","juv_3_21")]
col_order = c("lin_rugos", "linear_100", "sur_rugos", "fracdim", "numcoveredcells", 
              "meansides", "meantop_1_15", "juv_1_9", "juv_1_21","juv_3_9","juv_3_21")
corplot <- corplot[, col_order]
colnames(corplot)[which(names(corplot) == "lin_rugos")] <- "linear_rugosity"
colnames(corplot)[which(names(corplot) == "linear_100")] <- "linear_rugosity_100"
colnames(corplot)[which(names(corplot) == "sur_rugos")] <- "surface_rugosity"
colnames(corplot)[which(names(corplot) == "numcoveredcells")] <- "shelter_volume"
colnames(corplot)[which(names(corplot) == "meantop_1_15")] <- "pelagic_shelter"
colnames(corplot)[which(names(corplot) == "meansides")] <- "demersal_shelter"
colnames(corplot)[which(names(corplot) == "fracdim")] <- "fractal_dimension"
colnames(corplot)[which(names(corplot) == "juv_1_9")] <- "S Prey S Pred"
colnames(corplot)[which(names(corplot) == "juv_1_21")] <- "S Prey M Pred"
colnames(corplot)[which(names(corplot) == "juv_3_9")] <- "M Prey S Pred"
colnames(corplot)[which(names(corplot) == "juv_3_21")] <- "M Prey M Pred"

## Heatmap
res<-cor(corplot)
p.mat <- cor.mtest(corplot)

setwd(paper)
png(height=1800, width=1800, file="S2_corrplot.png", type = "cairo")

corrplot(res, 
         method="circle", 
         col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col=colCOR, #Text label color 
         tl.srt=45, tl.cex = 3, cl.cex = 3, number.cex = 3, #Text rotation and size
         p.mat = p.mat, insig = "blank", # add sig. level
)

dev.off()
