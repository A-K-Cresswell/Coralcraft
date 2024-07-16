### Universal S.E. and C.I. functions for stat_summary

# standard error
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

### CORALCRAFT GRAPHING PARAMETERS
colpal = c("firebrick4", "navy", "plum4",
           "tomato", "gold", "springgreen4", "dodgerblue", "blueviolet",
           "darkolivegreen1", "aquamarine3", "skyblue3", "orchid1","orange")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

## COMBINED ##
linetype0 = c(rep("dashed", each=3), rep("solid", each=10))
linesize0 = c(rep(1, each=3), rep(0.5, each=10))
colpal0 = colpal[1:13]

## COMBINED ##
linetype00 = c(rep("dashed", each=1), rep("solid", each=2))
linesize00 = c(rep(2, each=1), rep(2.5, each=2))
colpal00 = colpal[c(1,8,13)]

## DIVERSITY ##
linetype1 = rep("solid", each=3)
linesize1 = rep(1.5, each=3)
colpal1 = colpal[1:3]

## 1 FTS ONLY ##
linetype2 = c("dashed", rep("solid", each=10))
linesize2 = c(2, rep(2, each=10))
colpal2 = colpal[c(1,4:13)] 

## FUNCTIONAL REDUNDANCY ##
linetype3 = rep("dotted", each=10)
linesize3 = rep(0.5, each=10)
colpal3 = colpal[4:13]

## CORRPLOT ##
colCOR = c(rep("red", each=3), 
           rep("black", each=8))

## THEME ##
theme1 = theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18),
        strip.text = element_text(size=16),
        legend.text = element_text(size = 16),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.title = element_blank(),
        legend.box.margin = unit(c(1,1,1,1),"cm"))


theme2 = theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18),
        strip.text = element_text(size=16),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        legend.box.margin = unit(c(1,1,1,1),"cm"))











