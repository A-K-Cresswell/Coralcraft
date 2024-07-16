## SOURCE SCRIPT 
# for reading in outputs of models

#### read in scenario file ####
setwd(work.dir)
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth forms.csv")

labs = as.list(scens.id$labels)
code = as.list(scens.id$code)
old.labs = as.list(scens.id$names)
################################################################################################################
setwd(output)
#### READ IN FILES----
# DIVERSITY (all.10, bottom 5, complex 5)
df_met = read.csv("1_df_allmetrics.csv")
juv1 = read.csv("1_df_juv.prot.csv")

# MONOSPECIFIC COMMUNITIES (1fts only + all.10)
df_met_1fts = read.csv("2_df_allmetrics.csv")
juv2 = read.csv("2_df_juv.prot_1fts.csv")

for (i in 1:13){
  df_met$scenarios[df_met$scenarios %in% code[i]] <- labs[i]
  df_met_1fts$scenarios[df_met_1fts$scenarios %in% code[i]] <- labs[i]
  juv1$scenarios[juv1$scenarios %in% old.labs[i]] <- labs[i]
  juv2$scenarios[juv2$scenarios %in% old.labs[i]] <- labs[i]
  print(i)
}

df_met$scenarios = factor(df_met$scenarios, levels = labs)
df_met_1fts$scenarios = factor(df_met_1fts$scenarios, levels = labs)
################################################################################################################
## 1. DIVERSITY----

# Add totals
df1 <- df_met %>% 
  mutate(lin_rugos = lin_rug/100,
         sur_rugos = sur_rug/10000,
         sheltervol=numcoveredcells/1000, 
         meansides2 = meansides*ws*ws, # number of covered cells
         sidesprop = meansides/100, #prop. of sides covered
         meantop_2 = meantop_2*100,
         meantop_10 = meantop_10*100,
         meantop_1_15 = meantop_1_15*100,
         meantop_1_20 = meantop_1_20*100,
         year= ts/52) 

df.top <- df1[, names(df1) %in% c("run", "ts", "year", "id", "scenarios", "meantop_2", "meantop_10", "meantop_1_15", "meantop_1_20")]

df.long <- df.top %>% 
  gather(key=height, value=meanshelter, meantop_2:meantop_1_20) %>% 
  mutate(height = replace(height, height == "meantop_2", "2cm"),
         height = replace(height, height == "meantop_10", "10cm"),
         height = replace(height, height == "meantop_1_15", "1-15cm"),
         height = replace(height, height == "meantop_1_20", "1-20cm"))
df.long$height = factor(df.long$height, 
                        levels = c("1-15cm", "1-20cm", "2cm", "10cm"))
df.long1 = df.long %>%
  filter(height%in%c("1-15cm", "2cm", "10cm"))

## 2. 1FTS ----
# Add totals
df2 <- df_met_1fts %>% 
  mutate(lin_rugos = lin_rug/100,
         sur_rugos = sur_rug/10000,
         sheltervol=numcoveredcells/1000, 
         meansides2 = meansides*ws*ws, # number of covered cells
         sidesprop = meansides/100, #prop. of sides covered
         meantop_2 = meantop_2*100,
         meantop_10 = meantop_10*100,
         meantop_1_15 = meantop_1_15*100,
         meantop_1_20 = meantop_1_20*100,
         year= ts/52) 

df.top2 <- df2[, names(df2) %in% c("run", "ts", "year", "id", "scenarios", "meantop_2", "meantop_10", "meantop_1_15", "meantop_1_20")]

df.long2 <- df.top2 %>% 
  gather(key=height, value=meanshelter, meantop_2:meantop_1_20) %>% 
  mutate(height = replace(height, height == "meantop_2", "2cm"),
         height = replace(height, height == "meantop_10", "10cm"),
         height = replace(height, height == "meantop_1_15", "1-15cm"),
         height = replace(height, height == "meantop_1_20", "1-20cm"))
df.long2$height = factor(df.long2$height, 
                         levels = c("1-15cm", "1-20cm", "2cm", "10cm"))
df.long22 = df.long2 %>%
  filter(height%in%c("1-15cm", "2cm", "10cm"))
################################################################################################################
#### Size-dependent Shelter----
# 1. DIVERSITY
juv1$scenarios = factor(juv1$scenarios, levels = scens.id$labels)
juv1_long <- juv1 %>% 
  gather(key=predsize, value=mres, juv_1_9:juv_3_21) %>% 
  mutate(preysize = ifelse(predsize == "juv_1_9", "Small Prey", 
                           ifelse(predsize == "juv_1_21", "Small Prey", "Medium Prey"))) %>% 
  mutate(predsize = replace(predsize, predsize == "juv_1_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_1_21", "Medium Predator"),
         predsize = replace(predsize, predsize == "juv_3_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_3_21", "Medium Predator")) %>% 
  mutate(mres=mres*100,
         year = ts/52)

juv1_long$predsize = factor(juv1_long$predsize, 
                            levels = c("Small Predator", "Medium Predator"))
juv1_long$preysize = factor(juv1_long$preysize, 
                            levels = c("Small Prey", "Medium Prey"))

# 1fts only
juv2$scenarios = factor(juv2$scenarios, levels = scens.id$labels)
juv2_long <- juv2 %>% 
  gather(key=predsize, value=mres, juv_1_9:juv_3_21) %>% 
  mutate(preysize = ifelse(predsize == "juv_1_9", "Small Prey", 
                           ifelse(predsize == "juv_1_21", "Small Prey", "Medium Prey"))) %>% 
  mutate(predsize = replace(predsize, predsize == "juv_1_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_1_21", "Medium Predator"),
         predsize = replace(predsize, predsize == "juv_3_9", "Small Predator"),
         predsize = replace(predsize, predsize == "juv_3_21", "Medium Predator")) %>% 
  mutate(mres=mres*100,
         year = ts/52)

juv2_long$predsize = factor(juv2_long$predsize, 
                            levels = c("Small Predator", "Medium Predator"))
juv2_long$preysize = factor(juv2_long$preysize, 
                            levels = c("Small Prey", "Medium Prey"))




