## SOURCE SCRIPT 
# for reading in outputs of models

#### read in scenario file ####
scens.id=read.csv("scenarios_id.csv")
scens.csv=read.csv("scenarios.csv", header=T)
fts=read.csv("growth_forms.csv")

labs = as.list(scens.id$labels)
################################################################################################################
#### READ IN FILES----
# DIVERSITY (all.10, bottom 5, complex 5)
df_met = read.csv("df_metrics.csv", path = output)

# MONOSPECIFIC COMMUNITIES (1fts only + all.10)
df_met_1fts = read.csv("df_metrics_1fts.csv", path = output)

df_met$scenarios = factor(df_met$scenarios, levels = labs)
df_met_1fts$scenarios = factor(df_met_1fts$scenarios, levels = labs)
################################################################################################################
## 1. DIVERSITY----

# Add totals
df1 <- df_met %>% 
  mutate(coral.cover = coral_cover*100,
         linear_rugosity = linear_rugosity/100,
         surface_rugosity = surface_rugosity/10000,
         shelter_volume=shelter_volume/1000, 
         demersal_shelter = demersal_shelter/100, #prop. of sides covered
         pelagic_shelter_2 = pelagic_shelter_2*100,
         pelagic_shelter_1_15 = pelagic_shelter_1_15*100,
         size_dependent = size_dependent*100,
         year= ts/52) 

# mean 
df11 = df1 %>% 
  group_by(ts, scenarios) %>% 
  summarise(coral.cover = mean(coral.cover),
            lin_rug = mean(linear_rugosity),
            sur_rug = mean(surface_rugosity),
            fracdim = mean(fracdim), 
            sheltervol = mean(shelter_volume), 
            meansides = mean(demersal_shelter))

## 2. 1FTS ----
# Add totals
df2 <- df_met_1fts %>% 
  mutate(coral.cover = coral_cover*100,
         linear_rugosity = linear_rugosity/100,
         surface_rugosity = surface_rugosity/10000,
         shelter_volume=shelter_volume/1000, 
         demersal_shelter = demersal_shelter/100, #prop. of sides covered
         pelagic_shelter_2 = pelagic_shelter_2*100,
         pelagic_shelter_1_15 = pelagic_shelter_1_15*100,
         size_dependent = size_dependent*100,
         year= ts/52) 

# mean 
df22 = df1 %>% 
  group_by(ts, scenarios) %>% 
  summarise(coral.cover = mean(coral.cover),
            lin_rug = mean(linear_rugosity),
            sur_rug = mean(surface_rugosity),
            fracdim = mean(fracdim), 
            sheltervol = mean(shelter_volume), 
            meansides = mean(demersal_shelter))
###############################################################################################################




