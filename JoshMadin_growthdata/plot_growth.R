setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/SCIENCE ARCHIVE/MANUSCRIPT REVIEWS/22 10 Ecology - Demographic data/Data_S1/all_data")

dir()


dat = read.csv("growth.csv")


ggplot(aes(x = area_cm2, y = area_cm2_next), dat = dat) +
  facet_wrap(~species, scales = "free" ) +
  geom_point() +
  theme_bw()
