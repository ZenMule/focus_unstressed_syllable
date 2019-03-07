## LOAD PACKAGES ####
library(tidyverse)
library(ez)
library(effsize)

m1_stats
summary(m1_stats)

aov(GDUR ~ Info_str * Footedness * Place, data = m1_stats) %>% summary()
aov(MDISP ~ Info_str * Footedness * Place, data = m1_stats) %>% summary()
aov(PVEL ~ Info_str * Footedness * Place, data = m1_stats) %>% summary()
aov(T2PVEL ~ Info_str * Footedness * Place, data = m1_stats) %>% summary()
