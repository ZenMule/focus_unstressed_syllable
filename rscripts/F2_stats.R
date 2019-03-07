## LOAD PACKAGES ####
library(tidyverse)
library(ez)
library(effsize)

f2_stats
summary(f2_stats)

aov(GDUR ~ Info_str * Footedness * Place, data = f2_stats) %>% summary()
aov(MDISP ~ Info_str * Footedness * Place, data = f2_stats) %>% summary()
aov(PVEL ~ Info_str * Footedness * Place, data = f2_stats) %>% summary()
aov(T2PVEL ~ Info_str * Footedness * Place, data = f2_stats) %>% summary()
