## LOAD PACKAGES ####
library(tidyverse)
library(ez)
library(effsize)

f1_stats
summary(f1_stats)

aov(GDUR ~ Info_str * Footedness * Place, data = f1_stats) %>% summary()
aov(MDISP ~ Info_str * Footedness * Place, data = f1_stats) %>% summary()
aov(PVEL ~ Info_str * Footedness * Place, data = f1_stats) %>% summary()
aov(T2PVEL ~ Info_str * Footedness * Place, data = f1_stats) %>% summary()
