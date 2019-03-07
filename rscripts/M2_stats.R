## LOAD PACKAGES ####
library(tidyverse)
library(ez)
library(effsize)

m2_stats
summary(m2_stats)

aov(GDUR ~ Info_str * Footedness * Place, data = m2_stats) %>% summary()
aov(GDUR ~ Info_str * Footedness * Place, data = m2_stats) %>% TukeyHSD()

aov(MDISP ~ Info_str * Footedness * Place, data = m2_stats) %>% summary()
aov(MDISP ~ Info_str * Footedness * Place, data = m2_stats) %>% TukeyHSD()

aov(PVEL ~ Info_str * Footedness * Place, data = m2_stats) %>% summary()
aov(PVEL ~ Info_str * Footedness * Place, data = m2_stats) %>% TukeyHSD()

aov(T2PVEL ~ Info_str * Footedness * Place, data = m2_stats) %>% summary()
aov(T2PVEL ~ Info_str * Footedness * Place, data = m2_stats) %>% TukeyHSD()

