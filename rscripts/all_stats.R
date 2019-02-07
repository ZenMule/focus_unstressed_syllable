## LOAD PACKAGES ####
library(tidyverse)
library(car)

## PREPARE THE DATASETS FOR STATS ANALYSIS ####
all_stats <- all
summary(all_stats)

## MANOVA ####
manova_all <- manova(cbind(GDUR, MDISP, PVEL, T2PVEL) ~ Info_str * Footedness * Place, data = all_stats)
summary(manova_all)
summary.aov(manova_all)
# Info_str. GDUR: F(3, 286) = 8.60 (p < .05), 
#           MDISP: F(3, 286) = 6.82 (p < .05),
#           T2PVEL: F(3, 286) = 6.12 (p < .05).
# Footedness. MDISP: F(1, 286) = 4.36 (p < .05),
#             PVEL: F(1, 286) = 4.09 (p < .05).
# Place. GDUR: F(2, 286) = 204.50 (p < .05),
#        MDISP: F(2, 286) = 208.58 (p < .05),
#        PVEL: F(2, 286) = 25.1498 (p < .05),
#        T2PVEL: F(2, 286) = 59.41 ( p < .05).
# Info_str:Footedness. NA
# Info_str:Place. GDUR: F(6, 286) = 2.56 (p <. 05),
#                 T2PVEL: F(6, 286) = 3.34 (p < .05).
# Place:Fotoedness. MDISP: F(2, 286) = 5.08 (p < .05)

## GDUR ####
gdur_aov <- aov(GDUR ~ Info_str * Footedness * Place, data = all_stats)
gdur_aov_sum <- summary(gdur_aov)
gdur_aov_sum
gdur_aov_tukey <- TukeyHSD(gdur_aov)
gdur_aov_tukey$Info_str
gdur_aov_tukey$Place
# c-0 : 9.40ms longer
# c-b : 15.70ms longer
# tt-ll: 8.04ms longer
# tb-ll: 50.62ms longer
# tb-tt: 42.58ms longer

# place:info_str interaction
gdur_info_place <- all_stats %>% 
  group_by(Place, Info_str) %>%
  summarize(GDUR_mean = mean(GDUR)) %>%
  spread(Info_str, GDUR_mean)
gdur_info_place
# ll:c-ll:0 = 6.4ms
# tt:c-tt:b = 26.2ms
# tb:c-tb:b = 18ms

## MDISP ####
mdisp_aov <- aov(MDISP ~ Info_str * Footedness * Place, data = all_stats)
mdisp_aov_sum <- summary(mdisp_aov)
mdisp_aov_sum
mdisp_aov_tukey <- TukeyHSD(mdisp_aov)
mdisp_aov_tukey$Info_str
mdisp_aov_tukey$Footedness
mdisp_aov_tukey$Place
# c-0 = 1.14mm
# c-b = 1.37mm
# c-n = 0.85mm
# f-unf = 0.48mm
# tt-ll = 0.79mm
# tb-ll = 5.31mm
# tb-tt = 4.52mm

# Place:Footedness
mdisp_place_foot <- all_stats %>%
  group_by(Footedness, Place) %>%
  summarize(MDISP_mean = mean(MDISP)) %>%
  spread(Place, MDISP_mean)
mdisp_place_foot
# ll:f-ll:unf = 1.38mm
# tt:f-tt:unf = -0.37mm
# tb:f-tb:unf = 0.52mm

## PVEL ####
pvel_aov <- aov(PVEL ~ Info_str * Footedness * Place, data = all_stats)
pvel_aov_sum <- summary(pvel_aov)
pvel_aov_sum
pvel_aov_tukey <- TukeyHSD(pvel_aov)
pvel_aov_tukey$Footedness
pvel_aov_tukey$Place
# f-unf = 8.23mm/s faster
# tt-ll = 15.12mm/s faster
# tb-ll = 35.17mm/s faster
# tb-tt = 20.06mm/s faster

## T2PVEL ####
t2pvel_aov <- aov(T2PVEL ~ Info_str * Footedness * Place, data = all_stats)
t2pvel_aov_sum <- summary(t2pvel_aov)
t2pvel_aov_sum
t2pvel_aov_tukey <- TukeyHSD(t2pvel_aov)
t2pvel_aov_tukey$Place
t2pvel_aov_tukey$Info_str
# c-0: 6.34ms
# c-b: 9.84ms
# tt-ll: 13.32ms
# tb-ll: 21.56ms
# tb-tt: 8.24ms

# Place:Info_str
t2pvel_place_info <- all_stats %>%
  group_by(Place, Info_str) %>%
  summarize(T2PVEL_mean = mean(T2PVEL)) %>%
  spread(Place, T2PVEL_mean)
t2pvel_place_info
# ll:n-0 = 2.3ms
# tt:c-b = 23.9ms
# tb:c-b = 12.9ms