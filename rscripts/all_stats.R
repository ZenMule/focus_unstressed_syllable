## LOAD PACKAGES ####
library(tidyverse)
library(car)

## PREPARE THE DATASETS FOR STATS ANALYSIS ####
data_stats <- data_clean
summary(data_stats)

## Gesture Duration ####
gdur_aov <- aov(GDUR ~ Info_str * Footedness * Place * Speaker, data = data_stats)
gdur_aov_sum <- summary(gdur_aov)
gdur_aov_sum
# Info_str  (F(3, 531) = 4.11, p < .01)
# Place     (F(2, 531) = 3.18, p < .001)
# Speaker   (F(3, 531) = 9.52, p < .001)
# Info:Sp   (F(9, 531) = 2.45, p < .01)
# Pl:Sp     (F(6, 531) = 11.49, p < .001)
# In:Pl:Sp  (F(18, 531) = 1.90, p < .05)
# Ft:Pl:Sp  (F(6, 531) = 2.93, p < .01)

# post-hoc test
gdur_aov_tukey <- TukeyHSD(gdur_aov)
gdur_aov_tukey$Info_str
# cntr-bck : 8.13ms longer
# cntr-brd : 7.72ms longer
gdur_aov_tukey$Place
# tb-ll: 35.12ms longer
# tb-tt: 38.72ms longer

## Displacement ####
disp_aov <- aov(MDISP ~ Info_str * Footedness * Place * Speaker, data = data_stats)
disp_aov_sum <- summary(disp_aov)
disp_aov_sum
# Info    F(3, 531) = 7.23 (p < .001)
# Ft      F(1, 531) = 10.96 (p < .001)
# Pl      F(2, 531) = 182.31 (p < .001)
# Sp      F(3, 531) = 71.08 (p < .001)
# Ft:Pl   F(6, 531) = 3.04 (p < .05)
# Info:Sp F(9, 531) = 2.75 (p < .01)
# Pl:Sp   F(6, 531) = 30.64 (p < .001)

# post-hoc test
disp_aov_tukey <- TukeyHSD(disp_aov)
disp_aov_tukey$Info_str
# cntr-bck: 0.89mm longer
# cntr-brd: 0.79mm longer
# cntr-nrw: 0.49mm longer (p = .09 < .1)
disp_aov_tukey$Footedness
# ft-unf: 0.48 longer
disp_aov_tukey$Place
# tb-ll: 2.90mm longer
# tb-tt: 3.02mm longer

# Place:Footedness interaction
disp_pl.ft <- data_stats %>%
  group_by(Footedness, Place) %>%
  summarize(
    n=n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
    )
disp_place.ft_fig <- ggplot(disp_pl.ft, aes(Footedness, DISP_mean, ymax = DISP_max, ymin = DISP_min, group = Place)) +
  geom_pointrange(aes(shape = Place), position = position_dodge(width = 0.1)) +
  geom_line(aes(linetype = Place), position = position_dodge(width = 0.1)) +
  labs(title = "Interaction between place of articulation and footedness in displacement",
       y = "mm") +
  theme_bw() + 
  theme(plot.title = tc)


## Peak velocity ####
pvel_aov <- aov(PVEL ~ Info_str * Footedness * Place * Speaker, data = data_stats)
pvel_aov_sum <- summary(pvel_aov)
pvel_aov_sum
# Info:     F(3, 531) = 4.61 (p < .01)
# Ft:       F(1, 531) = 11.21 (p < .001)
# Pl:       F(2, 531) = 21.97 (p < .001)
# Sp:       F(3, 531) = 106.54 (p < .001)
# Ft:Pl     F(2, 531) = 4.08 (p < .05)
# Info:Sp   F(9, 531) = 1.95 (p < .05)
# Pl:Sp     F(6, 531) = 28.78 (p < .001)
# Ft:Pl:Sp  F(6, 531) = 2.71 (p < .001)

# post-hoc test
pvel_aov_tukey <- TukeyHSD(pvel_aov)
pvel_aov_tukey$Info_str
# cntr-bck: 9.77mm/s faster
# cntr-brd: 7.00mm/s faster
pvel_aov_tukey$Footedness
# ft-unf: 6.37mm/s faster
pvel_aov_tukey$Place
# tt-ll = 6.16mm/s faster
# tb-ll = 15.23mm/s faster
# tb-tt = 9.07mm/s faster

# Foot:Place interaction
pv_pl.ft <- data_stats %>%
  group_by(Footedness, Place) %>%
  summarize(
    n=n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
pv_pl.ft_fig <- ggplot(pv_pl.ft, aes(Footedness, PV_mean, ymax = PV_max, ymin = PV_min, group = Place)) +
  geom_pointrange(aes(shape = Place), position = position_dodge(width = 0.1)) +
  geom_line(aes(linetype = Place), position = position_dodge(width = 0.1)) +
  labs(title = "Interaction between place of articulation and footedness in peak velocity",
       y = "mm/s") +
  theme_bw() + 
  theme(plot.title = tc)

## T2PVEL ####
t2pvel_aov <- aov(T2PVEL ~ Info_str * Footedness * Place * Speaker, data = data_stats)
t2pvel_aov_sum <- summary(t2pvel_aov)
t2pvel_aov_sum
# Info:     F(3, 531) = 3.21 (p < .05)
# Ft:       F(1, 531) = 5.38 (p < .05)
# Pl:       F(3, 531) = 104.09 (p < .001)
# SP:       F(3, 531) = 28.12 (p < .001)
# Info:Pl   F(6, 531) = 3.22 (p < .01)
# Ft:Pl     F(2, 531) = 4.09 (p < .05)

# post-hoc test
t2pvel_aov_tukey <- TukeyHSD(t2pvel_aov)
t2pvel_aov_tukey$Info_str
# cntr-bck: 5.40ms less stiff
# cntr-brd: 5.20ms less stiff
t2pvel_aov_tukey$Footedness
# ft-unf: 3.21ms less stiff
t2pvel_aov_tukey$Place
# tb-ll: 22.42ms less stiff
# tb-tt: 19.55ms less stiff

# Place:Info_str
t2pvel_pl.inf <- data_stats %>%
  group_by(Place, Info_str) %>%
  summarize(
    n=n(),
    T2PV_mean = mean(T2PVEL),
    T2PV_max = T2PV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    T2PV_min = T2PV_mean - 1.96*sd(T2PVEL)/sqrt(n)
    ) 
t2pvel_pl.inf_fig <- t2pvel_pl.inf %>%
  ggplot(aes(Info_str, T2PV_mean, ymax = T2PV_max, ymin = T2PV_min, group = Place)) +
    geom_pointrange(aes(shape = Place), position = position_dodge(width = 0.1)) +
    geom_line(aes(linetype = Place), position = position_dodge(width = 0.1)) +
    labs(title="Interaction between information structure and place in time-to-peak velocity",
         x="Information structure",
         y="ms") +
    theme_bw() +
    theme(plot.title=tc)
t2pvel_pl.inf_fig

# Ft:Pl
t2pvel_ft.pl <- data_stats %>%
  group_by(Place, Footedness) %>%
  summarize(
    n=n(),
    T2PV_mean = mean(T2PVEL),
    T2PV_max = T2PV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    T2PV_min = T2PV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  ) 
t2pvel_ft.pl_fig <- t2pvel_ft.pl %>%
  ggplot(aes(Footedness, T2PV_mean, ymax = T2PV_max, ymin = T2PV_min, group = Place)) +
  geom_pointrange(aes(shape = Place), position = position_dodge(width = 0.1)) +
  geom_line(aes(linetype = Place), position = position_dodge(width = 0.1)) +
  labs(title="Interaction between footedness and place in time-to-peak velocity",
       x="Information structure",
       y="ms") +
  theme_bw() +
  theme(plot.title=tc)
t2pvel_ft.pl_fig
