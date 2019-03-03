## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)

## LOAD OBJECTS ####
data_fig <- data_clean

## SET POSITION FOR TITLE AND DODGE ####
pd <- position_dodge(width=0.18)
tc <- element_text(hjust = 0.5)


## opening duration ####
# 95% CI
gdur_fig <- data_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    GDUR_mean = mean(GDUR),
    GDUR_max = GDUR_mean + 1.96*sd(GDUR)/sqrt(n),
    GDUR_min = GDUR_mean - 1.96*sd(GDUR)/sqrt(n)
  )
gdur_fig
# plot
gdur_all_fig <- gdur_fig %>%
  ggplot(aes(Info_str, GDUR_mean, ymin = GDUR_min, ymax = GDUR_max, group = Place)) + 
    geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
    geom_line(aes(linetype = Place), position = pd) +
    labs(title="a) Gesture Opening Duration",
       x="Information Structure", 
       y="ms") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = tc) 
gdur_all_fig  

## displacement ####
# 95% CI
disp_fig <- data_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
  )
disp_fig
# plot
disp_all_fig <- disp_fig %>%
  ggplot(aes(Info_str, DISP_mean, ymin = DISP_min, ymax = DISP_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="b) Displacement",
       x="Information Structure", 
       y="mm") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
disp_all_fig  

## peak velocity ####
# 95% CI
pv_fig <- data_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
pv_fig
# plot
pvel_all_fig <- pv_fig %>%
  ggplot(aes(Info_str, PV_mean, ymin = PV_min, ymax = PV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="c) Peak Velocity",
       x="Information Structure", 
       y="mm/s") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
pvel_all_fig 

## time to peak velocity ####
# 95% CI
t2pv_fig <- data_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    TPV_mean = mean(T2PVEL),
    TPV_max = TPV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    TPV_min = TPV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  )
t2pv_fig
# plot
t2pv_all_fig <- t2pv_fig %>%
  ggplot(aes(Info_str, TPV_mean, ymin = TPV_min, ymax = TPV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="d) Time-to-Peak Velocity",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
t2pv_all_fig 

## 4 in 1 ####
all_arr <- ggarrange(gdur_all_fig, 
                      disp_all_fig, 
                      pvel_all_fig, 
                      t2pv_all_fig, 
                      ncol = 2, nrow = 2, 
                      common.legend = T, legend="bottom")
all_arr
all_plot <- annotate_figure(all_arr,
                            top = text_grob("Means and 95% CIs for the kinematic measures", 
                                            color = "black", 
                                            face = "bold", 
                                            size = 14))
all_plot

pdf("figures/all_plot.pdf")
all_plot
dev.off()
