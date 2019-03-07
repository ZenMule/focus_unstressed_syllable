## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)

## LOAD OBJECTS ####
f1_fig <- f1_stats

## SET POSITION FOR TITLE AND DODGE ####
pd <- position_dodge(width=0.18)
tc <- element_text(hjust = 0.5)

## 1. Place by Info_str ####
## opening duration ####
# 95% CI
f1_gdur_sum <- f1_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    GDUR_mean = mean(GDUR),
    GDUR_max = GDUR_mean + 1.96*sd(GDUR)/sqrt(n),
    GDUR_min = GDUR_mean - 1.96*sd(GDUR)/sqrt(n)
  )
f1_gdur_sum
# plot
f1_gdur_fig <- f1_gdur_sum %>%
  ggplot(aes(Info_str, GDUR_mean, ymin = GDUR_min, ymax = GDUR_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="a) Opening Duration",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f1_gdur_fig  

pdf("figures/f1_gdur_fig.pdf")
f1_gdur_fig
dev.off()

## displacement ####
# 95% CI
f1_disp_sum <- f1_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
  )
f1_disp_sum
# plot
f1_disp_fig <- f1_disp_sum %>%
  ggplot(aes(Info_str, DISP_mean, ymin = DISP_min, ymax = DISP_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="b) Displacement",
       x="Information Structure", 
       y="mm") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f1_disp_fig 

pdf("figures/f1_disp_fig.pdf")
f1_disp_fig
dev.off()

## peak velocity ####
# 95% CI
f1_pv_sum <- f1_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
f1_pv_sum
# plot
f1_pvel_fig <- f1_pv_sum %>%
  ggplot(aes(Info_str, PV_mean, ymin = PV_min, ymax = PV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="c) Peak Velocity",
       x="Information Structure", 
       y="mm/s") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f1_pvel_fig 

pdf("figures/f1_pvel_fig.pdf")
f1_pvel_fig 
dev.off()

## time to peak velocity ####
# 95% CI
f1_t2pv_sum <- f1_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    TPV_mean = mean(T2PVEL),
    TPV_max = TPV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    TPV_min = TPV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  )
f1_t2pv_sum
# plot
f1_t2pv_fig <- f1_t2pv_sum %>%
  ggplot(aes(Info_str, TPV_mean, ymin = TPV_min, ymax = TPV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.618, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="d) Time-to-Peak Velocity",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f1_t2pv_fig 

pdf("figures/f1_t2pv_fig.pdf")
f1_t2pv_fig
dev.off()

## 4 in 1 ####
f1_arr <- ggarrange(f1_gdur_fig, 
                     f1_disp_fig, 
                     f1_pvel_fig, 
                     f1_t2pv_fig, 
                     ncol = 2, nrow = 2, 
                     common.legend = T, legend="right")
f1_arr
f1_plot <- annotate_figure(f1_arr,
                            top = text_grob("F1: Means and 95% CIs for the kinematic measures by information structure", 
                                            color = "black", 
                                            size = 14))
f1_plot

pdf("figures/f1_plot.pdf")
f1_plot
dev.off()
