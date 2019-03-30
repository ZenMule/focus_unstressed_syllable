## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)

## LOAD OBJECTS ####
f2_fig <- data_clean %>% filter(Speaker == "F2")

## SET POSITION FOR TITLE AND DODGE ####
pd <- position_dodge(width=0.18)
tc <- element_text(hjust = 0.5)

## 1. Place by Info_str ####
## opening duration ####
# 95% CI
f2_gdur_sum <- f2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    GDUR_mean = mean(GDUR),
    GDUR_max = GDUR_mean + 1.96*sd(GDUR)/sqrt(n),
    GDUR_min = GDUR_mean - 1.96*sd(GDUR)/sqrt(n)
  )
f2_gdur_sum
# plot
f2_gdur_fig <- f2_gdur_sum %>%
  ggplot(aes(Info_str, GDUR_mean, ymin = GDUR_min, ymax = GDUR_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="a) Opening Duration",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2_gdur_fig  

pdf("figures/f2_gdur_fig.pdf")
f2_gdur_fig
dev.off()

## displacement ####
# 95% CI
f2_disp_sum <- f2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
  )
f2_disp_sum
# plot
f2_disp_fig <- f2_disp_sum %>%
  ggplot(aes(Info_str, DISP_mean, ymin = DISP_min, ymax = DISP_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="b) Displacement",
       x="Information Structure", 
       y="mm") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2_disp_fig 

pdf("figures/f2_disp_fig.pdf")
f2_disp_fig
dev.off()

## peak velocity ####
# 95% CI
f2_pv_sum <- f2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
f2_pv_sum
# plot
f2_pvel_fig <- f2_pv_sum %>%
  ggplot(aes(Info_str, PV_mean, ymin = PV_min, ymax = PV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="c) Peak Velocity",
       x="Information Structure", 
       y="mm/s") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2_pvel_fig 

pdf("figures/f2_pvel_fig.pdf")
f2_pvel_fig 
dev.off()

## time to peak velocity ####
# 95% CI
f2_t2pv_sum <- f2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    TPV_mean = mean(T2PVEL),
    TPV_max = TPV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    TPV_min = TPV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  )
f2_t2pv_sum
# plot
f2_t2pv_fig <- f2_t2pv_sum %>%
  ggplot(aes(Info_str, TPV_mean, ymin = TPV_min, ymax = TPV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="d) Time-to-Peak Velocity",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2_t2pv_fig 

pdf("figures/f2_t2pv_fig.pdf")
f2_t2pv_fig
dev.off()

## 4 in 1 ####
f2_arr <- ggarrange(f2_gdur_fig, 
                    f2_disp_fig, 
                    f2_pvel_fig, 
                    f2_t2pv_fig, 
                    ncol = 2, nrow = 2, 
                    common.legend = T, legend="right")
f2_arr
f2_plot <- annotate_figure(f2_arr,
                           top = text_grob("Fig 4. F2: Means and 95% CIs of kinematic measures by information structure", 
                                           color = "black", 
                                           size = 12))
f2_plot

pdf("figures/f2_plot.pdf")
f2_arr
dev.off()

## 2. Place by foot ####
## opening duration ####
# 95% CI
f2gdur_ft_sum <- f2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    GDUR_mean = mean(GDUR),
    GDUR_max = GDUR_mean + 1.96*sd(GDUR)/sqrt(n),
    GDUR_min = GDUR_mean - 1.96*sd(GDUR)/sqrt(n)
  )
f2gdur_ft_sum
# plot
f2gdur_ft_fig <- f2gdur_ft_sum %>%
  ggplot(aes(Foot, GDUR_mean, ymin = GDUR_min, ymax = GDUR_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="a) Opening Duration",
       x="Foot Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2gdur_ft_fig  

## displacement ####
# 95% CI
f2disp_ft_sum <- f2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
  )
f2disp_ft_sum
# plot
f2disp_ft_fig <- f2disp_ft_sum %>%
  ggplot(aes(Foot, DISP_mean, ymin = DISP_min, ymax = DISP_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="b) Displacement",
       x="Foot Structure", 
       y="mm") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2disp_ft_fig 

## peak velocity ####
# 95% CI
f2pv_ft_sum <- f2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
f2pv_ft_sum
# plot
f2pvel_ft_fig <- f2pv_ft_sum %>%
  ggplot(aes(Foot, PV_mean, ymin = PV_min, ymax = PV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="c) Peak Velocity",
       x="Foot Structure", 
       y="mm/s") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2pvel_ft_fig 

## time to peak velocity ####
# 95% CI
f2t2pv_ft_sum <- f2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    TPV_mean = mean(T2PVEL),
    TPV_max = TPV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    TPV_min = TPV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  )
f2t2pv_ft_sum
# plot
f2t2pv_ft_fig <- f2t2pv_ft_sum %>%
  ggplot(aes(Foot, TPV_mean, ymin = TPV_min, ymax = TPV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="d) Time-to-Peak Velocity",
       x="Foot Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
f2t2pv_ft_fig 

## 4 in 1 ####
f2_ft_arr <- ggarrange(f2gdur_ft_fig, 
                       f2disp_ft_fig, 
                       f2pvel_ft_fig, 
                       f2t2pv_ft_fig, 
                       ncol = 2, nrow = 2, 
                       common.legend = T, legend="right")
f2_ft_arr
f2_ft_plot <- annotate_figure(f2_ft_arr,
                              top = text_grob("Fig 8. F2: Means and 95% CIs of kinematic measures by foot structure",
                                              color = "black",
                                              size = 12))
f2_ft_plot

pdf("figures/f2_ft_plot.pdf")
f2_ft_arr
dev.off()

