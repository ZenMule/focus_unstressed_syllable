## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)

## LOAD OBJECTS ####
m2_fig <- data_clean %>% filter(Speaker == "M2")

## SET POSITION FOR TITLE AND DODGE ####
pd <- position_dodge(width=0.18)
tc <- element_text(hjust = 0.5)

## 1. Place by Info_str ####
## opening duration ####
# 95% CI
m2_gdur_sum <- m2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    GDUR_mean = mean(GDUR),
    GDUR_max = GDUR_mean + 1.96*sd(GDUR)/sqrt(n),
    GDUR_min = GDUR_mean - 1.96*sd(GDUR)/sqrt(n)
  )
m2_gdur_sum
# plot
m2_gdur_fig <- m2_gdur_sum %>%
  ggplot(aes(Info_str, GDUR_mean, ymin = GDUR_min, ymax = GDUR_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="a) Opening Duration",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2_gdur_fig  

pdf("figures/m2_gdur_fig.pdf")
m2_gdur_fig
dev.off()

## displacement ####
# 95% CI
m2_disp_sum <- m2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
  )
m2_disp_sum
# plot
m2_disp_fig <- m2_disp_sum %>%
  ggplot(aes(Info_str, DISP_mean, ymin = DISP_min, ymax = DISP_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="b) Displacement",
       x="Information Structure", 
       y="mm") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2_disp_fig 

pdf("figures/m2_disp_fig.pdf")
m2_disp_fig
dev.off()

## peak velocity ####
# 95% CI
m2_pv_sum <- m2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
m2_pv_sum
# plot
m2_pvel_fig <- m2_pv_sum %>%
  ggplot(aes(Info_str, PV_mean, ymin = PV_min, ymax = PV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="c) Peak Velocity",
       x="Information Structure", 
       y="mm/s") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2_pvel_fig 

pdf("figures/m2_pvel_fig.pdf")
m2_pvel_fig 
dev.off()

## time to peak velocity ####
# 95% CI
m2_t2pv_sum <- m2_fig %>%
  group_by(Place, Info_str) %>%
  summarize(
    n = n(),
    TPV_mean = mean(T2PVEL),
    TPV_max = TPV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    TPV_min = TPV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  )
m2_t2pv_sum
# plot
m2_t2pv_fig <- m2_t2pv_sum %>%
  ggplot(aes(Info_str, TPV_mean, ymin = TPV_min, ymax = TPV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="d) Time-to-Peak Velocity",
       x="Information Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2_t2pv_fig 

pdf("figures/m2_t2pv_fig.pdf")
m2_t2pv_fig
dev.off()

## 4 in 1 ####
m2_arr <- ggarrange(m2_gdur_fig, 
                    m2_disp_fig, 
                    m2_pvel_fig, 
                    m2_t2pv_fig, 
                    ncol = 2, nrow = 2, 
                    common.legend = T, legend="right")
m2_arr
m2_plot <- annotate_figure(m2_arr,
                           top = text_grob("Fig 6. M2: Means and 95% CIs for the kinematic measures by information structure", 
                                           color = "black", 
                                           size = 12))
m2_plot

pdf("figures/m2_plot.pdf")
m2_plot
dev.off()

## 2. Place by foot ####
## opening duration ####
# 95% CI
m2gdur_ft_sum <- m2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    GDUR_mean = mean(GDUR),
    GDUR_max = GDUR_mean + 1.96*sd(GDUR)/sqrt(n),
    GDUR_min = GDUR_mean - 1.96*sd(GDUR)/sqrt(n)
  )
m2gdur_ft_sum
# plot
m2gdur_ft_fig <- m2gdur_ft_sum %>%
  ggplot(aes(Foot, GDUR_mean, ymin = GDUR_min, ymax = GDUR_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="a) Opening Duration",
       x="Foot Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2gdur_ft_fig  

## displacement ####
# 95% CI
m2disp_ft_sum <- m2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    DISP_mean = mean(MDISP),
    DISP_max = DISP_mean + 1.96*sd(MDISP)/sqrt(n),
    DISP_min = DISP_mean - 1.96*sd(MDISP)/sqrt(n)
  )
m2disp_ft_sum
# plot
m2disp_ft_fig <- m2disp_ft_sum %>%
  ggplot(aes(Foot, DISP_mean, ymin = DISP_min, ymax = DISP_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="b) Displacement",
       x="Foot Structure", 
       y="mm") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2disp_ft_fig 

## peak velocity ####
# 95% CI
m2pv_ft_sum <- m2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    PV_mean = mean(PVEL),
    PV_max = PV_mean + 1.96*sd(PVEL)/sqrt(n),
    PV_min = PV_mean - 1.96*sd(PVEL)/sqrt(n)
  )
m2pv_ft_sum
# plot
m2pvel_ft_fig <- m2pv_ft_sum %>%
  ggplot(aes(Foot, PV_mean, ymin = PV_min, ymax = PV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="c) Peak Velocity",
       x="Foot Structure", 
       y="mm/s") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2pvel_ft_fig 

## time to peak velocity ####
# 95% CI
m2t2pv_ft_sum <- m2_fig %>%
  group_by(Place, Foot) %>%
  summarize(
    n = n(),
    TPV_mean = mean(T2PVEL),
    TPV_max = TPV_mean + 1.96*sd(T2PVEL)/sqrt(n),
    TPV_min = TPV_mean - 1.96*sd(T2PVEL)/sqrt(n)
  )
m2t2pv_ft_sum
# plot
m2t2pv_ft_fig <- m2t2pv_ft_sum %>%
  ggplot(aes(Foot, TPV_mean, ymin = TPV_min, ymax = TPV_max, group = Place)) + 
  geom_pointrange(aes(shape = Place), size = 0.5, position = pd) +
  geom_line(aes(linetype = Place), position = pd) +
  labs(title="d) Time-to-Peak Velocity",
       x="Foot Structure", 
       y="ms") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) 
m2t2pv_ft_fig 

## 4 in 1 ####
m2_ft_arr <- ggarrange(m2gdur_ft_fig, 
                       m2disp_ft_fig, 
                       m2pvel_ft_fig, 
                       m2t2pv_ft_fig, 
                       ncol = 2, nrow = 2, 
                       common.legend = T, legend="right")
m2_ft_arr
m2_ft_plot <- annotate_figure(m2_ft_arr,
                              top = text_grob("Fig 10. M2: Means and 95% CIs of kinematic measures by foot structure",
                                              color = "black",
                                              size = 12))
m2_ft_plot

pdf("figures/m2_ft_plot.pdf")
m2_ft_plot
dev.off()


