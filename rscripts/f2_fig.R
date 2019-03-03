## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

## LOAD THE DATA FOR F1 ####
f2_fig <- f2

## SET THE COLORS ####
colors_place <- brewer.pal(5, "Set1")
color_lp <- colors_place[2]
color_alv <- colors_place[3]
color_vl <- colors_place[5]

## GDUR ####
f2_gdur_fig <- f2_fig %>%
  ggplot(aes(Info_str, GDUR)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Gesture Duration (ms)") +
  scale_fill_manual(values = c(color_lp, color_alv, color_vl),
                    name = "Places",
                    breaks = c("lower_lip", "tongue_tip", "tongue_body"),
                    labels = c("Lower Lip", "Tongue Tip", "Tongue Body")) +
  facet_wrap(~ Footedness) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## MDISP ####
f2_mdisp_fig <- f2_fig %>%
  ggplot(aes(Info_str, MDISP)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Displacement (mm)") +
  scale_fill_manual(values = c(color_lp, color_alv, color_vl),
                    name = "Places",
                    breaks = c("lower_lip", "tongue_tip", "tongue_body"),
                    labels = c("Lower Lip", "Tongue Tip", "Tongue Body")) +
  facet_wrap(~ Footedness) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## PVEL ####
f2_pvel_fig <- f2_fig %>%
  ggplot(aes(Info_str, PVEL)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Peak Velocity (mm/s)") +
  scale_fill_manual(values = c(color_lp, color_alv, color_vl),
                    name = "Places",
                    breaks = c("lower_lip", "tongue_tip", "tongue_body"),
                    labels = c("Lower Lip", "Tongue Tip", "Tongue Body")) +
  facet_wrap(~ Footedness) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## T2PVEL ####
f2_t2pvel_fig <- f2_fig %>%
  ggplot(aes(Info_str, T2PVEL)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Time-to-Peak Velocity (ms)") +
  scale_fill_manual(values = c(color_lp, color_alv, color_vl),
                    name = "Places",
                    breaks = c("lower_lip", "tongue_tip", "tongue_body"),
                    labels = c("Lower Lip", "Tongue Tip", "Tongue Body")) +
  facet_wrap(~ Footedness) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## 4 in 1 ###
f2_plot <- ggarrange(f2_gdur_fig, 
                     f2_mdisp_fig, 
                     f2_pvel_fig, 
                     f2_t2pvel_fig, 
                     ncol = 2, nrow = 2, 
                     common.legend = T,
                     legend = "bottom")
f2_plot <- annotate_figure(f2_plot,
                           top = text_grob("F2 Data Summary", 
                                           color = "black", 
                                           face = "bold", 
                                           size = 14))

pdf("figures/f2_plot.pdf")
f2_plot
dev.off()
