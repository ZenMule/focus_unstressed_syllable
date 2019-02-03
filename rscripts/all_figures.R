## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

## LOAD OBJECTS ####
all_f_fig <- all_f
all_unf_fig <- all_unf
all_fig <- all

## SET COLORS ####
colors_place <- brewer.pal(3, "Blues")
color_lp <- colors_place[1]
color_alv <- colors_place[2]
color_vl <- colors_place[3]

## FIGURES ####
# plot gesture duration
all_gdur_fig <- all_fig %>%
  ggplot(aes(Info_str, GDUR)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Gesture Duration (ms)") +
  scale_fill_manual(values=c(color_lp, color_alv, color_vl)) +
  facet_wrap(~ Footedness)

# plot peak velocity
all_pvel_fig <- all_fig %>%
  ggplot(aes(Info_str, PVEL)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Peak Velocity (mm/s)") +
  scale_fill_manual(values=c(color_lp, color_alv, color_vl)) +
  facet_wrap(~ Footedness)

# plot time to peak velocity
all_t2pvel_fig <- all_fig %>%
  ggplot(aes(Info_str, T2PVEL)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Time-to-Peak Velocity (ms)") +
  scale_fill_manual(values=c(color_lp, color_alv, color_vl)) +
  facet_wrap(~ Footedness) 

# plot maximum displacement
all_mdisp_fig <- all_fig %>%
  ggplot(aes(Info_str, MDISP)) + 
  geom_boxplot(aes(fill = Place)) +
  labs(x="Information Structure", 
       y="Maximum Displacement (mm)") +
  scale_fill_manual(values=c(color_lp, color_alv, color_vl)) +
  facet_wrap(~ Footedness)

# lower lip 4 in 1
all_plot <- ggarrange(all_gdur_fig, 
                        all_mdisp_fig, 
                        all_pvel_fig, 
                        all_t2pvel_fig, 
                        ncol = 2, nrow = 2, 
                        common.legend = T, legend="bottom")
all_plot <- annotate_figure(all_plot,
                            top = text_grob("Overall Data Summary", 
                                            color = "black", 
                                            face = "bold", 
                                            size = 14))

pdf("figures/all_plot.pdf")
all_plot
dev.off()
