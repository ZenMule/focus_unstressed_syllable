## LOAD THE PACKAGE ####
library(tidyverse)
library(ggpubr)

## 1. Place by Info_str ####
## opening duration ####
# plot
gdur_talk <- data_fig %>%
  ggplot(aes(Place, GDUR)) + 
  geom_boxplot(aes(fill = Info_str)) +
  labs(title="Fig 1. Opening Duration",
       x="Articulators", 
       y="ms") +
  scale_fill_brewer(name = "", labels = c("Background", "Broad", "Narrow", "Contrastive")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) +
  annotate("text", x = 1.5, y = 200, 
           label = "Contrastive - Background = +8.9%\nContrastive - Broad = +8.1%\nAll*", 
           color = "red2",
           size = 4.8)
gdur_talk  
ggsave("figures/gdur_talk.png")

## displacement ####
# plot
disp_talk <- data_fig %>%
  ggplot(aes(Place, MDISP)) + 
  geom_boxplot(aes(fill = Info_str)) +
  coord_cartesian(ylim = c(-2.5, 15)) +
  labs(title="Fig 2. Maximum Displacement",
       x="Articulators", 
       y="mm") +
  scale_fill_brewer(name = "", labels = c("Background", "Broad", "Narrow", "Contrastive")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) + 
  annotate("text", 
           x = 1.5, y = 13, 
           label = "Contrastive - Background: +18.8%\nContrastive - Broad: +16.3%\nAll*", 
           color = "red2",
           size = 4.8)
disp_talk
ggsave("figures/disp_talk.png")

## peak velocity ####
# plot
pvel_talk <- data_fig %>%
  ggplot(aes(Place, PVEL)) + 
  geom_boxplot(aes(fill = Info_str)) +
  labs(title="Fig 3. Peak Velocity",
       x="Articulators", 
       y="mm/s") +
  scale_fill_brewer(name = "", labels = c("Background", "Broad", "Narrow", "Contrastive")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) +
  annotate("text", x = 1.2, y = 180, 
           label = "Contrastive - Background: +12.4%\nContrastive - Broad: +8.4%\nAll*", 
           color = "red2",
           size = 4.8)
pvel_talk   
ggsave("figures/pvel_talk.png")

## time to peak velocity ####
# plot
t2pvel_talk <- data_fig %>%
  ggplot(aes(Place, T2PVEL)) + 
  geom_boxplot(aes(fill = Info_str)) +
  labs(title="Fig 4. Time-to-peak Velocity",
       x="Articulators", 
       y="ms") +
  scale_fill_brewer(name = "", labels = c("Background", "Broad", "Narrow", "Contrastive")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) +
  annotate("text", 
           x = 1.4, y = 140, 
           label = "Contrastive - Background: +9.7%\nContrastive - Broad: +9.3%\nLower lip*, Tongue tip*", 
           color = "red2",
           size = 4.8)
t2pvel_talk 
ggsave("figures/t2pvel_talk.png")

## 2. Place by foot ####
## opening duration ####
# plot
gdur_ft_talk <- data_fig %>%
  ggplot(aes(Place, GDUR)) + 
  geom_boxplot(aes(fill = Foot), width = 0.5) +
  labs(title="Fig 5. Opening Duration",
       x="Articulators", 
       y="ms") +
  scale_fill_discrete(name = "", labels = c("Unfooted", "Footed")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) +
  annotate("text", x = 1.5, y = 200, label = "Footed - Unfooted: +3.9%\nTongue body*", color = "red2", size = 4.8)
gdur_ft_talk 
ggsave("figures/gdur_ft_talk.png")

## displacement ####
# plot
disp_ft_talk <- data_fig %>%
  ggplot(aes(Place, MDISP)) + 
  geom_boxplot(aes(fill = Foot), width = 0.5) +
  labs(title="Fig 6. Maximum Displacement",
       x="Articulators", 
       y="mm") +
  scale_fill_discrete(name = "", labels = c("Unfooted", "Footed")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) +
  annotate("text", x = 1.6, y = 13, label = "Footed - Unfooted: +9.9%\nTongue body*", color = "red2", size = 4.8)
disp_ft_talk  
ggsave("figures/disp_ft_talk.png") 

## peak velocity ####
# plot
pv_ft_talk <- data_fig %>%
  ggplot(aes(Place, PVEL)) + 
  geom_boxplot(aes(fill = Foot), width = 0.5) +
  labs(title="Fig 7. Peak Velocity",
       x="Articulators", 
       y="mm/s") +
  scale_fill_discrete(name = "", labels = c("Unfooted", "Footed")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) + 
  annotate("text", x = 1.3, y = 180, label = "Footed - Unfooted: +7.8%\nLower lip*, Tongue body*", color = "red2", size = 4.8)
pv_ft_talk  
ggsave("figures/pv_ft_talk.png") 

## time to peak velocity ####
# plot
t2pv_ft_talk <- data_fig %>%
  ggplot(aes(Place, T2PVEL)) + 
  geom_boxplot(aes(fill = Foot), width = 0.5) +
  labs(title="Fig 8. Time-to-peak Velocity",
       x="Articulators", 
       y="ms") +
  scale_fill_discrete(name = "", labels = c("Unfooted", "Footed")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = tc) +
  annotate("text", x = 1.5, y = 140, label = "Footed - Unfooted: +5.7%\nTongue Body*", color = "red2", size = 4.8)
t2pv_ft_talk  
ggsave("figures/t2pv_ft_talk.png") 
