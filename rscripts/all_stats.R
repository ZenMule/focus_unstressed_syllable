## LOAD PACKAGES ####
library(tidyverse)
library(ez)

## PREPARE THE DATASETS FOR STATS ANALYSIS ####
data_stats <- data_clean
summary(data_stats)

## MAKE SUMMARY ####
data_info_sum <- data_stats %>%
  group_by(Info_str) %>%
  summarize(
    GDUR_mean = mean(GDUR),
    DISP_mean = mean(MDISP),
    PV_mean = mean(PVEL),
    TPV_mean = mean(T2PVEL)
  )
data_pl_sum <- data_stats %>%
  group_by(Place) %>%
  summarize(
    GDUR_mean = mean(GDUR),
    DISP_mean = mean(MDISP),
    PV_mean = mean(PVEL),
    TPV_mean = mean(T2PVEL)
  )
data_ft_sum <- data_stats %>%
  group_by(Footedness) %>%
  summarize(
    GDUR_mean = mean(GDUR),
    DISP_mean = mean(MDISP),
    PV_mean = mean(PVEL),
    TPV_mean = mean(T2PVEL)
  )


## Gesture Duration ####
gdur_aov <- aov(GDUR ~ Info_str * Place * Footedness * Speaker, data = data_stats)
gdur_aov_sum <- summary(gdur_aov)
gdur_aov_sum

# post-hoc test
gdur_aov_tukey <- TukeyHSD(gdur_aov)
gdur_aov_tukey$Info_str
gdur_aov_tukey$Place
gdur_aov_tukey$Footedness

## Displacement ####
disp_aov <- aov(MDISP ~ Info_str * Footedness * Place * Speaker, data = data_stats)
disp_aov_sum <- summary(disp_aov)
disp_aov_sum

# post-hoc test
disp_aov_tukey <- TukeyHSD(disp_aov)
disp_aov_tukey$Info_str
disp_aov_tukey$Footedness
disp_aov_tukey$Place

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

pdf("figures/disp_pl.ft_fig.pdf")
disp_place.ft_fig
dev.off()

## Peak velocity ####
pvel_aov <- aov(PVEL ~ Info_str * Footedness * Place * Speaker, data = data_stats)
pvel_aov_sum <- summary(pvel_aov)
pvel_aov_sum

# post-hoc test
pvel_aov_tukey <- TukeyHSD(pvel_aov)
pvel_aov_tukey$Info_str
pvel_aov_tukey$Footedness
pvel_aov_tukey$Place

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

pdf("figures/pv_pl.ft_fig.pdf")
pv_pl.ft_fig
dev.off()

## T2PVEL ####
t2pvel_aov <- aov(T2PVEL ~ Info_str * Footedness * Place * Speaker, data = data_stats)
t2pvel_aov_sum <- summary(t2pvel_aov)
t2pvel_aov_sum

# post-hoc test
t2pvel_aov_tukey <- TukeyHSD(t2pvel_aov)
t2pvel_aov_tukey$Info_str
t2pvel_aov_tukey$Footedness
t2pvel_aov_tukey$Place

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

pdf("figures/t2pvel_pl.inf_fig.pdf")
t2pvel_pl.inf_fig
dev.off()

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

pdf("figures/t2pvel_ft.pl_fig")
t2pvel_ft.pl_fig
dev.off()
