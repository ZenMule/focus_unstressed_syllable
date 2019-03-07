## LOAD PACKAGES ####
library(tidyverse)
library(car)
library(lme4)
library(ez)
library(effsize)

## PREPARE THE DATASETS FOR STATS ANALYSIS ####
data_stats <- data_clean
summary(data_stats)
# separate the data by speaker
f1_stats <- data_stats %>% filter(Speaker == "F1")
f2_stats <- data_stats %>% filter(Speaker == "F2")
m1_stats <- data_stats %>% filter(Speaker == "M1")
m2_stats <- data_stats %>% filter(Speaker == "M2")

# further separate the data by poa for each speaker
f1_ll <- f1_stats %>% filter(Place == "Lower lip")
f1_tt <- f1_stats %>% filter(Place == "Tongue tip")
f1_tb <- f1_stats %>% filter(Place == "Tongue body")

f2_ll <- f2_stats %>% filter(Place == "Lower lip")
f2_tt <- f2_stats %>% filter(Place == "Tongue tip")
f2_tb <- f2_stats %>% filter(Place == "Tongue body")

m2_ll <- m2_stats %>% filter(Place == "Lower lip")
m2_tt <- m2_stats %>% filter(Place == "Tongue tip")
m2_tb <- m2_stats %>% filter(Place == "Tongue body")

m1_ll <- m1_stats %>% filter(Place == "Lower lip")
m1_tt <- m1_stats %>% filter(Place == "Tongue tip")
m1_tb <- m1_stats %>% filter(Place == "Tongue body")

p_adj <- .05/(3*4) #0.004
p_adj
## Duration ####
# F1
f1_ll_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = f1_ll))
f1_tt_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = f1_tt)) # Info*, Ft+
TukeyHSD(aov(GDUR ~ Info_str * Footedness, data = f1_tt))             # cntr-bck, cntr-brd, cntr-nrw, ft < unf
f1_tb_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = f1_tb))

# F2
f2_ll_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = f2_ll))
f2_tt_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = f2_tt)) # Ft*
TukeyHSD(aov(GDUR ~ Info_str * Footedness, data = f2_tt))             # ft > unf
f2_tb_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = f2_tb))

# M1
m1_ll_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = m1_ll))
m1_tt_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = m1_tt)) # Ft.
TukeyHSD(aov(GDUR ~ Info_str * Footedness, data = m1_tt))             # ft < unf
m1_tb_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = m1_tb)) # Info+
TukeyHSD(aov(GDUR ~ Info_str * Footedness, data = m1_tb))             # brd-cntr, brd-bck

# M2
m2_ll_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = m2_ll)) 
m2_tt_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = m2_tt)) # Info+
TukeyHSD(aov(GDUR ~ Info_str * Footedness, data = m2_tt))$Info_str
m2_tb_dur <- summary(aov(GDUR ~ Info_str * Footedness, data = m2_tb)) 

## Displacement ####
# F1
f1_ll_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = f1_ll)) # Ft*
TukeyHSD(aov(MDISP ~ Info_str * Footedness, data = f1_ll))              # ft > unf
f1_tt_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = f1_tt)) # Info*
TukeyHSD(aov(MDISP ~ Info_str * Footedness, data = f1_tt))              # cntr-bck, cntr-brd, cntr-nrw
f1_tb_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = f1_tb))

# F2
f2_ll_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = f2_ll)) # Info* Ft#
TukeyHSD(aov(MDISP ~ Info_str * Footedness, data = f2_ll))              # nrw-bck, cntr-bck, ft > unf
f2_tt_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = f2_tt)) # Info+
TukeyHSD(aov(MDISP ~ Info_str * Footedness, data = f2_tt))              # cntr-brd
f2_tb_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = f2_tb))

# M1
m1_ll_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = m1_ll))
m1_tt_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = m1_tt))
m1_tb_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = m1_tb)) # Info*, Ft*
TukeyHSD(aov(MDISP ~ Info_str * Footedness, data = m1_tb))              # brd-cntr; brd-nrw; brd-bck; ft-unf 1.74

# M2
m2_ll_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = m2_ll))
m2_tt_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = m2_tt))
m2_tb_disp <- summary(aov(MDISP ~ Info_str * Footedness, data = m2_tb)) 

## Peak velocity ####
# F1
f1_ll_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = f1_ll)) # Ft+
f1_tt_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = f1_tt)) 
f1_tb_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = f1_tb)) # Ft+

# F2
f2_ll_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = f2_ll)) # Info_str+, Ft+
TukeyHSD(aov(PVEL ~ Info_str * Footedness, data = f2_ll))            # cntr-bck, ft > unf
f2_tt_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = f2_tt)) # Info_str+
TukeyHSD(aov(PVEL ~ Info_str * Footedness, data = f2_tt))$Info_str   # cntr-brd
f2_tb_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = f2_tb)) # Ft+
TukeyHSD(aov(PVEL ~ Info_str * Footedness, data = f2_tb))$Footedness # ft>unf

# M1 
m1_ll_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = m1_ll))
m1_tt_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = m1_tt)) # Info_str+
TukeyHSD(aov(PVEL ~ Info_str * Footedness, data = m1_tt))$Info_str   # nrw-bck
m1_tb_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = m1_tb)) 

# M2
m2_ll_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = m2_ll)) # Ft+
TukeyHSD(aov(PVEL ~ Info_str * Footedness, data = m2_ll))$Footedness # ft < unf
m2_tt_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = m2_tt))
m2_tb_pv <- summary(aov(PVEL ~ Info_str * Footedness, data = m2_tb))

## Time-to-peak velocity ####
# F1
f1_ll_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = f1_ll))
f1_tt_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = f1_tt)) # Info*
TukeyHSD(aov(T2PVEL ~ Info_str * Footedness, data = f1_tt))             # cntr-bck; cntr-brd; cntr-nrw
f1_tb_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = f1_tb))

# F2
f2_ll_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = f2_ll))
f2_tt_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = f2_tt)) # Ft*
TukeyHSD(aov(T2PVEL ~ Info_str * Footedness, data = f2_tt))             # ft > unf
f2_tb_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = f2_tb))

# M1
m1_ll_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = m1_ll))
m1_tt_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = m1_tt)) # Ft*
TukeyHSD(aov(T2PVEL ~ Info_str * Footedness, data = m1_tt))             # ft < unf
m1_tb_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = m1_tb))

# M2
m2_ll_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = m2_ll)) # Ft*
TukeyHSD(aov(T2PVEL ~ Info_str * Footedness, data = m2_ll))             # ft > unf
m2_tt_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = m2_tt)) 
m2_tb_tpv <- summary(aov(T2PVEL ~ Info_str * Footedness, data = m2_tb)) # Ft+
TukeyHSD(aov(T2PVEL ~ Info_str * Footedness, data = m2_tb))$Footedness  # ft > unf       
