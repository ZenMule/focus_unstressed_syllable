## LOAD THE PACKAGE ####
library(tidyverse)

## READ IN DATA ####
f1 <- read_csv("data/f1_data.csv")
f2 <- read_csv("data/f2_data.csv")
all <- rbind(f1, f2)

# clean the data
all <- all %>% mutate( # refactor all the categorical variables
  Footedness = factor(Footedness, levels = c("unfooted", "footed")),
  Info_str = factor(Info_str, levels = c("background", "broad", "narrow", "contrastive"), labels = c("bck", "brd", "nrw", "cntr")),
  Place = factor(Place, levels = c("lower_lip", "tongue_tip", "tongue_body")),
  Subject_id = factor(Subject_id),
  Trial_num = factor(Trial_num)
) %>%
  select(-Trial_num) 
all <- all %>%
  rename(Speaker = Subject_id)


# create separate data sets for Footedness conditions
all_unf <- filter(all, Footedness == "unfooted")
all_f <- filter(all, Footedness == "footed")

# save all data
write_csv(all, "data/all_data.csv")

# make separate datasets for each speakers
f1 <- filter(all, Speaker == "F1")
f2 <- filter(all, Speaker == "F2")
