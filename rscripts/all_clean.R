## LOAD THE PACKAGE ####
library(tidyverse)
library(car)
library(readbulk)

## READ IN DATA ####
files <- list.files(path = "data/", pattern = "*.csv")
data_raw <- lapply(paste("data/", files, sep=""), read_csv) %>% bind_rows()
data_raw

## DATA CLEANING ####
data_clean <- data_raw %>%
  select(-Trial_id) %>%
  mutate( # refactor all the categorical variables
    Footedness = factor(Footedness, levels = c("unfooted", "footed"), labels = c("Unfooted", "Footed")),
    Info_str = factor(Info_str, levels = c("background", "broad", "narrow", "contrastive"), labels = c("Bck", "Brd", "Nrw", "Cntr")),
    Place = factor(Place, levels = c("lower_lip", "tongue_tip", "tongue_body"), labels = c("Lower lip", "Tongue tip", "Tongue body")),
    Subject_id = factor(Subject_id)
  ) %>%
  rename(Speaker = Subject_id)
data_clean

# how many observations being analyzed?
total_tokens <- nrow(data_clean)

# outliers
qqPlot(lm(GDUR ~ Speaker * Info_str * Place * Footedness, data = data_clean))
qqPlot(lm(MDISP ~ Speaker * Info_str * Place * Footedness, data = data_clean))
qqPlot(lm(PVEL ~ Speaker * Info_str * Place * Footedness, data = data_clean))
qqPlot(lm(T2PVEL ~ Speaker * Info_str * Place * Footedness, data = data_clean))

# save all data
write_csv(data_clean, "data/all_speaker_data.csv")

# make separate datasets for each speakers
f1 <- filter(data_clean, Speaker == "F1")
f2 <- filter(data_clean, Speaker == "F2")
m1 <- filter(data_clean, Speaker == "M1")
m2 <- filter(data_clean, Speaker == "M2")
