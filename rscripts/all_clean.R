## LOAD THE PACKAGE ####
library(tidyverse)
library(car)
library(readbulk)

## READ IN DATA ####
data_raw <- read_csv("data/all_speaker_data.csv")
data_raw

## DATA CLEANING ####
data_clean <- data_raw %>%
  mutate( # refactor all the categorical variables
    Foot = factor(Footedness, levels = c("Unfooted", "Footed"), labels = c("Dactyl", "Trochee")),
    Info_str = factor(Info_str),
    Place = factor(Place),
    Speaker = factor(Speaker)
  ) 
data_clean <- data_clean[,c(1,9,3,4,5,6,7,8)]
data_clean

# how many observations being analyzed?
total_tokens <- nrow(data_clean)
total_tokens

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