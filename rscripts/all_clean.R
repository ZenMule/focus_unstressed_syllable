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
    Foot = factor(Foot, levels = c("Dactyl", "Trochee")),
    Info_str = factor(Info_str, levels=c("Bck", "Brd", "Nrw", "Cntr")),
    Place = factor(Place, levels=c("Lower lip", "Tongue tip", "Tongue body")),
    Speaker = factor(Speaker)
  ) 
data_clean

# save all data
write_csv(data_clean, "data/all_speaker_data.csv")

# make separate datasets for each speakers
f1 <- filter(data_clean, Speaker == "F1")
f2 <- filter(data_clean, Speaker == "F2")
m1 <- filter(data_clean, Speaker == "M1")
m2 <- filter(data_clean, Speaker == "M2")