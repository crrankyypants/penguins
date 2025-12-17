#Load Library
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)

# Load Data
penguins<-read.csv('penguins.csv')
head(penguins)

# Clean Data

penguins_clean <- penguins %>% drop_na(bill_length_mm,bill_depth_mm,flipper_length_mm, body_mass_g, sex) %>% filter(sex %in% c("female", "male"))

# Basic Analysis and Visuals
# means of al lnumerical variables
mean(penguins_clean$bill_length_mm)
mean(penguins_clean$bill_depth_mm)
mean(penguins_clean$flipper_length_mm)
mean(penguins_clean$body_mass_g)

#proportions of cat variables
table(penguins_clean$sex)
table(penguins_clean$species)
table(penguins_clean$island)
table(penguins_clean$species, penguins_clean$island)
table(penguins_clean$species, penguins_clean$sex)

# species, sex compared to body mass

sex_mass <- penguins_clean %>% group_by(species, sex) %>% 
  summarize(mean_mass <- mean(body_mass_g, na.rm=TRUE), n=n(), .groups="drop")

# island vs mass
island_mass <- penguins_clean %>% group_by(island) %>% summarize(mean_mass=mean(body_mass_g, na.rm=TRUE), n=n())

penguins_clean %>% group_by(year, species) %>% summarize(mean_mass=mean(body_mass_g, na.rm=TRUE),n=n())

# how do the penguins species vary by size
penguins_clean %>% group_by(species) %>% summarize(mean_flipper=mean(flipper_length_mm, na.rm=TRUE), n=n())
penguins_clean %>% group_by(species) %>% summarize(mean_flipper=mean(bill_length_mm, na.rm=TRUE), n=n())
penguins_clean %>% group_by(species) %>% summarize(mean_flipper=mean(bill_depth_mm, na.rm=TRUE), n=n())

ggplot(penguins_clean, aes(x=species, y=body_mass_g)) + 
  geom_jitter(col="darkgreen", width=.2, alpha=.6) + 
  labs(x="Species", y="Body Mass", title="Body Mass of Species of Penguins") + theme_classic()
  



