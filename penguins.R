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

# function for comparing x and y
num_cat <- function(x_value,y_value,x_name,y_name,title_, col_var, dataset) {
  ggplot(dataset, aes(x={{x_value}}, y={{y_value}}, color= {{col_var}})) + 
    geom_boxplot(width=.2, alpha=.3) + 
    scale_color_manual(values=c(Adelie="skyblue", Chinstrap="purple", Gentoo="orange")) +
    labs(x=x_name, y=y_name, title=title_) + theme_classic()
}
#species v numerical
num_cat(species,bill_length_mm, x_name="Species", y_name="Bill Length (mm)", title_="Species by Bill Length", col_var=species, dataset=penguins_clean)
num_cat(species, body_mass_g, x_name="Species", y_name="Body Mass (grams)", title_="Species by Body Mass", col_var=species,dataset=penguins_clean)
num_cat(species, flipper_length_mm, x_name="Species", y_name="Flipper Length (mm)", title_="Species by Flipper Length", col_var=species, dataset=penguins_clean)
num_cat(species,bill_depth_mm, x_name="Species", y_name="Bill Depth (mm)", title_="Species by Bill Depth", col_var=species, dataset=penguins_clean)

# compare sex vs size cs species or island vs size vs species 2 cat vs 1 num
# shows specific differences between variables
penguins_long<- penguins_clean|> pivot_longer(cols=c(bill_length_mm,bill_depth_mm), names_to="measure", values_to="value")
ggplot(penguins_long, aes(x=species, y=value, fill=species))+ geom_boxplot() + facet_wrap(~ measure, scales="free_y")+ theme_classic()

# displays more correlation between all three variables
ggplot(penguins_clean, aes(x=bill_length_mm, y=bill_depth_mm, color= species)) + geom_point() + geom_smooth(method="lm", se = FALSE) + 
  theme_classic()

ggplot(penguins_clean, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) + geom_point(size=1.05, alpha=.6) + 
  scale_color_manual(values=c(Adelie="skyblue", Chinstrap="purple", Gentoo="orange")) + geom_smooth(method="lm", se = FALSE, color="red", alpha=.5) + 
  facet_wrap(~ species)

# compare sex vs body mass vs flipper length, or species vs bodymass vs sflipper length etc 1 cat vs 2 num

# numerical vs numerical


