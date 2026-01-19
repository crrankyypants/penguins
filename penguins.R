#Load Library
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(nnet)
library(class)

# Load Data
penguins<-read.csv('penguins.csv')
head(penguins)

# Clean Data

penguins_clean <- penguins %>% drop_na(bill_length_mm,bill_depth_mm,flipper_length_mm, body_mass_g, sex) %>% filter(sex %in% c("female", "male"))

# Adelie species only - noticed they are the only species spread across all 3 islands
adelie_df <- penguins_clean %>% filter(species == "Adelie")

# widen the data set to separate the 3 different islands for ML and modeling
penguins_wider <- penguins_clean %>% mutate(value=1) %>%  pivot_wider(names_from=island, values_from=value, values_fill=0, names_prefix="island_")



##### Statistics
# means of all numerical variables
mean(penguins_clean$bill_length_mm)
mean(penguins_clean$bill_depth_mm)
mean(penguins_clean$flipper_length_mm)
mean(penguins_clean$body_mass_g)

#proportions of cat variables
table(penguins_clean$sex)
table(penguins_clean$species)
table(penguins_clean$island)
table(penguins_clean$species, penguins_clean$island) # Does which island the Adelie species resides in affect their size?
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


### Visuals
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
ggplot(penguins_clean, aes(x=body_mass_g, y=flipper_length_mm, color=species)) + geom_point(size=1, alpha =.5) + 
  scale_color_manual(values=c(Adelie="skyblue", Chinstrap="purple", Gentoo="orange")) +
  facet_wrap(~ sex) + theme_minimal()

# THIS IS BETTER
ggplot(penguins_clean, aes(x=body_mass_g, y=flipper_length_mm, color=sex)) + geom_boxplot(size=1, alpha =.5) + 
  scale_color_manual(values=c(male="skyblue", female="purple")) +
  facet_wrap(~ species) + theme_dark()
# numerical vs numerical


# distribution of bodymass clearly shows against each island
ggplot(adelie_df, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  facet_wrap(~ island) +
  theme_dark()

# boxplot gives better understanding of how the distribution presents mathematically
ggplot(adelie_df, aes(x=island, y=body_mass_g)) + geom_boxplot(fill="skyblue")  + theme_dark()


# distribution of overall bodymass
ggplot(adelie_df, aes(x=body_mass_g)) + geom_histogram(binwidth=100, fill="skyblue", color="black") + theme_dark()
  #actual stats
# by island stat
adelie_df %>% group_by(island) %>% summarize(n =n(), mean_body_mass = mean(body_mass_g),
                                             mean_flipper_length = mean(flipper_length_mm),
                                             sd_body_mass = sd(body_mass_g),
                                             iqr_body_mass = IQR(body_mass_g))
# overall stat
adelie_df %>% summarize(n =n(), mean_body_mass = mean(body_mass_g), 
                        mean_flipper_length = mean(flipper_length_mm),
                        sd_body_mass = sd(body_mass_g),
                        iqr_body_mass = IQR(body_mass_g))

### some quesitons about the data

# Predict species based on body measurements
# what are the major differences between thse species of penguins, which features are most prominent in certain penguins
# How do penguins vary in size by the island
# can we predict the size of the adelie species based on the island - this is sort of bad because of biological 
# how do adielie penguins differ across islands


#### TESTING
# how do adielie penguins differ across islands
# Hypothesis testing
  # H0: Adelie bodymass mean is the same across all islands : mean(island1)=mean(island2)=mean(island3)
  # H1: The Adelie bodymass mean differs on at least 1 island: 

# permutation 
#observed test stat
obs_stat <- adelie_df %>% group_by(island) %>% summarize(mean_mass = mean(body_mass_g)) %>% pull(mean_mass) %>% var()
# permuted test stat
perm_df <- replicate(5000, {
  sample_bm <- sample(adelie_df$body_mass_g)
  data.frame(island = as.character(adelie_df$island), body_mass_g = as.numeric(sample_bm))
}, simplify=FALSE)

perm_var <- sapply(perm_df, function(x) {
    x %>% 
    group_by(island) %>%
    summarize(mean_mass = mean(body_mass_g)) %>%
    pull(mean_mass) %>%
    var()
})

# verifying the variances were correct
perm_var1 <- c()

for(i in 1:ncol(perm_means)) {
  x1 <- perm_means[1,i]
  x2 <- perm_means[2,i]
  x3 <- perm_means[3,i]
  mean_i <- (x1 + x2 + x3) / 3
  
  var_i <- ((x1 - mean_i)^2 + (x2 - mean_i)^2 + (x3 - mean_i)^2) / (3 - 1)
  perm_var1[i] <- var_i
}


p_value <- mean(perm_var >= obs_stat)
# Based on this permutations test, the observed variance in mean body mass across the 3 islands was not unusual under the null hypothesis. 
# island location has no significant effect on the bodymass of the Adelie species . the Pvalue was .98 indicating the observed differences in mean bodymass can occur due the random variation alone. 


# using anova to check our permutations pvalue answer
anova_model <- aov(body_mass_g ~ island, data = adelie_df)
summary(anova_model) # pvalue is .995 and our permutations pvalue is .9958 very similar meaning our test was accurate. 

### Modeling and ML

model_logistic <- multinom(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = penguins_clean)

# train/test split
set.seed(123)

train_idx <- sample(nrow(penguins_clean), 0.7 * nrow(penguins_clean))
train <- penguins_clean[train_idx,]
test <- penguins_clean[-train_idx,]

pred <- predict(model_logistic, test)
mean(pred==test$species) # classification accuracy

table(Predicted = pred, Actual = test$species)

model_full <- multinom( species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g + sex + island, data = test)
pred1 <- predict(model_full, test)
mean(pred1==test$species)


# scale
X_train <- scale(train[, c("bill_length_mm", "bill_depth_mm",
                           "flipper_length_mm", "body_mass_g")])
X_test  <- scale(test[, c("bill_length_mm", "bill_depth_mm",
                          "flipper_length_mm", "body_mass_g")])
knn_pred <- knn( train = X_train, test = X_test, cl = train$species, k = 5)
mean(knn_pred == test$species)

