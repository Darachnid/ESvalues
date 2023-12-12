###############################################################
##### Single Parameter Demographic Visualizations #############
###############################################################

# Libraries
library(tidyverse)

# Set the plot theme
theme_set(theme_minimal())

# Import Data
data <- readRDS("out/clean_data.rds")

###################
### Demographics
###################

### Gender
data |>
  filter(!is.na(gender)) |>
  group_by(gender) |>
  summarise("count" = n()) |>
  ggplot(aes(x = gender, y = count)) +
  geom_bar(stat = "identity")

# Save the plot as a png
ggsave(filename = "out/demographics/barplot__gender.png", 
       device = "png", 
       bg = "white")

######################## Birth Year ########################
birth_year_data <- data |>
  filter(!is.na(birth_year) & birth_year != "Prefer not to answer") |>
  select(birth_year) |>
  mutate(birth_year = as.numeric(birth_year)) 

# calculate the bins using Square Root Choice
bins <- round(sqrt(length(birth_year_data$birth_year)))

ggplot(birth_year_data, aes(x = birth_year)) +
  geom_histogram(bins = bins) +
  geom_vline(xintercept = median(birth_year_data$birth_year, 
                                 na.rm = TRUE),
             color = "grey65",
             size = 1) +
  labs(title = "Birth Year",
       x = "birth year")

# Save the plot as a png
ggsave(filename = "out/demographics/hist__birth_year.png", 
       device = "png", 
       bg = "white")

rm(birth_year_data, bins)

######################## Race ########################
race_data <- data |>
  filter(!is.na(race)) |>
  group_by(race) |>
  summarise("count" = n()) |>
  arrange(desc(count))

race_levels <- race_data$race

ggplot(race_data, aes(x = factor(race, levels = race_levels),
           y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "race")

# Save the plot as a png
ggsave(filename = "out/demographics/barplot__race.png", 
       device = "png", 
       bg = "white")

rm(race_data, race_levels)

################ Years in Community ################
years_data <- data |>
  filter(!is.na(year_in_community)) |>
  select(year_in_community) 

# calculate the bins using Square Root Choice
bins <- round(sqrt(length(years_data$year_in_community)))

median <- median(years_data$year_in_community)

ggplot(years_data, aes(x = year_in_community)) +
  geom_density(fill = "grey30") +
  geom_vline(xintercept = median,
             color = "grey65",
             size = 1) +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  labs(title = "Years lived in Present Community",
       x = "years")

# Save the plot as a png
ggsave(filename = "out/demographics/density__years_in_community.png", 
       device = "png", 
       bg = "white")

rm(years_data, bins, median)

################ Political Views ################
data |>
  filter(!is.na(politics)) |>
  group_by(politics) |>
  summarise("count" = n()) |>
  ggplot(aes(x = politics,
             y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Political Spectrum",
       x = "political view")

ggsave(filename = "out/demographics/barplot__political_spectrum.png",
       device = "png",
       bg = "white")

### Adults in Home
adult_data <- data |>
  filter(!is.na(adults_in_household)) 

median <- median(adult_data$adults_in_household)

ggplot(adult_data, aes(x = adults_in_household)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = median,
             color = "grey65",
             size = 1) +
  labs(title = "Adults in Household",
       x = "number of adults in the household")

ggsave(filename = "out/demographics/hist__adults_in_household.png",
       device = "png",
       bg = "white")

rm(adult_data, median)

### Children in Home
child_data <- data |>
  filter(!is.na(children_in_household)) 

median <- mean(child_data$children_in_household)

ggplot(child_data, aes(x = children_in_household)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = median,
             color = "grey65",
             size = 1) +
  labs(title = "Children in Household",
       x = "number of children in the household") +
  scale_x_continuous(breaks = seq(0, 5, 1))

ggsave(filename = "out/demographics/hist__children_in_household.png",
       device = "png",
       bg = "white")

rm(adult_data, median)

### Fraction of household members that are children
child_data <- data |>
  filter(!is.na(frac_children)) 

mean <- mean(child_data$frac_children)

ggplot(child_data, aes(x = frac_children)) +
  geom_histogram(binwidth = 0.25) +
  geom_vline(xintercept = mean,
             color = "grey65",
             size = 1) +
  labs(title = "Fraction of Household Members Children",
       x = "proportion of household members that are children") 

ggsave(filename = "out/demographics/hist__frac_children_in_household.png",
       device = "png",
       bg = "white")

rm(child_data, mean)

### Income
data |>
  filter(!is.na(income)) |>
  group_by(income) |>
  summarise("count" = n()) |>
  ggplot(aes(x = income,
                      y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "income bracket",
       title = "Income")

# Save the plot as a png
ggsave(filename = "out/demographics/barplot__income.png", 
       device = "png", 
       bg = "white")

### Education
data |>
  filter(!is.na(education)) |>
  group_by(education) |>
  summarise("count" = n()) |>
  ggplot(aes(x = education,
             y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "education level",
       title = "Education Level")

# Save the plot as a png
ggsave(filename = "out/demographics/barplot__education.png", 
       device = "png", 
       bg = "white")







