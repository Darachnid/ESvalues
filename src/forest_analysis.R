#################################################################
#+ This script makes models that predict for ES values related
#+ to the forest ecosystem in southern NH.
#################################################################

library(tidyverse)
library(performance)
library(ggthemes)
library(GGally)

theme_set(theme_minimal())

data <- read_rds("out/clean_data.rds") |>
  select(birth_year, year_in_community, politics,
         env_quality, `7d`, `7j`, `7l`, gender,
         education, income, val_forest_wood, val_forest_biodiversity, 
         val_forest_climate_protection, val_fall_foliage) |>
  rename("Fall Foliage" = val_fall_foliage,
         "Climate Protection" = val_forest_climate_protection,
         "Wood" = val_forest_wood,
         "Biodiversity" = val_forest_biodiversity,
         "disregard_of_future_generations"= `7d`,
         "consideration_of_future_generations"  = `7j`,
         "obligation_to_future_generations"  = `7l`) |>
  pivot_longer(cols = c("Fall Foliage",
                        "Climate Protection",
                        "Wood",
                        "Biodiversity"), 
               names_to = "ES",
               values_to = "importance") |>
  mutate("age" = 2015 - as.numeric(birth_year)) |>
  select(-birth_year) |>
  mutate(politics = as.numeric(politics),
         education = as.numeric(education),
         income = as.numeric(income)) 

#################################################################
########################## Initial Viz ########################## 
#################################################################

### Plot: ES importace by years_in_community
plot_es_years <- data |> 
  ggplot(aes(x = year_in_community,
             y = importance,
             color = gender)) +
  geom_jitter(height = 0.3,
              width = 0.3) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_colorblind() 
plot_es_years + facet_wrap(~ES) 

### Plot: ES importace by age
plot_es_age <- data |> 
  ggplot(aes(x = age, 
             y = importance, 
             color = gender)) + 
  geom_jitter() + 
  stat_smooth(method = "lm", se = FALSE) + 
  scale_color_colorblind() 
plot_es_age + facet_wrap(~ES) 

### Plot: ES importace by env_quality
plot_es_env <- data |>
  ggplot(aes(x = env_quality,
             y = importance,
             color = gender)) +
  geom_jitter(height = 0.3,
              width = 0.3) +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_color_colorblind()
plot_es_env + facet_wrap(~ES)

### Plot: ES importace by income
plot_es_income <- data |>
  select(income, importance, ES, gender) |>
  na.omit() |>
  ggplot(aes(x = income,
             y = importance,
             color = gender)) +
  geom_jitter(height = 0.3,
              width = 0.3) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_colorblind()
plot_es_income + facet_wrap(~ES)

### Plot: ES importace by education
plot_es_edu <- data |>
  select(education, importance, ES, gender) |>
  na.omit() |>
  ggplot(aes(x = education,
             color = gender,
             y = importance)) +
  geom_jitter(height = 0.3,
              width = 0.3) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_colorblind()
plot_es_edu + facet_wrap(~ES)

### Plot: ES importace by politics
plot_es_politics <- data |>
  select(politics, importance, ES, gender) |>
  na.omit() |>
  filter(politics != "Prefer Not to Answer") |>
  ggplot(aes(x = politics,
             color = factor(gender),
             y = importance)) +
  geom_jitter(height = 0.3,
              width = 0.3) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_colorblind()
plot_es_politics + facet_wrap(~ES)

### Plot: ES importance by age, gender
plot_es_age <- data |>
  select(importance, age, gender, ES) |>
  filter(gender != "Prefer Not to Answer") |>
  na.omit() |>
  ggplot(aes(x = age,
             y = importance,
             color = gender)) +
  geom_jitter(height = 0.45,
              width = 0.35)  +
  scale_color_colorblind() +
  stat_smooth(method = "lm", 
              se = FALSE)
plot_es_age + facet_wrap(~ES)


### Plot: ES importance by years_in_community, gender
plot_es_years <- data |>
  select(importance, year_in_community, gender, ES) |>
  filter(gender != "Prefer Not to Answer") |>
  na.omit() |>
  ggplot(aes(x = year_in_community,
             group = gender,
             y = importance,
             color = gender)) +
  geom_jitter(height = 0.45,
              width = 0.35)  +
  scale_color_colorblind() +
  stat_smooth(method = "lm", 
              se = FALSE)
plot_es_years + facet_wrap(~ES)

### Plot: ES importance by consideration_of_future_generations
plot_es_considers <- data |>
  select(consideration_of_future_generations, importance,
         ES) |>
  na.omit() |>
  ggplot(aes(x = consideration_of_future_generations,
             y = importance,
             group = consideration_of_future_generations)) +
  geom_violin(fill = "orange") +
  geom_jitter(height = 0.5,
              width = 0.5) +
  geom_violin(alpha = 0.1) +
  stat_summary(fun = mean, geom = "line", 
               color = "blue", size = 0.8,
               aes(group = 1)) 
plot_es_considers + facet_wrap(~ES)

### Plot: ES importance by disregard_of_future_generations
plot_es_disregard <- data |>
  select(disregard_of_future_generations, importance,
         ES) |>
  na.omit() |>
  ggplot(aes(x = disregard_of_future_generations,
             y = importance,
             group = disregard_of_future_generations)) +
  geom_violin(fill = "orange") +
  geom_jitter(height = 0.5,
              width = 0.5) +
  geom_violin(alpha = 0.1) +
  stat_summary(fun = mean, geom = "line", 
               color = "blue", size = 0.8,
               aes(group = 1)) 
plot_es_disregard + facet_wrap(~ES)

## Weird that they are both mode of -2 and they dont oppose
## I think its bc this question "8j" is a bit confusing w a double negative

plot_consider_disregard <- data |>
  ggplot(aes(x = disregard_of_future_generations,
             y = consideration_of_future_generations)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)
plot_consider_disregard

plot_disregard_obligation <- data |>
  ggplot(aes(x = disregard_of_future_generations,
             y = obligation_to_future_generations)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)
plot_disregard_obligation 

## It seems obligation does oppose disregard so we should
## use that instead of consideration 
plot_consider_obligation <- data |>
  ggplot(aes(x = consideration_of_future_generations,
             y = obligation_to_future_generations)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)
plot_consider_obligation 


### Plot: ES importance by obligation_to_future_generations
plot_es_obligation <- data |>
  select(obligation_to_future_generations, importance,
         ES, gender) |>
  na.omit() |>
  ggplot(aes(x = obligation_to_future_generations,
             y = importance,
             group = obligation_to_future_generations,
             color = gender)) +
  geom_jitter(height = 0.5,
              width = 0.5) +
  xlim(0, 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(group = gender)) +
  scale_color_colorblind()
plot_es_obligation + facet_wrap(~ES)





