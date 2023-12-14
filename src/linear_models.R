library(tidyverse)
library(performance)
library(PerformanceAnalytics)
library(broom)

theme_set(theme_minimal())

data <- readRDS("out/clean_data.rds") |>
  mutate(birth_year = as.numeric(birth_year)) 

names(data)
str(data)

numeric_data_1to4 <- data[,c(2:3, 4:7, 32:33, 35:36)]

chart.Correlation(numeric_data_1to4)

lm_data <- data |>
  select(birth_year,
         year_in_community,
         ESmean, income, 
         politics, education) |>
  na.omit()

lm_year <- lm(ESmean ~ year_in_community, data = lm_data)
lm_birth <- lm(ESmean ~ birth_year, data = lm_data)
lm_year_birth <- lm(ESmean ~ birth_year + year_in_community, data = lm_data)
lm_birth_year <- lm(ESmean ~ birth_year * year_in_community, data = lm_data)


### Do the assumptions hold true?
check_model(lm_year)
check_model(lm_birth)
check_model(lm_year_birth)
check_model(lm_birth_year)

performance::compare_performance(lm_year, lm_birth, lm_year_birth, lm_birth_year, rank = TRUE)

lm_year 

ggplot(lm_data, aes(x = year_in_community,
                    y = ESmean)) +
  geom_jitter() +
  stat_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~income)

ggplot(lm_data, aes(x = year_in_community,
                    y = ESmean)) +
  geom_jitter() +
  stat_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~education)

### More Models
lm_year_edu <- lm(ESmean ~ year_in_community + education, data = lm_data)

performance::compare_performance(lm_year_edu,
                                 lm_year, lm_birth, lm_year_birth, lm_birth_year, rank = TRUE)
