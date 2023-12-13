library(tidyverse)
library(tigris)
library(zipcodeR)
library(sf)

setwd(here::here())

data <- read_csv("data/Data.csv") |>
  filter(!is.na(year_in_community)) |>
  mutate(zipcode = paste0("0", as.character(zipcode))) |>
  mutate(search_county(state_abb = NH))
  mutate(across(starts_with("val"), ~case_when(
    . == "Unimportant" ~ 1,
    . == "Slight Importance" ~ 2,
    . == "Moderately Important" ~ 3,
    . == "Important" ~ 4,
    . == "Very Important" ~ 5,
    TRUE ~ NA
  ))) |>
  mutate(ESmean = rowMeans(across(starts_with("val")))) |>
  mutate(across(starts_with("7"), ~case_when(
    . == "Strongly Disagree" ~ -2,
    . == "Somewhat Disagree" ~ -1,
    . == "Neither Disagree nor Agree" ~ 0,
    . == "Somewhat Agree" ~ 1,
    . == "Strongly Agree" ~ 2,
    TRUE ~ NA
  ))) |>
  mutate(env_quality = case_when(
    env_quality == "Very Poor" ~ -2,
    env_quality == "Poor" ~ -1,
    env_quality == "Fair" ~ 0,
    env_quality == "Good" ~ 1,
    env_quality == "Very Good" ~ 2,
    TRUE ~ NA
  )) |>
  mutate(politics = factor(politics, 
                           levels = c("Extremely Liberal",
                                      "Slightly Liberal",
                                      "Liberal",
                                      "Moderate",
                                      "Slightly Conservative",
                                      "Conservative",
                                      "Extremely Conservative",
                                      "Prefer Not to Answer"))) |>
  mutate(education = case_when(
    education == "High School (includes GED)" ~ "High School",
    education == "Some College or Associate’s or Technical Degree" ~ "Some College",
    education == "4-year College Degree (Bachelor’s)" ~ "Bachelors",
    education == "Graduate or Professional Degree (Master’s, Ph.D., M.B.A., M.D., J.D., etc)" ~ "Gradutate Degree",
    TRUE ~ NA
  )) |>
  mutate(education = factor(education, levels = c("Primary School",
                                                 "High School",
                                                 "Some College",
                                                 "Bachelors",
                                                 "Gradutate Degree",
                                                 "Prefer Not to Answer"))) |>
  mutate(income = factor(income,
                         levels = c("Less than $25,000 per year",
                                    "$25,001 to $50,000 per year",
                                    "$50,001 to $75,000 per year",
                                   "$75,001 to $100,000 per year",
                                    "More than $100,000 per year",
                                    "Prefer Not to Answer"))) |>
  mutate(frac_children = children_in_household/(children_in_household+adults_in_household)) 

county_data <- reverse_zipcode(data$zipcode)  

merged_data <- merge(data, county_data, by = "zipcode") |>
  filter(state.x == "NH")
## Save R object 
saveRDS(data, file = "out/clean_data.rds")

## Save R object 
saveRDS(merged_data, file = "out/geocoded_data.rds")
