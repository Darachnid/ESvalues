library(tidyverse)
library(sf)
library(stringr)

theme_set(theme_minimal())

## Generate Required Datafiles

states_data <- tigris::states(class = "sf")
nh_boundary <- states_data[states_data$STUSPS == "NH", ]

data <- readRDS("out/geocoded_data.rds") 

data$county <-  substr(data$county.x, 1, nchar(data$county.x) - 7)
data$state <- data$state.x

county_bounds <- tigris::counties(cb = TRUE, class = "sf") |>
  rename("county" = NAME) |>
  rename("state" = STUSPS) |>
  mutate("costate" = paste0(county, ", ", state))

zip_boundaries <- tigris::zctas(cb = TRUE, class = "sf", year = 2020)  |>
  rename("zipcode" = ZCTA5CE20) 


############### ES values by County ###############

county_data <- data |>
  group_by(county, state) |>
  summarise(across(starts_with("val"), ~ mean(., na.rm = TRUE)),
            across(starts_with("val"), ~ n(), .names = "count_{.col}")) |>
  mutate("costate" = paste0(county, ", NH")) |>
  merge(county_bounds, by = "costate") |>
  mutate("county" = costate) |>
  mutate(county = substr(county, 1, nchar(county) - 4)) |>
  mutate(across(starts_with("count_"), as.numeric)) |>
  mutate(across(starts_with("val_"), as.numeric)) |>
  mutate(county = factor(county)) |>
  select(starts_with("val"), starts_with("count"), 
         geometry, county) |>
  select(-county.x, -county.y,
         -COUNTYFP, -COUNTYNS) 

    
long_means <- county_data |>  pivot_longer(cols = starts_with("val"),
               names_to = "ES",
               values_to = "mean") |>
  select(-starts_with("val"), -starts_with("count_")) |>
  arrange(ES, county)
county_data |> pivot_longer(cols = starts_with("count_"),
               names_to = "ES",
               values_to = "count") |>
  select(-starts_with("val"), -starts_with("count_")) |>
  arrange(ES, county) |>
  mutate("mean" = long_means$mean) |>
  mutate(ES = gsub("val_", "", ES)) |>
  mutate(ES = gsub("count_", "", ES)) |>
  mutate(ES = gsub("_", " ", ES)) |>
  mutate(ES = str_to_title(ES)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry, 
              fill = mean,
              alpha = count)) +
  scale_alpha_continuous(range = c(0.25, 1)) +
  geom_sf(data = nh_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf_text(aes(geometry = geometry,
                    label = as.character(count)),
               alpha = 0.3) +
  scale_fill_viridis_c(limits = c(0, 5)) +
  facet_wrap(~ES) +
  theme(
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),   
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank()) +
  labs(fill = "Mean ES Value",
       title = "Values for 11 Ecosystem Services by County",
       subtitle = "number labels and transparency indicate response counts") +
  guides(alpha = "none")

ggsave("out/maps/map__ES_means_per_county.pdf", scale = 1.5)
ggsave("out/maps/map__ES_means_per_county.png", bg = "white", scale = 1.5)




  

