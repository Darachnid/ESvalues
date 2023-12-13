library(tidyverse)
library(ggthemes)
library(tigris)


theme_set(theme_minimal())

states_data <- tigris::states(class = "sf")
nh_boundary <- states_data[states_data$STUSPS == "NH", ]

geocoded_data <- readRDS("out/geocoded_data.rds") 

geocoded_data$county <-  substr(geocoded_data$county.x, 1, nchar(geocoded_data$county.x) - 7)
geocoded_data$state <- geocoded_data$state.x

county_bounds <- tigris::counties(cb = TRUE, class = "sf") |>
  rename("county" = NAME) |>
  rename("state" = STUSPS) |>
  mutate("costate" = paste0(county, ", ", state))

zip_boundaries <- tigris::zctas(cb = TRUE, class = "sf", year = 2020)  |>
  rename("zipcode" = ZCTA5CE20) 


############### Respondents by County ###############

county_data <- geocoded_data |>
  group_by(county) |>
  summarise("count" = n()) |>
  na.omit() |>
  arrange(desc(count)) |>
  mutate("costate" = paste0(county, ", NH")) |>
  merge(county_bounds, by = "costate")

ggplot(data = county_data) +
  geom_sf(aes(fill = count, geometry = geometry)) +
  geom_sf(data = nh_boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c() +
  labs(title = "Respondents per County", fill = "Count")

ggsave("out/maps/map__respondents_per_county.png", bg = "WHITE")
ggsave("out/maps/map__respondents_per_county.pdf")


############### Respondents by Zipcode ###############

zip_data <- geocoded_data |>
  group_by(zipcode) |>
  summarise("count" = n()) |>
  na.omit() |>
  arrange(desc(count)) |>
  merge(zip_boundaries, by = "zipcode")

zip_data |> 
  ggplot() +
  geom_sf(aes(fill = count, geometry = geometry)) +
  geom_sf(data = nh_boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c() +
  labs(title = "Respondents per Zipcode", fill = "Count")

ggsave("out/maps/map__respondents_per_zipcode.png", bg = "WHITE")
ggsave("out/maps/map__respondents_per_zipcode.pdf")
