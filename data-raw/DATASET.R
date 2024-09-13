## code to prepare `DATASET` dataset goes here

# Preparea data for maps.

library(maps)

countries_sf <- rnaturalearth::ne_countries(scale = "small")
countries_sf <- countries_sf["name"]

states_sf    <- rnaturalearth::ne_states()
states_sf    <- states_sf["name"]

usethis::use_data(countries_sf, states_sf, overwrite = TRUE)

