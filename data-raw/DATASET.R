## code to prepare `DATASET` dataset goes here

# Preparea data for maps.

library(maps)

countries_sf <- rnaturalearth::ne_countries(scale = "small")
states_sf    <- rnaturalearth::ne_states()

usethis::use_data(countries_sf, states_sf, overwrite = TRUE)

