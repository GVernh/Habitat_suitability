libs <- c("tidyverse", "lubridate", "sf")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list = ls())


# 1km DATA ############
df <- read_csv("./data/NBN_data/data/data.csv")

# Standardise column names (makes them lowercase with underscores)
df <- df %>% janitor::clean_names()
names(df)

clean_twite <- df %>%
  # Keep only rows with valid lat/long columns
  filter(
    !is.na(latitude_wgs84),
    !is.na(longitude_wgs84),
    latitude_wgs84 != 0,
    longitude_wgs84 != 0
  ) %>%
  
  # Ensure date is parsed
  dplyr::filter(str_detect(scientific_name, "Linaria flavirostris")) %>%
  dplyr::filter(start_date_year >= 1990) %>%
  mutate(date = as.Date(start_date)) %>%
  filter(month(date) >= 3 & month(date) <= 9) %>%
  # Possible seperate analysis of May- July:
  #filter(month(date) >= 5 & month(date) <= 7) %>%
  filter(coordinate_uncertainty_m <= 1000) %>%
  
  # Keep only accepted verified observations (if field exists)
  filter(
    identification_verification_status %in% c("Accepted", 
                                              "Accepted - correct",
                                              "accepted",
                                              "validated",
                                              "Accepted - considered correct")|
      is.na(identification_verification_status)
  ) %>%
  
  # Keep human observations only
  filter(
    is.na(basis_of_record) | basis_of_record == "HumanObservation"
  ) %>%
  
  # Remove duplicates based on species + location + date
  distinct(scientific_name, latitude_wgs84, longitude_wgs84, date, .keep_all = TRUE)

twite_sf <- st_as_sf(
  clean_twite,
  coords = c("longitude_wgs84", "latitude_wgs84"),
  crs = 4326,
  remove = FALSE
)

twite_bng <- st_transform(twite_sf, 27700)
coords_bng <- st_coordinates(twite_bng)

twite_out <- twite_bng %>%
  mutate(easting_BNG = coords_bng[,1],
         northing_BNG = coords_bng[,2]) %>%
  st_drop_geometry()

write.csv(twite_out, "./data/Processed_data/NBN/twite_occurence_cleaned.csv", row.names = F)


