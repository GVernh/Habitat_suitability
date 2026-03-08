# =========================================
# Twite occurrences: bind years + clean + GRIDREF -> lon/lat
# (OS National Grid -> WGS84)
# =========================================

libs <- c("tidyverse", "readr", "sf", "stringr")

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

dir.create("./Data/Processed_data/twite_100m/", showWarnings = FALSE)

# ---- USER SETTINGS ----
file_1999 <- "./Data/100m_twite_survey_1999_2013/twite_1999.csv"   # change to your actual path/filename
file_2013 <- "./Data/100m_twite_survey_1999_2013/twite_2013.csv"   # change to your actual path/filename
out_file  <- "./Data/Processed_data/twite_100m/twite_1999_2013_cleaned_lonlat.csv"

grid_col <- "GRIDREF"

# ---- Helper: OS GridRef -> Easting/Northing (meters) ----
source("./R_scripts/Functions/gridref_to_en_one.R")

gridref_to_en_vec <- function(v) {
  m <- t(vapply(v, gridref_to_en_one, numeric(2)))
  tibble(E = m[,1], N = m[,2])
}

# ---- Read files (tab-delimited) + add year ----
d1999 <- read_csv(file_1999, show_col_types = FALSE) %>%
  select(GRIDREF) %>%
  mutate(year = 1999)

d2013 <- read_csv(file_2013, show_col_types = FALSE) %>%
  select(GRIDREF) %>%
  mutate(year = 2013)

dat <- bind_rows(d1999, d2013)

# ---- Clean: drop missing/blank GRIDREF ----
dat_clean <- dat %>%
  mutate(
    !!grid_col := str_trim(as.character(.data[[grid_col]])),
    !!grid_col := na_if(.data[[grid_col]], "")
  ) %>%
  filter(!is.na(.data[[grid_col]]))

# ---- Convert GRIDREF -> E/N ----
en <- gridref_to_en_vec(dat_clean[[grid_col]])
dat_clean <- bind_cols(dat_clean, en)

# drop rows where parsing failed
dat_clean <- dat_clean %>%
  filter(!is.na(E), !is.na(N))

# Remove improper inputs
valid_first <- c("S","T","N","H","O")

is_valid_os <- function(gr) {
  gr <- toupper(gr)
  gr <- gsub("[^A-Z0-9]", "", gr)
  if (!grepl("^[A-Z]{2}", gr)) return(FALSE)
  substr(gr,1,1) %in% valid_first
}

dat_clean <- dat_clean %>%
  mutate(valid_grid = sapply(GRIDREF, is_valid_os)) %>%
  filter(valid_grid)

# ---- Convert OSGB36 (EPSG:27700) -> WGS84 lon/lat (EPSG:4326) ----
pts <- st_as_sf(dat_clean, coords = c("E", "N"), crs = 27700, remove = FALSE)
pts_ll <- st_transform(pts, 4326)

xy <- st_coordinates(pts_ll)

dat_out <- pts_ll %>%
  st_drop_geometry() %>%
  mutate(
    lon = xy[, "X"],
    lat = xy[, "Y"]
  )

# ---- Write output ----
write_csv(dat_out, out_file)
message("Wrote: ", out_file)
