# =========================================
# Twite 100m records:
#  - Presence points from NUMBIRDS -> lon/lat CSV
#  - Background 1km squares (surveyed, excl. Twite) -> raster per year
#  - Effort surface from TOTVISITS -> raster per year
# =========================================

libs <- c("tidyverse", "readr", "sf", "stringr", "terra")
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])
invisible(lapply(libs, library, character.only = TRUE))

dir.create("./Data/Processed_data/twite_100m/", showWarnings = FALSE, recursive = TRUE)
dir.create("./Data/Processed_data/twite_100m/background_1km/", showWarnings = FALSE, recursive = TRUE)

# ---- USER SETTINGS ----
file_1999 <- "./Data/100m_twite_survey_1999_2013/twite_1999.csv"
file_2013 <- "./Data/100m_twite_survey_1999_2013/twite_2013.csv"

out_occ <- "./Data/Processed_data/twite_100m/twite_presences_1999_2013_lonlat.csv"

# BACKGROUND OUTPUT (mask of surveyed squares excluding Twite squares)
out_bg_1999 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_1999_excl_twite.tif"
out_bg_2013 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_2013_excl_twite.tif"
out_bg_pts_1999 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_1999_centroids.csv"
out_bg_pts_2013 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_2013_centroids.csv"

# EFFORT OUTPUT (TOTVISITS per surveyed square; includes Twite squares too)
out_eff_1999 <- "./Data/Processed_data/twite_100m/background_1km/effort_totvisits_1999.tif"
out_eff_2013 <- "./Data/Processed_data/twite_100m/background_1km/effort_totvisits_2013.tif"
out_eff_pts_1999 <- "./Data/Processed_data/twite_100m/background_1km/effort_totvisits_1999_centroids.csv"
out_eff_pts_2013 <- "./Data/Processed_data/twite_100m/background_1km/effort_totvisits_2013_centroids.csv"

# column names in your CSVs
gridref_col  <- "GRIDREF"   # 100m-ish gridref
kmsq_col     <- "XKMSQ"     # 1km square code (e.g., NN2319)
numbirds_col <- "NUMBIRDS"
effort_col   <- "TOTVISITS"

# ---- Helper: OS GridRef -> Easting/Northing (meters) ----
source("./R_scripts/Functions/gridref_to_en_one.R")
source("./R_scripts/Functions/make_bg_raster_for_year.R")

gridref_to_en_vec <- function(v) {
  m <- t(vapply(v, gridref_to_en_one, numeric(2)))
  tibble(E = m[,1], N = m[,2])
}

# ---- Basic OS validity check ----
valid_first <- c("S","T","N","H","O")
is_valid_os <- function(gr) {
  gr <- toupper(gr)
  gr <- gsub("[^A-Z0-9]", "", gr)
  if (!grepl("^[A-Z]{2}", gr)) return(FALSE)
  substr(gr, 1, 1) %in% valid_first
}

# ---- Parsers ----
parse_numbirds <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- dplyr::na_if(x, "")
  suppressWarnings(as.numeric(stringr::str_extract(x, "\\d+")))  # "1+" -> 1
}
parse_num <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- dplyr::na_if(x, "")
  suppressWarnings(as.numeric(stringr::str_extract(x, "\\d+")))
}

# ---- Read raw files + add year ----
d1999 <- read_csv(file_1999, show_col_types = FALSE) %>%
  mutate(year = 1999) %>%
  dplyr::select(all_of(c(gridref_col, kmsq_col, numbirds_col, effort_col, "year"))) %>%
  mutate(
    !!numbirds_col := parse_numbirds(.data[[numbirds_col]]),
    !!effort_col   := parse_num(.data[[effort_col]])
  )

d2013 <- read_csv(file_2013, show_col_types = FALSE) %>%
  mutate(year = 2013) %>%
  dplyr::select(all_of(c(gridref_col, kmsq_col, numbirds_col, effort_col, "year"))) %>%
  mutate(
    !!numbirds_col := parse_numbirds(.data[[numbirds_col]]),
    !!effort_col   := parse_num(.data[[effort_col]])
  )

dat <- bind_rows(d1999, d2013)

# ==========================================================
# PART A: PRESENCE POINTS (NUMBIRDS) -> lon/lat
# ==========================================================
occ <- dat %>%
  mutate(
    !!gridref_col := str_trim(as.character(.data[[gridref_col]])),
    !!gridref_col := na_if(.data[[gridref_col]], "")
  ) %>%
  filter(!is.na(.data[[gridref_col]])) %>%
  filter(!is.na(.data[[numbirds_col]]) & .data[[numbirds_col]] > 0) %>%
  mutate(valid_grid = sapply(.data[[gridref_col]], is_valid_os)) %>%
  filter(valid_grid) %>%
  distinct(year, .data[[gridref_col]], .keep_all = TRUE)

en_occ <- gridref_to_en_vec(occ[[gridref_col]])
occ <- bind_cols(occ, en_occ) %>% filter(!is.na(E), !is.na(N))

occ_sf <- st_as_sf(occ, coords = c("E", "N"), crs = 27700, remove = FALSE)
occ_ll <- st_transform(occ_sf, 4326)
xy <- st_coordinates(occ_ll)

occ_out <- occ_ll %>%
  st_drop_geometry() %>%
  mutate(lon = xy[, "X"], lat = xy[, "Y"]) %>%
  select(year, !!gridref_col, !!numbirds_col, !!effort_col, lon, lat, everything())

write_csv(occ_out, out_occ)
message("Wrote presences: ", out_occ)

# ==========================================================
# PART B: survey effort per 1km square + background mask per year
# ==========================================================

# 1) Survey effort table (one row per year x square)
surveyed_eff <- dat %>%
  mutate(
    !!kmsq_col := str_trim(as.character(.data[[kmsq_col]])),
    !!kmsq_col := na_if(.data[[kmsq_col]], "")
  ) %>%
  filter(!is.na(.data[[kmsq_col]])) %>%
  mutate(valid_km = sapply(.data[[kmsq_col]], is_valid_os)) %>%
  filter(valid_km) %>%
  group_by(year, .data[[kmsq_col]]) %>%
  summarise(
    TOTVISITS = suppressWarnings(max(.data[[effort_col]], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(TOTVISITS = dplyr::na_if(TOTVISITS, -Inf))

# 2) Twite 1km squares per year (exclude from background)
twite_km <- dat %>%
  mutate(
    !!kmsq_col := str_trim(as.character(.data[[kmsq_col]])),
    !!kmsq_col := na_if(.data[[kmsq_col]], "")
  ) %>%
  filter(!is.na(.data[[kmsq_col]])) %>%
  filter(!is.na(.data[[numbirds_col]]) & .data[[numbirds_col]] > 0) %>%
  distinct(year, .data[[kmsq_col]])

# Template raster (1km)
temp_1km <- rast("./Data/Processed_data/template_raster_1km.tif")

# Run for each year
make_bg_and_effort_for_year(1999, out_bg_1999, out_bg_pts_1999, out_eff_1999, out_eff_pts_1999, temp_1km)
make_bg_and_effort_for_year(2013, out_bg_2013, out_bg_pts_2013, out_eff_2013, out_eff_pts_2013, temp_1km)