# =========================================
# Twite: presences from NUMBIRDS -> lon/lat CSV
# Background: surveyed 1km squares (XKMSQ) -> 1km raster per year
# Exclude squares with Twite (per year) from background
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

out_bg_1999 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_1999_excl_twite.tif"
out_bg_2013 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_2013_excl_twite.tif"

# optional: save surveyed-square centroids used to make rasters
out_bg_pts_1999 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_1999_centroids_osgb.csv"
out_bg_pts_2013 <- "./Data/Processed_data/twite_100m/background_1km/surveyed_background_2013_centroids_osgb.csv"

gridref_col  <- "GRIDREF"   # finer grid ref (e.g., 100m)
kmsq_col     <- "XKMSQ"     # 1km square (e.g., NN2319)
numbirds_col <- "NUMBIRDS"

# ---- Helper: OS GridRef -> Easting/Northing (meters) ----
source("./R_scripts/Functions/gridref_to_en_one.R")
source("./R_scripts/Functions/make_bg_raster_for_year.R")

gridref_to_en_vec <- function(v) {
  m <- t(vapply(v, gridref_to_en_one, numeric(2)))
  tibble(E = m[,1], N = m[,2])
}

# ---- Basic OS validity check (adjust if your gridref parser is stricter) ----
valid_first <- c("S","T","N","H","O")
is_valid_os <- function(gr) {
  gr <- toupper(gr)
  gr <- gsub("[^A-Z0-9]", "", gr)
  if (!grepl("^[A-Z]{2}", gr)) return(FALSE)
  substr(gr, 1, 1) %in% valid_first
}

# ---- Read raw files + add year ----
parse_numbirds <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- dplyr::na_if(x, "")
  suppressWarnings(as.numeric(stringr::str_extract(x, "\\d+")))
}

d1999 <- read_csv(file_1999, show_col_types = FALSE) %>% mutate(year = 1999) %>%
  dplyr::select(gridref_col, kmsq_col, numbirds_col, year)
d2013 <- read_csv(file_2013, show_col_types = FALSE) %>%
  mutate(year = 2013) %>%
  dplyr::select(all_of(c(gridref_col, kmsq_col, numbirds_col, "year"))) %>%
  mutate(
    !!numbirds_col := parse_numbirds(.data[[numbirds_col]])
  )
dat <- bind_rows(d1999, d2013)

# ==========================================================
# PART A: PRESENCE POINTS (NUMBIRDS) -> lon/lat
# ==========================================================

occ <- dat %>%
  mutate(
    !!gridref_col := str_trim(as.character(.data[[gridref_col]])),
    !!gridref_col := na_if(.data[[gridref_col]], ""),
    numbirds = suppressWarnings(as.numeric(.data[[numbirds_col]]))
  ) %>%
  filter(!is.na(.data[[gridref_col]])) %>%
  filter(!is.na(numbirds) & numbirds > 0) %>%                 # keep only true Twite records
  mutate(valid_grid = sapply(.data[[gridref_col]], is_valid_os)) %>%
  filter(valid_grid) %>%
  distinct(year, .data[[gridref_col]], .keep_all = TRUE)       # de-dup per year per GRIDREF

# Convert GRIDREF -> OSGB36 E/N
en_occ <- gridref_to_en_vec(occ[[gridref_col]])
occ <- bind_cols(occ, en_occ) %>%
  filter(!is.na(E), !is.na(N))

# OSGB36 -> WGS84 lon/lat
occ_sf <- st_as_sf(occ, coords = c("E", "N"), crs = 27700, remove = FALSE)
occ_ll <- st_transform(occ_sf, 4326)
xy <- st_coordinates(occ_ll)

occ_out <- occ_ll %>%
  st_drop_geometry() %>%
  mutate(lon = xy[, "X"], lat = xy[, "Y"]) %>%
  select(year, !!gridref_col, numbirds, lon, lat, everything())

write_csv(occ_out, out_occ)
message("Wrote presences: ", out_occ)

# ==========================================================
# PART B: BACKGROUND RASTERS PER YEAR (surveyed 1km squares)
# Exclude squares with Twite in that year
# ==========================================================

# Clean surveyed 1km squares (all surveyed, not just Twite)
surveyed <- dat %>%
  mutate(
    !!kmsq_col := str_trim(as.character(.data[[kmsq_col]])),
    !!kmsq_col := na_if(.data[[kmsq_col]], "")
  ) %>%
  filter(!is.na(.data[[kmsq_col]])) %>%
  mutate(valid_km = sapply(.data[[kmsq_col]], is_valid_os)) %>%
  filter(valid_km) %>%
  distinct(year, .data[[kmsq_col]])

# Identify Twite 1km squares per year (to exclude from background)
twite_km <- dat %>%
  mutate(
    !!kmsq_col := str_trim(as.character(.data[[kmsq_col]])),
    !!kmsq_col := na_if(.data[[kmsq_col]], ""),
    numbirds = suppressWarnings(as.numeric(.data[[numbirds_col]]))
  ) %>%
  filter(!is.na(.data[[kmsq_col]])) %>%
  filter(!is.na(numbirds) & numbirds > 0) %>%
  distinct(year, .data[[kmsq_col]])

# Helper: build background raster for a given year
temp_1km <- rast("./Data/Processed_data/template_raster_1km.tif")

# Make rasters
make_bg_raster_for_year(1999, out_bg_1999, out_bg_pts_1999, template_r = temp_1km)
make_bg_raster_for_year(2013, out_bg_2013, out_bg_pts_2013, template_r = temp_1km)
