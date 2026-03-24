libs <- c("terra")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())

# Paths
lc_dir <- "./Data/Processed_data/CEH_land_cover/"
out_dir <- "./Data/Processed_data/CEH_land_cover/CEH_land_cover_1km_favourable/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Load functions
source("./R_scripts/Functions/Make_favourable_LC_raster.R")

# LC Data
LC_ras <- list.files(lc_dir, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
template_1km <- terra::rast("./data/Processed_data/template_raster_1km.tif")
template_100m <- terra::rast("./data/Processed_data/template_raster_100m.tif")

# 1km rasters
  # Favourable habitat classes:
  # 7 Acid grassland, 8 Fen, 9 Heather (heath), 11 Bog
favourable_codes <- c(7, 8, 9, 11)

# Run for all rasters
out_files <- vapply(
  LC_ras,
  make_favourable_habitat_ras,
  FUN.VALUE = character(1),
  template = template_1km,
  fav_codes = favourable_codes,
  out_dir = out_dir,
  res="1k"
)

# Dwarf shrub & heath
favourable_codes <- c(9, 10)
out_dir <- "./Data/Processed_data/CEH_land_cover/CEH_land_cover_1km_favourable/heath_dwarfShrub/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_files <- vapply(
  LC_ras,
  make_favourable_habitat_ras,
  FUN.VALUE = character(1),
  template = template_1km,
  fav_codes = favourable_codes,
  out_dir = out_dir,
  res = "1km"
)

#100m
favourable_codes <- c(7, 8, 9, 11)
out_dir <- "./Data/Processed_data/CEH_land_cover/CEH_land_cover_100m_favourable/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_files <- vapply(
  LC_ras,
  make_favourable_habitat_ras,
  FUN.VALUE = character(1),
  template = template_100m,
  fav_codes = favourable_codes,
  out_dir = out_dir,
  res = "100"
)

# Dwarf shrub & heath
favourable_codes <- c(9, 10)
out_dir <- "./Data/Processed_data/CEH_land_cover/CEH_land_cover_100m_favourable/heath_dwarfShrub/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_files <- vapply(
  LC_ras,
  make_favourable_habitat_ras,
  FUN.VALUE = character(1),
  template = template_100m,
  fav_codes = favourable_codes,
  out_dir = out_dir,
  res = "100"
)

#######################
###### % HABITAT ######
#######################

dir.create("./Data/Processed_data/CEH_land_cover/%_habitat/",
           showWarnings = FALSE, recursive = TRUE)

out_dir <- paste0("./Data/Processed_data/CEH_land_cover/%_habitat/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_dir <- paste0("./Data/Processed_data/CEH_land_cover/%_habitat/1km/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_dir <- paste0("./Data/Processed_data/CEH_land_cover/%_habitat/100m/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


x <- rast(LC_ras[1])
y <- levels(x)[[1]]

#1km
for (i in c(7, 8, 9, 10, 11)) {
  favourable_codes <- i
  tag <- y$class[y$value == i]

    out_dir <- paste0("./Data/Processed_data/CEH_land_cover/%_habitat/1km/", tag, "/")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  out_files <- vapply(
    LC_ras,
    make_percent_habitat_ras,
    FUN.VALUE = character(1),
    template = template_1km,
    fav_codes = favourable_codes,
    out_dir = out_dir,
    res = "1k",
    tag = tag
  )
}

# 100m
for (i in c(7, 8, 9, 10, 11)) {
  favourable_codes <- i
  tag <- y$class[y$value == i]
  
  out_dir <- paste0("./Data/Processed_data/CEH_land_cover/%_habitat/100m/", tag, "/")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  out_files <- vapply(
    LC_ras,
    make_percent_habitat_ras,
    FUN.VALUE = character(1),
    template = template_100m,
    fav_codes = favourable_codes,
    out_dir = out_dir,
    res = "100",
    tag = tag
  )
}

# ==========================
# Distance to feeding ground
# ==========================

source("./R_scripts/Functions/distance_to_LC_ras.R")

dir.create("./Data/Processed_data/Feeding_ground/",
           showWarnings = FALSE, recursive = TRUE)

# Get class table from first raster
x <- rast(LC_ras[1])
y <- levels(x)[[1]]

print(y)

# Combine all twite feeding habitats into one set of LC codes
twite_feeding_codes <- unique(c(
  y$value[grepl("Arable", y$class, ignore.case = TRUE)],
  y$value[grepl("Neutral grassland", y$class, ignore.case = TRUE)],
  y$value[grepl("Calcareous grassland", y$class, ignore.case = TRUE)],
  y$value[grepl("Acid grassland", y$class, ignore.case = TRUE)],
  y$value[grepl("Heather grassland|Heather", y$class, ignore.case = TRUE)]
))

print(twite_feeding_codes)

# 1km distance rasters
out_dir <- "./Data/Processed_data/Feeding_ground/1km/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_files <- vapply(
  LC_ras,
  make_distance_to_lc_ras,
  FUN.VALUE = character(1),
  template = template_1km,
  lc_codes = twite_feeding_codes,
  out_dir = out_dir,
  res = "1km",
  tag = "twite_feeding"
)

# 100m distance rasters
out_dir <- "./Data/Processed_data/Feeding_ground/100m/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_files <- vapply(
  LC_ras,
  make_distance_to_lc_ras,
  FUN.VALUE = character(1),
  template = template_100m,
  lc_codes = twite_feeding_codes,
  out_dir = out_dir,
  res = "100m",
  tag = "twite_feeding"
)

