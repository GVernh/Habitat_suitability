libs <- c("terra")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())


all_files <- list.files(
  "./Data/Processed_data/",
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)
# Template

template_100m <- terra::rast("./Data/Processed_data/template_raster_100m.tif")

#################################
########## 1999 #################
#################################

# 1km climate
tmp_1999 <- all_files[grep("tas_.*_1999\\.tif$", all_files)]
prec_1999 <- all_files[grep("rainfall_.*_1999\\.tif$", all_files)]
wind_1999 <- all_files[grep("sfcWind_.*_1999\\.tif$", all_files)]
groundfrost_1999 <- all_files[grep("groundfrost_.*_1999\\.tif$", all_files)]

# 100m habitat
hab_1999 <- all_files[grep("LC_2000_.*%_habit.*100m\\.tif$", all_files)] # names fine

#################################
########## 2013 #################
#################################

#1km climate
tmp_2013 <- all_files[grep("tas_.*_2013\\.tif$", all_files)]
prec_2013 <- all_files[grep("rainfall_.*_2013\\.tif$", all_files)]
wind_2013 <- all_files[grep("sfcWind_.*_2013\\.tif$", all_files)]
groundfrost_2013 <- all_files[grep("groundfrost_.*_2013\\.tif$", all_files)]

# 100m habitat
hab_2013 <- all_files[grep("LC_2015_.*%_habit.*100m\\.tif$", all_files)] # names fine

#################################
########## Static ###############
#################################
terrain <- all_files[grep("(DTM|slope_mean|aspect|TRI).*100m", all_files)]
distance <- all_files[grep("(road|urban|coast).*100m", all_files)]

#################################
###### Background + Effort ######
#################################

BG_1999 <- all_files[grepl("^surveyed_background_1999.*\\.tif$", basename(all_files))]
effort_1999 <- all_files[grepl("^effort_totvisits_1999.*\\.tif$", basename(all_files))]

BG_2013 <- all_files[grepl("^surveyed_background_2013.*\\.tif$", basename(all_files))]
effort_2013 <- all_files[grepl("^effort_totvisits_2013.*\\.tif$", basename(all_files))]

#################################
######## TWITE OCCURENCE ########
#################################

twite <- read.csv("./Data/Processed_data/twite_100m/twite_presences_1999_2013_lonlat.csv")

# Data cubes ----
################################
###### predictor STACK #########
################################
clim_1999_1km <- terra::rast(c(tmp_1999, prec_1999, wind_1999, groundfrost_1999))
clim_2013_1km <- terra::rast(c(tmp_2013, prec_2013, wind_2013, groundfrost_2013))

clim_1999_100m <- terra::resample(clim_1999_1km, template_100m, method = "near")
clim_2013_100m <- terra::resample(clim_2013_1km, template_100m, method = "near")

eff_1999_100m <- terra::resample(terra::rast(effort_1999), template_100m, method = "near")
eff_2013_100m <- terra::resample(terra::rast(effort_2013), template_100m, method = "near")

# stacks
pred_1999 <- c(
  clim_1999_100m,
  terra::rast(hab_1999),
  terra::rast(terrain),
  terra::rast(distance),
  eff_1999_100m
)

pred_2013 <- c(
  clim_2013_100m,
  terra::rast(hab_2013),
  terra::rast(terrain),
  terra::rast(distance),
  eff_2013_100m
)

rm(list = setdiff(ls(), c("pred_1999", "pred_2013", "twite", "BG_1999", "BG_2013", "template_100m")))

# Twite
twite_1999 <- subset(twite, year == 1999)
twite_2013 <- subset(twite, year == 2013)

pres_1999 <- terra::vect(twite_1999, geom = c("lon","lat"), crs = "EPSG:4326")
pres_2013 <- terra::vect(twite_2013, geom = c("lon","lat"), crs = "EPSG:4326")

pres_1999 <- terra::project(pres_1999, terra::crs(template_100m))
pres_2013 <- terra::project(pres_2013, terra::crs(template_100m))

# Background rasters
bgmask_1999_100m <- terra::resample(terra::rast(BG_1999), template_100m, method = "near")
bgmask_2013_100m <- terra::resample(terra::rast(BG_2013), template_100m, method = "near")

#1999
# valid non-NA cells in background mask
valid_cells_1999 <- which(!is.na(values(bgmask_1999_100m)))

# sample required number of cells
bg_cells_1999 <- sample(
  valid_cells_1999,
  size = min(length(valid_cells_1999), nrow(twite_1999) * 10)
)

# convert sampled cells to x/y
xy_1999 <- terra::xyFromCell(bgmask_1999_100m, bg_cells_1999)
xy_1999 <- data.frame(x = xy_1999[,1], y = xy_1999[,2])

# make SpatVector points
bg_1999 <- terra::vect(
  xy_1999,
  geom = c("x","y"),
  crs = terra::crs(bgmask_1999_100m)
)

#2013
# valid non-NA cells in background mask
valid_cells_2013 <- which(!is.na(values(bgmask_2013_100m)))

# sample required number of cells
bg_cells_2013 <- sample(
  valid_cells_2013,
  size = min(length(valid_cells_2013), nrow(twite_2013) * 10)
)

# convert sampled cells to x/y
xy_2013 <- terra::xyFromCell(bgmask_2013_100m, bg_cells_2013)
xy_2013 <- data.frame(x = xy_2013[,1], y = xy_2013[,2])

# make SpatVector points
bg_2013 <- terra::vect(
  xy_1999,
  geom = c("x","y"),
  crs = terra::crs(bgmask_2013_100m)
)


# Extract
pres_vals_1999 <- terra::extract(pred_1999, pres_1999)
bg_vals_1999 <- terra::extract(pred_1999, bg_1999)

pres_vals_2013 <- terra::extract(pred_2013, pres_2013)
bg_vals_2013   <- terra::extract(pred_2013, bg_2013)


# Assemble response df
pres_vals_1999$presence <- 1; bg_vals_1999$presence <- 0
pres_vals_2013$presence <- 1; bg_vals_2013$presence <- 0

pres_vals_1999$year <- 1999; bg_vals_1999$year <- 1999
pres_vals_2013$year <- 2013; bg_vals_2013$year <- 2013

# Remove years from column names
names(pres_vals_1999) <- gsub("_1999", "", names(pres_vals_1999))
names(bg_vals_1999)   <- gsub("_1999", "", names(bg_vals_1999))

names(pres_vals_2013) <- gsub("_2013", "", names(pres_vals_2013))
names(bg_vals_2013)   <- gsub("_2013", "", names(bg_vals_2013))

# Create full dataframe
df <- rbind(pres_vals_1999, bg_vals_1999, pres_vals_2013, bg_vals_2013)
df <- na.omit(df)
