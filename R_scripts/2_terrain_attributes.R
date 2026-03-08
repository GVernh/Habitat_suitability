libs <- c("terra")

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

# whitebox::install_whitebox()

dir.create("./Data/Processed_data/Terrain/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Terrain/50m", showWarnings = FALSE)
dir.create("./Data/Processed_data/Terrain/100m", showWarnings = FALSE)
dir.create("./Data/Processed_data/Terrain/1km", showWarnings = FALSE)

temp <- terra::rast("./Data/Processed_data/template_raster_25.tif")
temp_100 <- terra::rast("./Data/Processed_data/template_raster_100m.tif")
temp_1km <- terra::rast("./Data/Processed_data/template_raster_1km.tif")
dtm <- terra::rast("./Data/DTM/UKCEH_HDTM/DTM.tif")

fact_100 <- 2
fact <- 20

#############
###  DTM ###
#############

dtm_100 <- aggregate(dtm, fact=fact_100, fun=mean, na.rm=TRUE)
dtm_100 <- resample(dtm_100, temp_100, method = "near")
names(dtm_100) <- "DTM_100"

terra::writeRaster(dtm_100, "./Data/Processed_data/Terrain/100m/DTM_100m.tif", overwrite=TRUE)

dtm_1km <- aggregate(dtm, fact=fact, fun=mean, na.rm=TRUE)
dtm_1km <- resample(dtm_100, temp_1km, method = "near")
names(dtm_1km) <- "DTM_1km"

terra::writeRaster(dtm_1km, "./Data/Processed_data/Terrain/1km/DTM_1km.tif", overwrite=TRUE)

#############
### SLOPE ###
#############

slope <- terrain(dtm, "slope", unit="degrees", neighbors=8)
names(slope) <- "Slope_deg"

terra::writeRaster(slope, "./Data/Processed_data/Terrain/50m/slope_50.tif", overwrite=TRUE)

# 100m
slope_mean <- aggregate(slope, fact=fact_100, fun=mean, na.rm=TRUE)
slope_sd   <- aggregate(slope, fact=fact_100, fun=sd,   na.rm=TRUE)

ext(slope_mean) <- ext(temp_100)
ext(slope_sd) <- ext(temp_100)

names(slope_mean) <- "Slope_mean"
names(slope_sd) <- "slope_SD"

terra::writeRaster(slope_mean, "./Data/Processed_data/Terrain/100m/slope_mean_100m.tif", overwrite=TRUE)
terra::writeRaster(slope_sd, "./Data/Processed_data/Terrain/100m/slope_sd_100m.tif", overwrite=TRUE)

# 1km
slope_mean <- aggregate(slope, fact=fact, fun=mean, na.rm=TRUE)
slope_sd   <- aggregate(slope, fact=fact, fun=sd,   na.rm=TRUE)

ext(slope_mean) <- ext(temp_1km)
ext(slope_sd) <- ext(temp_1km)

names(slope_mean) <- "Slope_mean"
names(slope_sd) <- "slope_SD"

terra::writeRaster(slope_mean, "./Data/Processed_data/Terrain/1km/slope_mean_1km.tif", overwrite=TRUE)
terra::writeRaster(slope_sd, "./Data/Processed_data/Terrain/1km/slope_sd_1km.tif", overwrite=TRUE)

##############
### ASPECT ###
##############

aspect <- terrain(dtm, "aspect", unit="degrees", neighbors=8)
names(aspect) <- "Aspect"

terra::writeRaster(aspect, "./Data/Processed_data/Terrain/50m/aspect_50m.tif", overwrite=TRUE)

rad <- aspect * pi/180
northness <- cos(rad)  # +1 = north-facing, -1 = south-facing
eastness  <- sin(rad)  # +1 = east-facing,  -1 = west-facing

#100m
north_mean <- aggregate(northness, fact=fact_100, fun=mean, na.rm=TRUE)
east_mean  <- aggregate(eastness,  fact=fact_100, fun=mean, na.rm=TRUE)

ext(north_mean) <- ext(temp_100)
ext(east_mean) <- ext(temp_100)

names(north_mean) <- "north_mean"
names(east_mean) <- "east_mean"

terra::writeRaster(north_mean, "./Data/Processed_data/Terrain/100m/northness_mean_100m.tif", overwrite=TRUE)
terra::writeRaster(east_mean, "./Data/Processed_data/Terrain/100m/eastness_mean_100m.tif", overwrite=TRUE)

aspect_conc <- sqrt(north_mean^2 + east_mean^2)
ext(aspect_conc) <- ext(temp_100)
names(aspect_conc) <- "aspect_conc"

terra::writeRaster(aspect_conc, "./Data/Processed_data/Terrain/100m/aspect_concentration_100m.tif", overwrite=TRUE)

#1km
north_mean <- aggregate(northness, fact=fact, fun=mean, na.rm=TRUE)
east_mean  <- aggregate(eastness,  fact=fact, fun=mean, na.rm=TRUE)

ext(north_mean) <- ext(temp_1km)
ext(east_mean) <- ext(temp_1km)
names(north_mean) <- "north_mean"
names(east_mean) <- "east_mean"

terra::writeRaster(north_mean, "./Data/Processed_data/Terrain/1km/northness_mean_1km.tif", overwrite=TRUE)
terra::writeRaster(east_mean, "./Data/Processed_data/Terrain/1km/eastness_mean_1km.tif", overwrite=TRUE)

aspect_conc <- sqrt(north_mean^2 + east_mean^2)
ext(aspect_conc) <- ext(temp_1km)
names(aspect_conc) <- "aspect_conc"

terra::writeRaster(aspect_conc, "./Data/Processed_data/Terrain/1km/aspect_concentration_1km.tif", overwrite=TRUE)

##########################################
### LOCAL TOPOGRAPHIC RUGGEDNESS INDEX ###
##########################################
tri3  <- terrain(dtm, "TRI", neighbors=8)   # classic 3x3 TRI
names(tri3) <- "TRI"

terra::writeRaster(tri3, "./Data/Processed_data/Terrain/50m/TRI_50m.tif", overwrite=TRUE)

#100m
tri3_mean <- aggregate(tri3, fact=fact_100, fun=mean, na.rm=TRUE)
ext(tri3_mean) <- ext(temp_100)
names(tri3_mean) <- "TRI"

terra::writeRaster(tri3_mean, "./Data/Processed_data/Terrain/100m/TRI_local_mean_100m.tif", overwrite=TRUE)


# 1km
tri3_mean <- aggregate(tri3, fact=fact, fun=mean, na.rm=TRUE)
ext(tri3_mean) <- ext(temp_1km)
names(tri3_mean) <- "TRI"

terra::writeRaster(tri3_mean, "./Data/Processed_data/Terrain/1km/TRI_local_mean_1km.tif", overwrite=TRUE)

###########################
### REGIONAL RUGGEDNESS ###
###########################

elev_sd_1km <- aggregate(dtm, fact=fact, fun=sd, na.rm=TRUE)
ext(elev_sd_1km) <- ext(temp_1km)
names(elev_sd_1km) <- "Elev_SD"

terra::writeRaster(elev_sd_1km, "./Data/Processed_data/Terrain/1km/elev_regional_SD_1km.tif", overwrite=TRUE)

################################
### REGIONAL ELEVATION RANGE ###
################################

elev_rng_1km <- aggregate(
  dtm,
  fact = fact,
  fun = function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    }
  }
)

ext(elev_rng_1km) <- ext(temp_1km)
names(elev_rng_1km) <- "Elev_RNG"

terra::writeRaster(elev_rng_1km, "./Data/Processed_data/Terrain/1km/elev_range_1km.tif", overwrite=TRUE)
