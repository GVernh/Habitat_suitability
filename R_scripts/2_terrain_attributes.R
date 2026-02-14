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
dir.create("./Data/Processed_data/Terrain/1km", showWarnings = FALSE)

temp <- terra::rast("./Data/Processed_data/template_raster_25.tif")
temp_1km <- terra::rast("./Data/Processed_data/template_raster_1km.tif")
dtm <- terra::rast("./Data/DTM/UKCEH_HDTM/DTM.tif")

fact <- 20

#############
### SLOPE ###
#############

slope <- terrain(dtm, "slope", unit="degrees", neighbors=8)

terra::writeRaster(slope, "./Data/Processed_data/Terrain/50m/slope_50.tif", overwrite=TRUE)

slope_mean <- aggregate(slope, fact=fact, fun=mean, na.rm=TRUE)
slope_sd   <- aggregate(slope, fact=fact, fun=sd,   na.rm=TRUE)

ext(slope_mean) <- ext(temp_1km)
ext(slope_sd) <- ext(temp_1km)

terra::writeRaster(slope_mean, "./Data/Processed_data/Terrain/1km/slope_mean_1km.tif", overwrite=TRUE)
terra::writeRaster(slope_sd, "./Data/Processed_data/Terrain/1km/slope_sd_1km.tif", overwrite=TRUE)

##############
### ASPECT ###
##############

aspect <- terrain(dtm, "aspect", unit="degrees", neighbors=8)

terra::writeRaster(aspect, "./Data/Processed_data/Terrain/50m/aspect_50m.tif", overwrite=TRUE)

rad <- aspect * pi/180
northness <- cos(rad)  # +1 = north-facing, -1 = south-facing
eastness  <- sin(rad)  # +1 = east-facing,  -1 = west-facing

north_mean <- aggregate(northness, fact=fact, fun=mean, na.rm=TRUE)
east_mean  <- aggregate(eastness,  fact=fact, fun=mean, na.rm=TRUE)

ext(north_mean) <- ext(temp_1km)
ext(east_mean) <- ext(temp_1km)

terra::writeRaster(north_mean, "./Data/Processed_data/Terrain/1km/northness_mean_1km.tif", overwrite=TRUE)
terra::writeRaster(east_mean, "./Data/Processed_data/Terrain/1km/eastness_mean_1km.tif", overwrite=TRUE)

aspect_conc <- sqrt(north_mean^2 + east_mean^2)
ext(aspect_conc) <- ext(temp_1km)

terra::writeRaster(aspect_conc, "./Data/Processed_data/Terrain/1km/aspect_concentration_1km.tif", overwrite=TRUE)

##########################################
### LOCAL TOPOGRAPHIC RUGGEDNESS INDEX ###
##########################################
tri3  <- terrain(dtm, "TRI", neighbors=8)   # classic 3x3 TRI

terra::writeRaster(tri3, "./Data/Processed_data/Terrain/50m/TRI_50m.tif", overwrite=TRUE)

tri3_mean <- aggregate(tri3, fact=fact, fun=mean, na.rm=TRUE)
ext(tri3_mean) <- ext(temp_1km)

terra::writeRaster(tri3_mean, "./Data/Processed_data/Terrain/1km/TRI_local_mean_1km.tif", overwrite=TRUE)

###########################
### REGIONAL RUGGEDNESS ###
###########################

elev_sd_1km <- aggregate(dtm, fact=fact, fun=sd, na.rm=TRUE)
ext(elev_sd_1km) <- ext(temp_1km)

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

terra::writeRaster(elev_rng_1km, "./Data/Processed_data/Terrain/1km/elev_range_1km.tif", overwrite=TRUE)
