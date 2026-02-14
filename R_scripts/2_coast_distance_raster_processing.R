libs <- c("terra", "sf", "dplyr", "rnaturalearth")

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

dir.create("./Data/Processed_data/Coast/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Coast/1km/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Coast/25m/", showWarnings = FALSE)

template <- terra::rast("./Data/Processed_data/template_raster_1km.tif")
template_25 <- terra::rast("./Data/Processed_data/template_raster_25.tif")

gpkg <- "./Data/Coastline/opmplc_gpkg_gb/Data/opmplc_gb.gpkg"
st_layers(gpkg)

coast <- st_read(
  gpkg,
  layer = "tidal_boundary",        # <-- change to exact name from st_layers()
  quiet = TRUE
)

coast_25m <- terra::rasterize(vect(coast), template_25, field = 1, touches = TRUE, background = NA)

coast_dist_25 <- distance(coast_25m)
terra::writeRaster(coast_dist_25, "./Data/Processed_data/Coast/25m/coast_dist_25m.tif", overwrite = TRUE)


coast_dist_1km_min <- terra::aggregate(coast_dist_25, fact = 40, fun = "min", na.rm = TRUE)
terra::writeRaster(coast_dist_1km_min, "./Data/Processed_data/Coast/1km/coast_dist_1km.tif", overwrite = TRUE)
