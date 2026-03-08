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

dir.create("./Data/Processed_data/Urban/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Urban/1km/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Urban/100m/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Urban/25m/", showWarnings = FALSE)


template_1km <- terra::rast("./Data/Processed_data/template_raster_1km.tif")
template_100 <- terra::rast("./Data/Processed_data/template_raster_100m.tif")
template_25 <- terra::rast("./Data/Processed_data/template_raster_25.tif")

urban <- st_read(
  "./Data/Urban/OS_Open_Built_Up_Areas_GeoPackage/os_open_built_up_areas.gpkg",
  layer = "os_open_built_up_areas"
) |> 
  st_transform(27700)

urban_ras_25 <- terra::rasterize(
  terra::vect(urban), template_25,
  field = 1, touches = TRUE, background = NA
)


dist_urban_25 <- distance(urban_ras_25)
terra::writeRaster(dist_urban_25, "./Data/Processed_data/Urban/25m/urban_dist_25m.tif", overwrite = TRUE)

# 100m
urban_dist_100m_min <- aggregate(dist_urban_25, fact = 4, fun = "min", na.rm = TRUE)
ext(urban_dist_100m_min) <- ext(template_100)
names(urban_dist_100m_min) <- "Urban_dist"

terra::writeRaster(urban_dist_100m_min, "./Data/Processed_data/Urban/100m/urban_dist_100m.tif", overwrite = TRUE)

# 1km
urban_dist_1km_min <- aggregate(dist_urban_25, fact = 40, fun = "min", na.rm = TRUE)
ext(urban_dist_1km_min) <- ext(template_1km)
names(urban_dist_1km_min) <- "Urban_dist"

terra::writeRaster(urban_dist_1km_min, "./Data/Processed_data/Urban/1km/urban_dist_1km.tif", overwrite = TRUE)
