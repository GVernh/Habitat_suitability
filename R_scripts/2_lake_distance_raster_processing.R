libs <- c("terra", "sf", "dplyr")

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

dir.create("./Data/Processed_data/Freshwater/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Freshwater/1km/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Freshwater/25m/", showWarnings = FALSE)

# Template raster
temp_25 <- terra::rast("./Data/Processed_data/template_raster_25.tif")
temp_1km <- terra::rast("./Data/Processed_data/template_raster_1km.tif")

# Load geopackage
gpkg <- "./Data/Hydrology/UK_CEH_waterbodies/b6b92ce3-dcd7-4f0b-8e43-e937ddf1d4eb(1)/data/uklakes_v3_6_poly.gpkg"
st_layers(gpkg)          # lists layers + geometry type
lakes <- st_read(gpkg)   # if there’s only one layer, this just works
st_crs(lakes)
names(lakes)
st_geometry_type(lakes, by_geometry = FALSE)

lakes_bng <- lakes |>
  st_make_valid() |>
  st_transform(27700) |>
  st_collection_extract("POLYGON")

lakes_bng$area_m2 <- st_area(lakes_bng)
lakes_bng <- lakes_bng |> filter(as.numeric(area_m2) >= 1000)

# Vectorise
lakes_v <- terra::vect(lakes_bng)

# Rasterize
water_r <- terra::rasterize(lakes_v, temp_25, field = 1, background = NA)

# Distance raster
dist_lake <- terra::distance(water_r)
terra::writeRaster(dist_lake, "./Data/Processed_data/Freshwater/25m/lake_dist_25m.tif", overwrite = TRUE)

# Aggregate to 1km
lake_dist_1km_min <- terra::aggregate(dist_lake, fact = 40, fun = "min", na.rm = TRUE)
terra::writeRaster(lake_dist_1km_min, "./Data/Processed_data/Freshwater/1km/lake_dist_1km.tif", overwrite = TRUE)
