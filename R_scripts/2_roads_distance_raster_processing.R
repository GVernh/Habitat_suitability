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

# DIR
dir.create("./Data/Processed_data/Roads/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Roads/1km/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Roads/25m/", showWarnings = FALSE)

# DATA
st_layers("./Data/Roads/oproad_gpkg_gb/Data/oproad_gb.gpkg")
template <- terra::rast("./Data/Processed_data/template_raster_1km.tif")
template_25 <- terra::rast("./Data/Processed_data/template_raster_25.tif")

roads <- st_read(
  "./Data/Roads/oproad_gpkg_gb/Data/oproad_gb.gpkg",
  layer = "road_link"
) |> 
  st_transform(27700)

unique(roads$road_classification)

major <- roads |> 
  dplyr::filter(road_classification %in% c("Motorway", "A Road"))


########
# 1) Rasterize major roads onto the *exact* 25 m template
r_roads <- rasterize(vect(major), template_25,
                     field = 1, touches = TRUE, background = NA)

# 2) Distance (metres)
r_dist_25 <- distance(r_roads)
terra::writeRaster(r_dist_25, "./Data/Processed_data/Roads/25m/road_dist_25.tif", overwrite = TRUE)

# 3) Aggregate to 1 km using min (40 x 40 cells)
r_dist_1km_min <- aggregate(r_dist_25, fact = 40, fun = "min", na.rm = TRUE)
terra::writeRaster(r_dist_1km_min, "./Data/Processed_data/Roads/1km/road_dist_1km.tif", overwrite = TRUE)

###### MASK? #######

# # 1) Get Natural Earth LAND polygons (coastline/landmass)
# land_sf <- rnaturalearth::ne_download(scale = 10, type = "land",
#                                       category = "physical", returnclass = "sf")
# 
# land <- vect(land_sf)
# land <- project(land, "EPSG:27700")
# 
# # 2) Crop land to your raster extent (fast)
# land_crop <- crop(land, ext(r_dist))
# 
# # 3) Rasterize land to EXACT raster grid (this avoids thin slivers)
# land_mask <- rasterize(land_crop, r_dist, field = 1, background = NA)
# 
# # 4) Apply mask
# r_dist_land <- mask(r_dist, land_mask)
# 
# terra::plot(r_dist_land)
