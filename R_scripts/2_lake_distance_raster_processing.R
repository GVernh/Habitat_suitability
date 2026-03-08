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
dir.create("./Data/Processed_data/Freshwater/100m/", showWarnings = FALSE)
dir.create("./Data/Processed_data/Freshwater/25m/", showWarnings = FALSE)

source("./R_scripts/Functions/Make_distance_rasters.R")

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

# Create subsets to lake size
lakes_bng$area_m2 <- st_area(lakes_bng)
lakes_bng_1000 <- lakes_bng |> filter(as.numeric(area_m2) >= 1000)
lakes_bng_5000 <- lakes_bng |> filter(as.numeric(area_m2) >= 5000)
lakes_bng_10000 <- lakes_bng |> filter(as.numeric(area_m2) >= 10000)
lakes_bng_20000 <- lakes_bng |> filter(as.numeric(area_m2) >= 20000)

# Distance rasters
make_dist(lakes_bng, template = temp_25, out_path_25m = "./Data/Processed_data/Freshwater/25m/lake_dist_25m_all.tif",
          out_path_1km = "./Data/Processed_data/Freshwater/1km/lake_dist_1km_all.tif",
          out_path_100 = "./Data/Processed_data/Freshwater/100m/lake_dist_100m_all.tif",
          tag = "all")

make_dist(lakes_bng_1000, template = temp_25, out_path_25m = "./Data/Processed_data/Freshwater/25m/lake_dist_25m_0.1ha.tif",
          out_path_1km = "./Data/Processed_data/Freshwater/1km/lake_dist_1km_0.1ha.tif",
          out_path_100 = "./Data/Processed_data/Freshwater/100m/lake_dist_100m_0.1ha.tif",
          tag = "0.1ha")

make_dist(lakes_bng_5000, template = temp_25, out_path_25m = "./Data/Processed_data/Freshwater/25m/lake_dist_25m_0.5ha.tif",
          out_path_1km = "./Data/Processed_data/Freshwater/1km/lake_dist_1km_0.5ha.tif",
          out_path_100 = "./Data/Processed_data/Freshwater/100m/lake_dist_100m_0.5ha.tif",
          tag = "0.5ha")

make_dist(lakes_bng_10000, template = temp_25, out_path_25m = "./Data/Processed_data/Freshwater/25m/lake_dist_25m_1ha.tif",
          out_path_1km = "./Data/Processed_data/Freshwater/1km/lake_dist_1km_1ha.tif",
          out_path_100 = "./Data/Processed_data/Freshwater/100m/lake_dist_100m_1ha.tif",
          tag = "1ha")

make_dist(lakes_bng_20000, template = temp_25, out_path_25m = "./Data/Processed_data/Freshwater/25m/lake_dist_25m_2ha.tif",
          out_path_1km = "./Data/Processed_data/Freshwater/1km/lake_dist_1km_2ha.tif",
          out_path_100 = "./Data/Processed_data/Freshwater/100m/lake_dist_100m_2ha.tif",
          tag = "2ha")
