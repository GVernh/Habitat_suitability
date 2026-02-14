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

dir.create(file.path("./Data/", "Processed_data"), showWarnings = FALSE)
dir.create(file.path("./", "R_scripts"), showWarnings = FALSE)

# Data
hyd_feat <- terra::rast("./Data/Hydrology/Copernicus_river_lake_shapefile/hyd_feat_25.tif")
coastline <- terra::vect("./Data/Hydrology/Coastline/gb_shp/gb.shp")

# Project coastline into same projection as target data
coastline_LEA <- terra::project(coastline, hyd_feat)

# Crop data to UK
x <- terra::crop(hyd_feat, coastline_LEA)
plot(x)

# Project data to projects targets crs
Hyd_fea_wgs84 <- terra::project(x, coastline)



# NOTES
# Terra::distance can compute the distance of all NA cells to cells that are not NA

