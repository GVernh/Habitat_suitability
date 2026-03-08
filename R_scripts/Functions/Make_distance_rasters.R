make_dist <- function(rast, template, out_path_25m, out_path_100, out_path_1km, tag = NULL) {
  v <- terra::vect(rast)
  r <- terra::rasterize(v, template, field = 1, background = NA, touches = TRUE)
  d <- terra::distance(r)
  terra::writeRaster(d, out_path_25m, overwrite = TRUE)
  
  dist_100m <- terra::aggregate(d, fact = 4, fun = "min", na.rm = TRUE)
  names(dist_100m) <- paste0("Lake_dist", tag)
  terra::writeRaster(dist_100m, out_path_100, overwrite = TRUE)
  
  dist_1km <- terra::aggregate(d, fact = 40, fun = "min", na.rm = TRUE)
  names(dist_1km) <- paste0("Lake_dist", tag)
  terra::writeRaster(dist_1km, out_path_1km, overwrite = TRUE)
}