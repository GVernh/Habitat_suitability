make_distance_to_lc_ras <- function(lc_file, template, lc_codes, out_dir, res = "1km", tag = "twite_feeding") {
  require(terra)
  
  # Read land cover raster
  x <- terra::rast(lc_file)
  
  # Align to template grid
  x_template <- terra::project(x, template, method = "near")
  
  # Create binary raster: target habitat = 1, all else = NA
  target <- x_template
  target[!(target[] %in% lc_codes)] <- NA
  target[target[] %in% lc_codes] <- 1
  
  # Distance to nearest target habitat cell
  d <- terra::distance(target)
  names(d) <- "dist_feeding_ground"
  # Output filename
  in_name <- tools::file_path_sans_ext(basename(lc_file))
  out_file <- file.path(out_dir, paste0(in_name, "_dist_to_", tag, "_", res, ".tif"))
  
  terra::writeRaster(d, out_file, overwrite = TRUE)
  
  return(out_file)
}