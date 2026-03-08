make_favourable_habitat_ras <- function(lc_path, template, fav_codes, out_dir, res, tag = NULL) {
  
  lc <- terra::rast(lc_path)
  
  # 1 = favourable, 0 = not favourable (NA stays NA)
  fav25 <- terra::ifel(lc %in% fav_codes, 1, 0)
  
  # Work out aggregation factor from resolutions (assumes square pixels)
  fact <- round(res(template)[1] / res(fav25)[1])  # 1000/25 = 40
  if (fact < 1) stop("Template resolution is finer than input; cannot aggregate.")
  
  # Proportion favourable within 1km cells (0-1)
  fav_agg <- terra::aggregate(fav25, fact = fact, fun = mean, na.rm = TRUE)
  
  if (!terra::compareGeom(fav_agg, template, stopOnError = FALSE)) {
    fav_1km <- terra::resample(fav_agg, template, method = "bilinear")
  } else {
    fav_1km <- fav_agg
  }
  
  # Optional: convert to percent (0-100)
  fav_1km_pct <- fav_1km * 100
  names(fav_1km_pct) <- "pct_favourable"
  
  # Output filename
  base <- tools::file_path_sans_ext(basename(lc_path))
  out_path <- file.path(out_dir, paste0(base, "_pct_favourable_",tag,"_",res,"m.tif"))
  
  terra::writeRaster(fav_1km_pct, out_path, overwrite = TRUE)
  return(out_path)
}







make_percent_habitat_ras <- function(lc_path, template, fav_codes, out_dir, res, tag = NULL) {
  
  lc <- terra::rast(lc_path)
  
  # 1 = favourable, 0 = not favourable (NA stays NA)
  fav25 <- terra::ifel(lc %in% fav_codes, 1, 0)
  
  # Work out aggregation factor from resolutions (assumes square pixels)
  fact <- round(res(template)[1] / res(fav25)[1])  # 1000/25 = 40
  if (fact < 1) stop("Template resolution is finer than input; cannot aggregate.")
  
  # Proportion favourable within 1km cells (0-1)
  fav_agg <- terra::aggregate(fav25, fact = fact, fun = mean, na.rm = TRUE)
  
  if (!terra::compareGeom(fav_agg, template, stopOnError = FALSE)) {
    fav_1km <- terra::resample(fav_agg, template, method = "bilinear")
  } else {
    fav_1km <- fav_agg
  }
  
  # Optional: convert to percent (0-100)
  fav_1km_pct <- fav_1km * 100
  names(fav_1km_pct) <- paste0(tag, "_%_habitat")
  
  # Output filename
  base <- tools::file_path_sans_ext(basename(lc_path))
  out_path <- file.path(out_dir, paste0(base, "_%_habitat_",tag,"_",res,"m.tif"))
  
  terra::writeRaster(fav_1km_pct, out_path, overwrite = TRUE)
  return(out_path)
}
