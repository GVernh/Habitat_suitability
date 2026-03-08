# make_bg_raster_for_year <- function(year_value, out_raster, out_pts_csv, template_r) {
#   
#   # surveyed squares for year, excluding squares with Twite in same year
#   bg_year <- surveyed %>%
#     filter(year == year_value) %>%
#     anti_join(twite_km %>% filter(year == year_value), by = c("year", kmsq_col)) %>%
#     distinct(.data[[kmsq_col]])
#   
#   if (nrow(bg_year) == 0) {
#     warning("No background squares left after excluding Twite for year ", year_value,
#             ". Check XKMSQ coverage and NUMBIRDS filtering.")
#     return(invisible(NULL))
#   }
#   
#   # Convert 1km XKMSQ -> OSGB36 E/N (SW corner)
#   en_bg <- gridref_to_en_vec(bg_year[[kmsq_col]])
#   bg_year <- bind_cols(bg_year, en_bg) %>%
#     filter(!is.na(E), !is.na(N)) %>%
#     mutate(E_c = E + 500, N_c = N + 500)
#   
#   # Add lon/lat for CSV output
#   bg_sf <- sf::st_as_sf(bg_year, coords = c("E_c", "N_c"), crs = 27700, remove = FALSE)
#   bg_ll <- sf::st_transform(bg_sf, 4326)
#   xy <- sf::st_coordinates(bg_ll)
#   
#   bg_year <- bg_year %>% mutate(lon = xy[, "X"], lat = xy[, "Y"])
#   
#   readr::write_csv(
#     bg_year %>% transmute(year = year_value,
#                           XKMSQ = .data[[kmsq_col]],
#                           E_c, N_c, lon, lat),
#     out_pts_csv
#   )
#   
#   # Rasterize onto the provided template geometry
#   bg_v <- terra::vect(bg_year, geom = c("E_c", "N_c"), crs = "EPSG:27700")
#   
#   # Ensure template is 1-layer numeric (terra sometimes carries names/types)
#   template_r <- terra::rast(template_r)
#   bg_r <- terra::rasterize(bg_v, template_r, field = 1, fun = "max", background = NA)
#   
#   terra::writeRaster(bg_r, out_raster, overwrite = TRUE)
#   message("Wrote background raster for ", year_value, ": ", out_raster)
#   
#   invisible(bg_r)
# }

gridref_to_en_vec <- function(v) {
  m <- t(vapply(v, gridref_to_en_one, numeric(2)))
  tibble(E = m[,1], N = m[,2])
}

make_bg_and_effort_for_year <- function(year_value,
                                        out_bg_raster, out_bg_pts_csv,
                                        out_eff_raster, out_eff_pts_csv,
                                        template_r) {
  
  # ---- Effort squares (includes Twite squares) ----
  eff_year <- surveyed_eff %>%
    filter(year == year_value) %>%
    distinct(.data[[kmsq_col]], TOTVISITS)
  
  if (nrow(eff_year) == 0) {
    warning("No surveyed squares for year ", year_value)
    return(invisible(NULL))
  }
  
  en_eff <- gridref_to_en_vec(eff_year[[kmsq_col]])
  eff_year <- bind_cols(eff_year, en_eff) %>%
    filter(!is.na(E), !is.na(N)) %>%
    mutate(E_c = E + 500, N_c = N + 500)
  
  eff_sf <- sf::st_as_sf(eff_year, coords = c("E_c", "N_c"), crs = 27700, remove = FALSE)
  eff_ll <- sf::st_transform(eff_sf, 4326)
  xy_eff <- sf::st_coordinates(eff_ll)
  
  eff_out <- eff_year %>%
    mutate(lon = xy_eff[, "X"], lat = xy_eff[, "Y"]) %>%
    transmute(year = year_value, XKMSQ = .data[[kmsq_col]], TOTVISITS, E_c, N_c, lon, lat)
  
  readr::write_csv(eff_out, out_eff_pts_csv)
  
  eff_v <- terra::vect(eff_year, geom = c("E_c", "N_c"), crs = "EPSG:27700")
  eff_r <- terra::rasterize(eff_v, terra::rast(template_r), field = "TOTVISITS", fun = "max", background = NA)
  names(eff_r) <- paste0("Effort_",year_value)
  terra::writeRaster(eff_r, out_eff_raster, overwrite = TRUE)
  message("Wrote effort raster for ", year_value, ": ", out_eff_raster)
  
  # ---- Background squares (surveyed excl. Twite squares) ----
  bg_year <- eff_year %>%
    mutate(year = year_value) %>%
    anti_join(twite_km %>% filter(year == year_value), by = c("year", kmsq_col)) %>%
    distinct(.data[[kmsq_col]], E_c, N_c)
  
  if (nrow(bg_year) == 0) {
    warning("No background squares left after excluding Twite for year ", year_value)
    return(invisible(list(effort = eff_r)))
  }
  
  bg_sf <- sf::st_as_sf(bg_year, coords = c("E_c", "N_c"), crs = 27700, remove = FALSE)
  bg_ll <- sf::st_transform(bg_sf, 4326)
  xy_bg <- sf::st_coordinates(bg_ll)
  
  bg_out <- bg_year %>%
    mutate(lon = xy_bg[, "X"], lat = xy_bg[, "Y"]) %>%
    transmute(year = year_value, XKMSQ = .data[[kmsq_col]], E_c, N_c, lon, lat)
  
  readr::write_csv(bg_out, out_bg_pts_csv)
  
  bg_v <- terra::vect(bg_year, geom = c("E_c", "N_c"), crs = "EPSG:27700")
  bg_r <- terra::rasterize(bg_v, terra::rast(template_r), field = 1, fun = "max", background = NA)
  names(bg_r) <- paste0("BG_",year_value)
  terra::writeRaster(bg_r, out_bg_raster, overwrite = TRUE)
  message("Wrote background raster for ", year_value, ": ", out_bg_raster)
  
  invisible(list(background = bg_r, effort = eff_r))
}
