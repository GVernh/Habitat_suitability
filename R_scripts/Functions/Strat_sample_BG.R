sample_bg_points_per_square <- function(bg_mask_1km,
                                        template_100m,
                                        twite_df_year,
                                        max_points_per_square = 3) {
  
  # 1) resample 1 km surveyed background mask to 100 m grid
  bg_mask_100m <- terra::resample(bg_mask_1km, template_100m, method = "near")
  
  # 2) valid candidate 100 m cells inside surveyed background squares
  valid_cells <- which(!is.na(terra::values(bg_mask_100m)))
  
  # 3) convert Twite points to SpatVector and get occupied 100 m cells
  pres_v <- terra::vect(twite_df_year, geom = c("lon", "lat"), crs = "EPSG:4326")
  pres_v <- terra::project(pres_v, terra::crs(template_100m))
  
  pres_xy <- terra::crds(pres_v)
  pres_cells <- unique(terra::cellFromXY(template_100m, pres_xy))
  pres_cells <- pres_cells[!is.na(pres_cells)]
  
  # 4) exclude any 100 m cells containing Twite
  candidate_cells <- setdiff(valid_cells, pres_cells)
  
  # 5) get coordinates of candidate 100 m cells
  xy <- terra::xyFromCell(template_100m, candidate_cells)
  
  # 6) assign each 100 m cell to its parent 1 km square
  # use the 1 km mask geometry to define square membership
  parent_square <- terra::cellFromXY(bg_mask_1km, xy)
  
  cand_df <- data.frame(
    cell_100m = candidate_cells,
    x = xy[, 1],
    y = xy[, 2],
    square_1km = parent_square
  )
  
  # remove anything that did not map cleanly to a 1 km square
  cand_df <- cand_df[!is.na(cand_df$square_1km), ]
  
  # 7) sample up to max_points_per_square from each 1 km square
  #set.seed(1)
  sampled_df <- do.call(
    rbind,
    lapply(split(cand_df, cand_df$square_1km), function(d) {
      n_take <- min(nrow(d), max_points_per_square)
      d[sample(seq_len(nrow(d)), size = n_take), ]
    })
  )
  
  # 8) convert sampled cells back to SpatVector points
  bg_points <- terra::vect(
    sampled_df[, c("x", "y")],
    geom = c("x", "y"),
    crs = terra::crs(template_100m)
  )
  
  return(list(
    points = bg_points,
    sampled_table = sampled_df
  ))
}