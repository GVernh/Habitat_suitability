libs <- c("terra")

installed_libs <- libs %in% rownames(installed.packages())
if (any(!installed_libs)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())

all_files <- list.files(
  "./Data/Processed_data/",
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

template_100m <- terra::rast("./Data/Processed_data/template_raster_100m.tif")
dir.create("./Data/Processed_data/Complete_datasets/coast_excluded/", showWarnings = FALSE)

# 1999
tmp_1999 <- all_files[grep("tas_.*_1999\\.tif$", all_files)]
prec_1999 <- all_files[grep("rainfall_.*_1999\\.tif$", all_files)]
wind_1999 <- all_files[grep("sfcWind_.*_1999\\.tif$", all_files)]
groundfrost_1999 <- all_files[grep("groundfrost_.*_1999\\.tif$", all_files)]
hab_1999 <- all_files[grep("LC_2000_.*%_habit.*100m\\.tif$", all_files)]
feed_1999 <- all_files[grep("LC_2000_.*dist_to_twite_feeding_.*100m\\.tif$", all_files)]

# 2013
tmp_2013 <- all_files[grep("tas_.*_2013\\.tif$", all_files)]
prec_2013 <- all_files[grep("rainfall_.*_2013\\.tif$", all_files)]
wind_2013 <- all_files[grep("sfcWind_.*_2013\\.tif$", all_files)]
groundfrost_2013 <- all_files[grep("groundfrost_.*_2013\\.tif$", all_files)]
hab_2013 <- all_files[grep("LC_2015_.*%_habit.*100m\\.tif$", all_files)]
feed_2013 <- all_files[grep("LC_2015_.*dist_to_twite_feeding_.*100m\\.tif$", all_files)]

# static
terrain <- all_files[grep("(DTM|slope_mean|aspect|TRI).*100m", all_files)]
distance <- all_files[grep("(road|urban|coast).*100m", all_files)]
lake <- all_files[grep("lake_dist_100m_2ha\\.tif$", all_files)]
distance <- c(distance, lake)

# BG + effort
BG_1999 <- all_files[grepl("^surveyed_background_1999.*\\.tif$", basename(all_files))]
effort_1999 <- all_files[grepl("^effort_totvisits_1999.*\\.tif$", basename(all_files))]
BG_2013 <- all_files[grepl("^surveyed_background_2013.*\\.tif$", basename(all_files))]
effort_2013 <- all_files[grepl("^effort_totvisits_2013.*\\.tif$", basename(all_files))]

twite <- read.csv("./Data/Processed_data/twite_100m/twite_presences_1999_2013_lonlat.csv")

# predictors
clim_1999_1km <- terra::rast(c(tmp_1999, prec_1999, wind_1999, groundfrost_1999))
clim_2013_1km <- terra::rast(c(tmp_2013, prec_2013, wind_2013, groundfrost_2013))

clim_1999_100m <- terra::resample(clim_1999_1km, template_100m, method = "near")
clim_2013_100m <- terra::resample(clim_2013_1km, template_100m, method = "near")

eff_1999_100m <- terra::resample(terra::rast(effort_1999), template_100m, method = "near")
eff_2013_100m <- terra::resample(terra::rast(effort_2013), template_100m, method = "near")

pred_1999 <- c(
  clim_1999_100m,
  terra::rast(hab_1999),
  terra::rast(terrain),
  terra::rast(distance),
  terra::rast(feed_1999),
  eff_1999_100m
)

pred_2013 <- c(
  clim_2013_100m,
  terra::rast(hab_2013),
  terra::rast(terrain),
  terra::rast(distance),
  terra::rast(feed_1999),
  eff_2013_100m
)

twite_1999 <- subset(twite, year == 1999)
twite_2013 <- subset(twite, year == 2013)

pres_1999_all <- terra::vect(twite_1999, geom = c("lon","lat"), crs = "EPSG:4326")
pres_2013_all <- terra::vect(twite_2013, geom = c("lon","lat"), crs = "EPSG:4326")

pres_1999_all <- terra::project(pres_1999_all, terra::crs(template_100m))
pres_2013_all <- terra::project(pres_2013_all, terra::crs(template_100m))

source("./R_scripts/Functions/Strat_sample_BG.R")

buffers <- c(2, 5, 10, 15, 20, 25)

for (buffer_km in buffers) {
  
  message("Running buffer: ", buffer_km, " km")
  
  buffer <- terra::vect(
    paste0("./Data/Processed_data/Coast/Exclusion_zone/coast_buffer_", buffer_km, "km.gpkg")
  )
  buffer <- terra::project(buffer, terra::crs(template_100m))
  
  # filter presences fresh each iteration
  keep_1999 <- !terra::relate(pres_1999_all, buffer, "intersects")[,1]
  keep_2013 <- !terra::relate(pres_2013_all, buffer, "intersects")[,1]
  
  pres_1999 <- pres_1999_all[keep_1999]
  pres_2013 <- pres_2013_all[keep_2013]
  
  twite_1999_filt <- twite_1999[keep_1999, ]
  twite_2013_filt <- twite_2013[keep_2013, ]
  
  # sample background using filtered twite data
  bg_1999_obj <- sample_bg_points_per_square(
    bg_mask_1km = terra::rast(BG_1999),
    template_100m = template_100m,
    twite_df_year = twite_1999_filt,
    max_points_per_square = 3
  )
  
  bg_2013_obj <- sample_bg_points_per_square(
    bg_mask_1km = terra::rast(BG_2013),
    template_100m = template_100m,
    twite_df_year = twite_2013_filt,
    max_points_per_square = 3
  )
  
  bg_1999 <- bg_1999_obj$points
  bg_2013 <- bg_2013_obj$points
  
  bg_keep_1999 <- !terra::relate(bg_1999, buffer, "intersects")[,1]
  bg_keep_2013 <- !terra::relate(bg_2013, buffer, "intersects")[,1]
  
  bg_1999 <- bg_1999[bg_keep_1999]
  bg_2013 <- bg_2013[bg_keep_2013]
  
  # extract raster values
  pres_vals_1999 <- terra::extract(pred_1999, pres_1999)
  bg_vals_1999   <- terra::extract(pred_1999, bg_1999)
  pres_vals_2013 <- terra::extract(pred_2013, pres_2013)
  bg_vals_2013   <- terra::extract(pred_2013, bg_2013)
  
  pres_vals_1999$presence <- 1
  bg_vals_1999$presence <- 0
  pres_vals_2013$presence <- 1
  bg_vals_2013$presence <- 0
  
  pres_vals_1999$year <- 1999
  bg_vals_1999$year <- 1999
  pres_vals_2013$year <- 2013
  bg_vals_2013$year <- 2013
  
  names(pres_vals_1999) <- gsub("_1999", "", names(pres_vals_1999))
  names(bg_vals_1999)   <- gsub("_1999", "", names(bg_vals_1999))
  names(pres_vals_2013) <- gsub("_2013", "", names(pres_vals_2013))
  names(bg_vals_2013)   <- gsub("_2013", "", names(bg_vals_2013))
  
  pres_vals_1999$ID <- NULL
  bg_vals_1999$ID <- NULL
  pres_vals_2013$ID <- NULL
  bg_vals_2013$ID <- NULL
  
  xy_pres_1999 <- terra::crds(pres_1999)
  xy_bg_1999   <- terra::crds(bg_1999)
  xy_pres_2013 <- terra::crds(pres_2013)
  xy_bg_2013   <- terra::crds(bg_2013)
  
  pres_vals_1999$Easting <- xy_pres_1999[,1]
  pres_vals_1999$Northing <- xy_pres_1999[,2]
  bg_vals_1999$Easting <- xy_bg_1999[,1]
  bg_vals_1999$Northing <- xy_bg_1999[,2]
  pres_vals_2013$Easting <- xy_pres_2013[,1]
  pres_vals_2013$Northing <- xy_pres_2013[,2]
  bg_vals_2013$Easting <- xy_bg_2013[,1]
  bg_vals_2013$Northing <- xy_bg_2013[,2]
  
  df <- rbind(pres_vals_1999, bg_vals_1999, pres_vals_2013, bg_vals_2013)
  df <- na.omit(df)
  
  df$year <- factor(df$year)
  df$log_effort <- log1p(df$Effort)
  df$Effort <- NULL
  
  if ("Fen_%_habitat" %in% names(df)) df$`Fen_%_habitat` <- NULL
  if ("TRI" %in% names(df)) df$TRI <- NULL
  
  message("Buffer ", buffer_km, " km: ",
          sum(df$presence == 1), " presences, ",
          sum(df$presence == 0), " background")
  
  write.csv(
    df,
    paste0("./Data/Processed_data/Complete_datasets/coast_excluded/", buffer_km, "km.csv"),
    row.names = FALSE
  )
}







# Correlation plot

# install if needed
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# make a clean copy
plot_df <- df

# remove response / grouping columns from the correlation plot
plot_df$presence <- NULL
plot_df$year <- NULL
plot_df$Northing <- NULL
plot_df$Easting <- NULL
# remove any constant columns just in case
plot_df <- plot_df[, sapply(plot_df, function(x) length(unique(x)) > 1)]

# correlation matrix
cor_mat <- cor(plot_df, use = "complete.obs")

# optional: nicer variable labels
nice_names <- c(
  tas = "Mean temperature",
  rainfall = "Rainfall",
  sfcWind = "Wind speed",
  groundfrost = "Ground frost",
  `Acid grassland_%_habitat` = "% Acid grassland",
  `Bog_%_habitat` = "% Bog",
  `Heather grassland_%_habitat` = "% Heather grassland",
  `Heather_%_habitat` = "% Heather",
  aspect_conc = "Aspect",
  DTM_100 = "Elevation",
  TRI = "Terrain ruggedness (TRI)",
  Slope_mean = "Slope",
  Coast_dist = "Distance to coast",
  Roads_dist = "Distance to roads",
  Urban_dist = "Distance to urban",
  Lake_dist2ha = "Distance to lake",
  log_effort = "Log effort"
)
plot_df <- plot_df[, names(nice_names)]
rownames(cor_mat) <- nice_names[rownames(cor_mat)]
colnames(cor_mat) <- nice_names[colnames(cor_mat)]

# output folder
out_dir <- "./Plots/Correlation_plots"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# save png
png(
  filename = file.path(out_dir, "twite_predictor_correlation_matrix.png"),
  width = 2400,
  height = 2200,
  res = 300
)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "original",
  diag = FALSE,
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.55,
  tl.cex = 0.9,
  col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
  mar = c(0, 0, 2, 0)
)

dev.off()

df$TRI <- NULL # High correlation with slope

# Write final dataframe
write.csv(df, "./Data/Processed_data/complete_dataset_2013_1999_survey.csv", row.names = F)