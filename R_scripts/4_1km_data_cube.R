libs <- c("terra")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())


all_files <- list.files(
  "./Data/Processed_data/",
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)
# Template

template_1km <- terra::rast("./Data/Processed_data/template_raster_1km.tif")

#################################
########## 1999 #################
#################################

# 1km climate
tmp_1999 <- all_files[grep("tas_.*_1999\\.tif$", all_files)]
prec_1999 <- all_files[grep("rainfall_.*_1999\\.tif$", all_files)]
wind_1999 <- all_files[grep("sfcWind_.*_1999\\.tif$", all_files)]
groundfrost_1999 <- all_files[grep("groundfrost_.*_1999\\.tif$", all_files)]

# 100m habitat
hab_1999 <- all_files[grep("LC_2000_.*%_habit.*1km\\.tif$", all_files)] # names fine

#################################
########## 2013 #################
#################################

#1km climate
tmp_2013 <- all_files[grep("tas_.*_2013\\.tif$", all_files)]
prec_2013 <- all_files[grep("rainfall_.*_2013\\.tif$", all_files)]
wind_2013 <- all_files[grep("sfcWind_.*_2013\\.tif$", all_files)]
groundfrost_2013 <- all_files[grep("groundfrost_.*_2013\\.tif$", all_files)]

# 100m habitat
hab_2013 <- all_files[grep("LC_2015_.*%_habit.*1km\\.tif$", all_files)] # names fine

#################################
########## Static ###############
#################################
terrain <- all_files[grep("(DTM|slope_mean|aspect|TRI|elev).*1km", all_files)]
distance <- all_files[grep("(road|urban|coast).*1km", all_files)]
lake <- all_files[grep("lake_dist_1km_2ha\\.tif$", all_files)] # Ensure sensitivity test has been run

distance <- c(distance, lake)
#################################
###### Background + Effort ######
#################################

BG_1999 <- all_files[grepl("^surveyed_background_1999.*\\.tif$", basename(all_files))]
effort_1999 <- all_files[grepl("^effort_totvisits_1999.*\\.tif$", basename(all_files))]

BG_2013 <- all_files[grepl("^surveyed_background_2013.*\\.tif$", basename(all_files))]
effort_2013 <- all_files[grepl("^effort_totvisits_2013.*\\.tif$", basename(all_files))]

#################################
######## TWITE OCCURENCE ########
#################################

twite <- read.csv("./Data/Processed_data/twite_100m/twite_presences_1999_2013_lonlat.csv")

# Data cubes ----
################################
###### predictor STACK #########
################################
clim_1999_1km <- terra::rast(c(tmp_1999, prec_1999, wind_1999, groundfrost_1999))
clim_2013_1km <- terra::rast(c(tmp_2013, prec_2013, wind_2013, groundfrost_2013))

eff_1999_1km <- terra::rast(effort_1999)
eff_2013_1km <- terra::rast(effort_2013)
# stacks
pred_1999 <- c(
  clim_1999_1km,
  terra::rast(hab_1999),
  terra::rast(terrain),
  terra::rast(distance),
  eff_1999_1km
)

pred_2013 <- c(
  clim_2013_1km,
  terra::rast(hab_2013),
  terra::rast(terrain),
  terra::rast(distance),
  eff_2013_1km
)

# Twite
# twite_1999 <- subset(twite, year == 1999)
# twite_2013 <- subset(twite, year == 2013)
# 
# pres_1999 <- terra::vect(twite_1999, geom = c("lon","lat"), crs = "EPSG:4326")
# pres_2013 <- terra::vect(twite_2013, geom = c("lon","lat"), crs = "EPSG:4326")
# 
# pres_1999 <- terra::project(pres_1999, terra::crs(template_1km))
# pres_2013 <- terra::project(pres_2013, terra::crs(template_1km))

# Twite
twite_1999 <- subset(twite, year == 1999)
twite_2013 <- subset(twite, year == 2013)

# make presence points in projected CRS
pres_1999_raw <- terra::vect(twite_1999, geom = c("lon","lat"), crs = "EPSG:4326")
pres_2013_raw <- terra::vect(twite_2013, geom = c("lon","lat"), crs = "EPSG:4326")

pres_1999_raw <- terra::project(pres_1999_raw, terra::crs(template_1km))
pres_2013_raw <- terra::project(pres_2013_raw, terra::crs(template_1km))

# collapse to unique occupied 1 km cells
pres_cells_1999 <- unique(terra::cellFromXY(template_1km, terra::crds(pres_1999_raw)))
pres_cells_2013 <- unique(terra::cellFromXY(template_1km, terra::crds(pres_2013_raw)))

pres_cells_1999 <- pres_cells_1999[!is.na(pres_cells_1999)]
pres_cells_2013 <- pres_cells_2013[!is.na(pres_cells_2013)]

# use cell centres as presence points
pres_xy_1999 <- terra::xyFromCell(template_1km, pres_cells_1999)
pres_xy_2013 <- terra::xyFromCell(template_1km, pres_cells_2013)

pres_1999 <- terra::vect(
  data.frame(x = pres_xy_1999[,1], y = pres_xy_1999[,2]),
  geom = c("x","y"),
  crs = terra::crs(template_1km)
)

pres_2013 <- terra::vect(
  data.frame(x = pres_xy_2013[,1], y = pres_xy_2013[,2]),
  geom = c("x","y"),
  crs = terra::crs(template_1km)
)


########## 1999 #########
bg_mask_1999 <- terra::rast(BG_1999)

bg_1999 <- terra::as.points(bg_mask_1999, na.rm = TRUE)
bg_1999_df <- as.data.frame(terra::crds(bg_1999))
names(bg_1999_df) <- c("x", "y")

# 2013
bg_mask_2013 <- terra::rast(BG_2013)

bg_2013 <- terra::as.points(bg_mask_2013, na.rm = TRUE)
bg_2013_df <- as.data.frame(terra::crds(bg_2013))
names(bg_2013_df) <- c("x", "y")

# Extract
pres_vals_1999 <- terra::extract(pred_1999, pres_1999)
bg_vals_1999 <- terra::extract(pred_1999, bg_1999)

pres_vals_2013 <- terra::extract(pred_2013, pres_2013)
bg_vals_2013   <- terra::extract(pred_2013, bg_2013)


# Assemble response df
pres_vals_1999$presence <- 1; bg_vals_1999$presence <- 0
pres_vals_2013$presence <- 1; bg_vals_2013$presence <- 0

pres_vals_1999$year <- 1999; bg_vals_1999$year <- 1999
pres_vals_2013$year <- 2013; bg_vals_2013$year <- 2013

# Remove years from column names
names(pres_vals_1999) <- gsub("_1999", "", names(pres_vals_1999))
names(bg_vals_1999)   <- gsub("_1999", "", names(bg_vals_1999))

names(pres_vals_2013) <- gsub("_2013", "", names(pres_vals_2013))
names(bg_vals_2013)   <- gsub("_2013", "", names(bg_vals_2013))

pres_vals_1999$ID <- NULL
bg_vals_1999$ID <- NULL
pres_vals_2013$ID <- NULL
bg_vals_2013$ID <- NULL

# Create full dataframe
# Create full dataframe
# coordinates from SpatVector objects
xy_pres_1999 <- terra::crds(pres_1999)
xy_bg_1999   <- terra::crds(bg_1999)

xy_pres_2013 <- terra::crds(pres_2013)
xy_bg_2013   <- terra::crds(bg_2013)

# attach coordinates to extracted tables
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
df$year <- factor(df$year) ## This will allow the model to create a seperate intercept per year
df$log_effort <- log1p(df$Effort) # Log transform will behave better given the scew toward 3
df$Effort <- NULL
sapply(df, function(x) length(unique(x)))

# df$`Fen_%_habitat` <- NULL # No fen habitat present in twite occurences

# checks
table(df$presence, df$year)
colSums(is.na(df))

summary(df$log_effort)
table(df$log_effort, useNA = "ifany")

# Correlations

cor_mat <- cor(df[, sapply(df, is.numeric)])

high_cor <- which(abs(cor_mat) > 0.8 & abs(cor_mat) < 1, arr.ind = TRUE)

high_cor_pairs <- data.frame(
  var1 = rownames(cor_mat)[high_cor[,1]],
  var2 = colnames(cor_mat)[high_cor[,2]],
  correlation = cor_mat[high_cor]
)

high_cor_pairs

# Correlation plot

# install if needed
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# make a clean copy
plot_df <- df

# remove response / grouping columns from the correlation plot
plot_df$presence <- NULL
plot_df$year <- NULL

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
  `Fen_%_habitat` = "Fen",
  `Acid grassland_%_habitat` = "Acid grassland",
  `Bog_%_habitat` = "Bog",
  `Heather grassland_%_habitat` = "Heather grassland",
  `Heather_%_habitat` = "Heather",
  aspect_conc = "Aspect",
  DTM_1km = "Elevation",
  TRI = "Ruggedness",
  Elev_RNG = "Elevation range",
  Elev_SD = "SD Elevation",
  Slope_mean = "Slope",
  Coast_dist = "Distance to coast",
  Roads_dist = "Distance to roads",
  Urban_dist = "Distance to urban",
  Lake_dist2ha = "Distance to lakes",
  log_effort = "Log effort"
)

rownames(cor_mat) <- nice_names[rownames(cor_mat)]
colnames(cor_mat) <- nice_names[colnames(cor_mat)]

# output folder
out_dir <- "./Plots/Correlation_plots"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# save png
png(
  filename = file.path(out_dir, "twite_predictor_correlation_matrix_1km.png"),
  width = 2400,
  height = 2200,
  res = 300
)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
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
df$Elev_RNG <- NULL
df$Elev_SD <- NULL
# Write final dataframe
write.csv(df, "./Data/Processed_data/complete_dataset_2013_1999_survey_1km.csv", row.names = F)
