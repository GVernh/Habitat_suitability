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

template_100m <- terra::rast("./Data/Processed_data/template_raster_100m.tif")

#################################
########## 1999 #################
#################################

# 1km climate
tmp_1999 <- all_files[grep("tas_.*_1999\\.tif$", all_files)]
prec_1999 <- all_files[grep("rainfall_.*_1999\\.tif$", all_files)]
wind_1999 <- all_files[grep("sfcWind_.*_1999\\.tif$", all_files)]
groundfrost_1999 <- all_files[grep("groundfrost_.*_1999\\.tif$", all_files)]

# 100m habitat
hab_1999 <- all_files[grep("LC_2000_.*%_habit.*100m\\.tif$", all_files)] # names fine

#################################
########## 2013 #################
#################################

#1km climate
tmp_2013 <- all_files[grep("tas_.*_2013\\.tif$", all_files)]
prec_2013 <- all_files[grep("rainfall_.*_2013\\.tif$", all_files)]
wind_2013 <- all_files[grep("sfcWind_.*_2013\\.tif$", all_files)]
groundfrost_2013 <- all_files[grep("groundfrost_.*_2013\\.tif$", all_files)]

# 100m habitat
hab_2013 <- all_files[grep("LC_2015_.*%_habit.*100m\\.tif$", all_files)] # names fine

#################################
########## Static ###############
#################################
terrain <- all_files[grep("(DTM|slope_mean|aspect|TRI).*100m", all_files)]
distance <- all_files[grep("(road|urban|coast).*100m", all_files)]

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

clim_1999_100m <- terra::resample(clim_1999_1km, template_100m, method = "near")
clim_2013_100m <- terra::resample(clim_2013_1km, template_100m, method = "near")

eff_1999_100m <- terra::resample(terra::rast(effort_1999), template_100m, method = "near")
eff_2013_100m <- terra::resample(terra::rast(effort_2013), template_100m, method = "near")

# stacks
pred_1999 <- c(
  clim_1999_100m,
  terra::rast(hab_1999),
  terra::rast(terrain),
  terra::rast(distance),
  eff_1999_100m
)

pred_2013 <- c(
  clim_2013_100m,
  terra::rast(hab_2013),
  terra::rast(terrain),
  terra::rast(distance),
  eff_2013_100m
)

rm(list = setdiff(ls(), c("pred_1999", "pred_2013", "twite", "BG_1999", "BG_2013", "template_100m")))

# Twite
twite_1999 <- subset(twite, year == 1999)
twite_2013 <- subset(twite, year == 2013)

pres_1999 <- terra::vect(twite_1999, geom = c("lon","lat"), crs = "EPSG:4326")
pres_2013 <- terra::vect(twite_2013, geom = c("lon","lat"), crs = "EPSG:4326")

pres_1999 <- terra::project(pres_1999, terra::crs(template_100m))
pres_2013 <- terra::project(pres_2013, terra::crs(template_100m))

#################
source("./R_scripts/Functions/Strat_sample_BG.R")

########## 1999 #########
bg_1999_obj <- sample_bg_points_per_square(
  bg_mask_1km = terra::rast(BG_1999),
  template_100m = template_100m,
  twite_df_year = twite_1999,
  max_points_per_square = 3
)

bg_1999 <- bg_1999_obj$points
bg_1999_df <- bg_1999_obj$sampled_table

# Sanity check
pres_v_1999 <- terra::vect(twite_1999, geom = c("lon", "lat"), crs = "EPSG:4326")
pres_v_1999 <- terra::project(pres_v_1999, terra::crs(template_100m))

pres_cells_1999 <- unique(terra::cellFromXY(template_100m, terra::crds(pres_v_1999)))
bg_cells_1999 <- bg_1999_df$cell_100m

length(intersect(pres_cells_1999, bg_cells_1999)) # Should be 0

# Plot
plot(terra::rast(BG_1999))
points(bg_1999, pch = 16, col = "blue", cex = 0.4)
points(pres_v_1999, pch = 16, col = "red", cex = 0.4)

###### 2013 ########

bg_2013_obj <- sample_bg_points_per_square(
  bg_mask_1km = terra::rast(BG_2013),
  template_100m = template_100m,
  twite_df_year = twite_2013,
  max_points_per_square = 3
)

bg_2013 <- bg_2013_obj$points
bg_2013_df <- bg_2013_obj$sampled_table

# Sanity check
pres_v_2013 <- terra::vect(twite_2013, geom = c("lon", "lat"), crs = "EPSG:4326")
pres_v_2013 <- terra::project(pres_v_2013, terra::crs(template_100m))

pres_cells_2013 <- unique(terra::cellFromXY(template_100m, terra::crds(pres_v_2013)))
bg_cells_2013 <- bg_2013_df$cell_100m

length(intersect(pres_cells_2013, bg_cells_2013)) # Should be 0

# Plot
plot(terra::rast(bg_2013))
points(bg_2013, pch = 16, col = "blue", cex = 0.4)
points(pres_v_2013, pch = 16, col = "red", cex = 0.4)

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
df <- rbind(pres_vals_1999, bg_vals_1999, pres_vals_2013, bg_vals_2013)
df <- na.omit(df)
df$year <- factor(df$year) ## This will allow the model to create a seperate intercept per year
df$log_effort <- log1p(df$Effort) # Log transform will behave better given the scew toward 3
df$Effort <- NULL
sapply(df, function(x) length(unique(x)))

df$`Fen_%_habitat` <- NULL # No fen habitat present in twite occurences

# checks
table(df$presence, df$year)
colSums(is.na(df))

summary(df$Effort)
table(df$Effort, useNA = "ifany")

# Lake size sensitivity test

lake_models <- list()
lake_results <- data.frame()
all_files <- list.files(
  "./Data/Processed_data/",
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

lake_files <- all_files[grep("lake_dist.*100m", all_files)]

lake_models <- list()
lake_results <- data.frame()

clean_df_names <- function(x) {
  names(x) <- gsub("_1999", "", names(x))
  names(x) <- gsub("_2013", "", names(x))
  x
}

for (lake_file in lake_files) {
  
  lake_r <- terra::rast(lake_file)
  
  # copy original extracted tables
  p99 <- pres_vals_1999
  b99 <- bg_vals_1999
  p13 <- pres_vals_2013
  b13 <- bg_vals_2013
  
  # add lake values
  p99$lake_dist <- terra::extract(lake_r, pres_1999)[, 2]
  b99$lake_dist <- terra::extract(lake_r, bg_1999)[, 2]
  p13$lake_dist <- terra::extract(lake_r, pres_2013)[, 2]
  b13$lake_dist <- terra::extract(lake_r, bg_2013)[, 2]
  
  # add response + year explicitly
  p99$presence <- 1; b99$presence <- 0
  p13$presence <- 1; b13$presence <- 0
  
  p99$year <- 1999; b99$year <- 1999
  p13$year <- 2013; b13$year <- 2013
  
  # clean names BEFORE binding
  p99 <- clean_df_names(p99)
  b99 <- clean_df_names(b99)
  p13 <- clean_df_names(p13)
  b13 <- clean_df_names(b13)
  
  # drop ID if present
  if ("ID" %in% names(p99)) p99$ID <- NULL
  if ("ID" %in% names(b99)) b99$ID <- NULL
  if ("ID" %in% names(p13)) p13$ID <- NULL
  if ("ID" %in% names(b13)) b13$ID <- NULL
  
  # force same names/order
  common_names <- Reduce(intersect, list(names(p99), names(b99), names(p13), names(b13)))
  p99 <- p99[, common_names, drop = FALSE]
  b99 <- b99[, common_names, drop = FALSE]
  p13 <- p13[, common_names, drop = FALSE]
  b13 <- b13[, common_names, drop = FALSE]
  
  df_test <- rbind(p99, b99, p13, b13)
  df_test <- na.omit(df_test)
  df_test$year <- factor(df_test$year)
  
  m <- glm(presence ~ ., data = df_test, family = binomial)
  
  lake_models[[basename(lake_file)]] <- m
  lake_results <- rbind(
    lake_results,
    data.frame(
      lake_layer = basename(lake_file),
      n = nrow(df_test),
      AIC = AIC(m)
    )
  )
}

# AIC
lake_results[order(lake_results$AIC),]
