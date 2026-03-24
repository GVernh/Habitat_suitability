libs <- c("terra", "dplyr", "corrplot")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())

dir.create("./Plots/", showWarnings = FALSE)
dir.create("./Plots/Correlation_plots/", showWarnings = FALSE)

all_files <- list.files(
  "./Data/Processed_data/climate/",
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

clim_files <- all_files[grep("_1999|2010|2024\\.tif$", all_files)]

bn   <- basename(clim_files)
var  <- sub("_.*", "", bn)                          # variable = before first "_"
year <- sub(".*_([0-9]{4})\\.tif$", "\\1", bn)      # year = last 4 digits before .tif
names(clim) <- paste0(var, "_", year)
names(clim)

samp <- spatSample(clim, size = 20000, method = "random", na.rm = TRUE)
colnames(samp) <- sub("_[0-9]{4}$", "", colnames(samp))

samp_df <- as.data.frame(samp)

base_var <- sub("_[0-9]{4}$", "", names(samp_df))

# average across years per variable
clim_mean <- as.data.frame(
  sapply(split.default(samp_df, base_var),
         function(x) rowMeans(as.data.frame(x), na.rm = TRUE))
)


cor_mat <- cor(clim_mean, use = "complete.obs")
round(cor_mat, 2)



# Plot

nice_names <- c(
  tas = "Mean temperature",
  tasmax = "Mean maximum temperature",
  tasmin = "Mean minimum temperature",
  rainfall = "Total precipitation",
  sfcWind = "Mean wind speed",
  groundfrost = "Frost frequency",
  snowLying = "Snow cover duration",
  sun = "Sunshine duration",
  hurs = "Relative humidity",
  psl = "Sea-level pressure",
  pv = "Vapour pressure"
)

names(clim_mean) <- nice_names[names(clim_mean)]

cor_mat <- cor(clim_mean, use = "complete.obs")

# ensure output directory exists
out_dir <- "./Plots/Correlation_plots"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# file path
out_file <- file.path(out_dir, "climate_correlation_matrix.png")

# save plot
png(out_file, width = 2400, height = 2200, res = 300)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.6,
  col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200)
)

dev.off()

#######################
## Climate selection ##
#######################

# tas (Mean temperature)
# rainfall (Total precipitation)
# sfcWind (Mean wind speed)
# groundfrost (Frost frequency)
