# ---- Packages ----
# install.packages(c("terra", "ggplot2", "scales"))
# Optional outline + map adornments:
# install.packages(c("rnaturalearth", "sf", "ggspatial"))

library(terra)
library(ggplot2)
library(scales)

# Optional
library(sf)
library(rnaturalearth)
library(ggspatial)
library(tidyterra)

# ---- Read raster ----
tif_path <- "./Data/Processed_data/climate/hadUK/tas/tas_hadukgrid_uk_1km_MarSepMean_1990.tif"
r <- rast(tif_path)

# If your raster has a "no data" value, terra usually carries it;
# but you can explicitly set NA if needed:
# NAflag(r) <- -9999

# ---- Convert to a data frame for ggplot ----
# as.data.frame(r, xy=TRUE) returns x/y plus one value column
df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df)[3] <- "value"

# ---- Optional: UK outline for context (auto reprojects) ----
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")
# Match raster CRS (important for correct overlay)
uk <- st_transform(uk, crs(r))

# ---- Plot ----
p <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = uk, fill = NA, color = "white", linewidth = 0.3) +
  coord_sf(crs = st_crs(uk), datum = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "transparent",
    name = "°C"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) + labs(x = NULL, y = NULL) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.2, "cm"), pad_y = unit(1.0, "cm"),
    style = north_arrow_fancy_orienteering
  )

print(p)

# ---- Save a nice image ----
ggsave("../For__presentation/Climate_impact_pres/tas_hadukgrid_1990_map.png", p, width = 8, height = 9, dpi = 300)



### Slope ###
# ---- Read raster ----
tif_path <- "./Data/Processed_data/Terrain/1km/slope_mean_1km.tif"
r <- rast(tif_path)

# If your raster has a "no data" value, terra usually carries it;
# but you can explicitly set NA if needed:
# NAflag(r) <- -9999

# ---- Convert to a data frame for ggplot ----
# as.data.frame(r, xy=TRUE) returns x/y plus one value column
df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df)[3] <- "value"

# ---- Optional: UK outline for context (auto reprojects) ----
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")
# Match raster CRS (important for correct overlay)
uk <- st_transform(uk, crs(r))

# ---- Plot ----
p <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = uk, fill = NA, color = "white", linewidth = 0.3) +
  coord_sf(crs = st_crs(uk), datum = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "transparent",
    name = "Degrees"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) + labs(x = NULL, y = NULL) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.2, "cm"), pad_y = unit(1.0, "cm"),
    style = north_arrow_fancy_orienteering
  )

print(p)

# ---- Save a nice image ----
ggsave("../For__presentation/Climate_impact_pres/slope.png", p, width = 8, height = 9, dpi = 300)


#### RAINFALL ####
# ---- Read raster ----
tif_path <- "./Data/Processed_data/climate/hadUK/rainfall/rainfall_hadukgrid_uk_1km_MarSepMean_1990.tif"
r <- rast(tif_path)

# If your raster has a "no data" value, terra usually carries it;
# but you can explicitly set NA if needed:
# NAflag(r) <- -9999

# ---- Convert to a data frame for ggplot ----
# as.data.frame(r, xy=TRUE) returns x/y plus one value column
df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df)[3] <- "value"

# ---- Optional: UK outline for context (auto reprojects) ----
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")
# Match raster CRS (important for correct overlay)
uk <- st_transform(uk, crs(r))

# ---- Plot ----
p <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = uk, fill = NA, color = "white", linewidth = 0.3) +
  coord_sf(crs = st_crs(uk), datum = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "transparent",
    name = "mm"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) + labs(x = NULL, y = NULL) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.2, "cm"), pad_y = unit(1.0, "cm"),
    style = north_arrow_fancy_orienteering
  )

print(p)

# ---- Save a nice image ----
ggsave("../For__presentation/Climate_impact_pres/prec_hadukgrid_1990_map.png", p, width = 8, height = 9, dpi = 300)



#### Distance to road ########
# ---- Read raster ----
tif_path <- "./Data/Processed_data/Roads/1km/road_dist_1km.tif"
r <- rast(tif_path)

lc   <- rast("./Data/Processed_data/CEH_land_cover/LC_1990.tif") # your land cover

# 1) Align land cover to rainfall grid (CRS/resolution/extent)
if (!same.crs(lc, r)) {
  lc <- project(lc, r, method = "near")
}
lc <- resample(lc, r, method = "near")

# 2) Create a binary mask: 1 = land-cover exists, NA = no data
m <- ifel(is.na(lc), NA, 1)

# 3) Crop + mask rainfall
r <- mask(crop(r, m), m)

r <- r/1000
# If your raster has a "no data" value, terra usually carries it;
# but you can explicitly set NA if needed:
# NAflag(r) <- -9999

# ---- Convert to a data frame for ggplot ----
# as.data.frame(r, xy=TRUE) returns x/y plus one value column
df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df)[3] <- "value"

# ---- Optional: UK outline for context (auto reprojects) ----
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")
# Match raster CRS (important for correct overlay)
uk <- st_transform(uk, crs(r))

# ---- Plot ----
p <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = uk, fill = NA, color = "white", linewidth = 0.3) +
  coord_sf(crs = st_crs(uk), datum = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "transparent",
    name = "km"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) + labs(x = NULL, y = NULL) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.2, "cm"), pad_y = unit(1.0, "cm"),
    style = north_arrow_fancy_orienteering
  )

print(p)

# ---- Save a nice image ----
ggsave("../For__presentation/Climate_impact_pres/distance_to_road.png", p, width = 8, height = 9, dpi = 300)



##### Distance to Coast ####
# ---- Read raster ----
tif_path <- "./Data/Processed_data/Coast/1km/coast_dist_1km.tif"
r <- rast(tif_path)

lc   <- rast("./Data/Processed_data/CEH_land_cover/LC_1990.tif") # your land cover

# 1) Align land cover to rainfall grid (CRS/resolution/extent)
if (!same.crs(lc, r)) {
  lc <- project(lc, r, method = "near")
}
lc <- resample(lc, r, method = "near")

# 2) Create a binary mask: 1 = land-cover exists, NA = no data
m <- ifel(is.na(lc), NA, 1)

# 3) Crop + mask rainfall
r <- mask(crop(r, m), m)

r <- r/1000
# If your raster has a "no data" value, terra usually carries it;
# but you can explicitly set NA if needed:
# NAflag(r) <- -9999

# ---- Convert to a data frame for ggplot ----
# as.data.frame(r, xy=TRUE) returns x/y plus one value column
df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
names(df)[3] <- "value"

# ---- Optional: UK outline for context (auto reprojects) ----
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")
# Match raster CRS (important for correct overlay)
uk <- st_transform(uk, crs(r))

# ---- Plot ----
p <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = uk, fill = NA, color = "white", linewidth = 0.3) +
  coord_sf(crs = st_crs(uk), datum = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "transparent",
    name = "km"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) + labs(x = NULL, y = NULL) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.2, "cm"), pad_y = unit(1.0, "cm"),
    style = north_arrow_fancy_orienteering
  )

print(p)

# ---- Save a nice image ----
ggsave("../For__presentation/Climate_impact_pres/distance_to_coast.png", p, width = 8, height = 9, dpi = 300)






### Land cover ####
# --- Paths ---
tif_1990 <- "./Data/Processed_data/CEH_land_cover/LC_1990.tif"
tif_2024 <- "./Data/Processed_data/CEH_land_cover/LC_2024.tif"  # <- update if needed

lc90  <- rast(tif_1990)
lc24  <- rast(tif_2024)

# --- UKCEH palette table (RGB -> hex) ---
ukceh_pal <- data.frame(
  class_code = 1:21,
  class_label = c(
    "Broadleaved woodland","Coniferous Woodland","Arable and Horticulture","Improved Grassland",
    "Neutral Grassland","Calcareous Grassland","Acid grassland","Fen, Marsh and Swamp","Heather",
    "Heather grassland","Bog","Inland Rock","Saltwater","Freshwater","Supralittoral Rock",
    "Supralittoral Sediment","Littoral Rock","Littoral sediment","Saltmarsh","Urban","Suburban"
  ),
  R = c(51,0,240,1,220,255,178,253,128,230,205,210,0,0,152,204,255,255,128,0,128),
  G = c(160,80,228,255,153,192,145,123,26,140,59,210,0,0,125,179,255,255,128,0,128),
  B = c(44,0,66,124,9,55,0,238,128,166,181,255,92,255,183,0,128,128,255,0,128)
)
ukceh_pal$hex <- rgb(ukceh_pal$R, ukceh_pal$G, ukceh_pal$B, maxColorValue = 255)
pal_named <- setNames(ukceh_pal$hex, ukceh_pal$class_label)

# --- Helper: force categorical levels + labels onto a raster ---
apply_ukceh_levels <- function(x, palette_df) {
  x <- as.factor(x)
  
  lev <- levels(x)[[1]]
  code_col <- names(lev)[1]
  
  # Ensure there is a "class_label" column derived from the official table
  lev$class_label <- palette_df$class_label[match(lev[[code_col]], palette_df$class_code)]
  # Put class_label as 2nd col for tidyterra/ggplot friendliness
  lev <- lev[, c(code_col, "class_label")]
  
  levels(x) <- lev
  x
}

lc90 <- apply_ukceh_levels(lc90, ukceh_pal)
lc24 <- apply_ukceh_levels(lc24, ukceh_pal)

# --- Align rasters so they stack and plot cleanly ---
# 1) CRS
if (!terra::same.crs(lc24, lc90)) {
  lc24 <- project(lc24, lc90, method = "near")  # nearest neighbour for classes
}

# 2) Resolution / grid
# resample 2024 onto 1990 grid (or vice versa); choose one "template"
if (!all(res(lc24) == res(lc90)) || !all(dim(lc24)[1:2] == dim(lc90)[1:2])) {
  lc24 <- resample(lc24, lc90, method = "near")
}

# 3) Extent (optional but usually needed)
common_ext <- intersect(ext(lc90), ext(lc24))
lc90 <- crop(lc90, common_ext)
lc24 <- crop(lc24, common_ext)

# --- Stack into 2-layer raster and name layers for faceting ---
lc_both <- c(lc90, lc24)
names(lc_both) <- c("1990", "2024")

# --- Plot side-by-side (facets) with ONE shared legend ---
p <- ggplot() +
  geom_spatraster(data = lc_both) +
  facet_wrap(~lyr, nrow = 1) +
  coord_sf(expand = FALSE) +
  scale_fill_manual(
    values = pal_named,
    name = "Land cover",
    drop = FALSE,              # keep full legend even if some classes absent
    na.value = "transparent"
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.key.height = unit(0.45, "cm")
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p)
ggsave("../For__presentation/Climate_impact_pres/landcover.png", p, width = 12, height = 9, dpi = 300)
