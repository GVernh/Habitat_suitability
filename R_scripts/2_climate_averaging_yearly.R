libs <- c("terra")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))

rm(list = ls())

in_dir  <- "./Data/Climate/Climate/hadUK/hadukgrid_1km_mon_raw"
out_dir <- "./Data/Processed_data/climate/hadUK"
dir.create("./Data/Processed_data/climate/", showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE)

vars  <- c("groundfrost","hurs","psl","pv","rainfall","sfcWind",
           "snowLying","sun","tas","tasmax","tasmin")
years <- 1990:2024
temp <- terra::rast("./Data/Processed_data/template_raster_25.tif")
# temp1km <- terra::rast(ext(temp), crs = crs(temp), resolution = 1000)
temp1km <- terra::rast("./Data/Processed_data/template_raster_1km.tif")

for (v in vars) {
  dir.create(file.path(out_dir, v), showWarnings = FALSE, recursive = TRUE)
  
  for (y in years) {
    f <- file.path(in_dir, v, sprintf("%s_hadukgrid_uk_1km_mon_%d01-%d12.nc", v, y, y))
    if (!file.exists(f)) {
      message("Missing: ", f)
      next
    }
    
    r <- terra::rast(f)           # should be 12 layers (months)
    r_ms <- r[[3:9]]       # Mar(3) ... Sep(9)
    m <- mean(r_ms)        # mean across those months
    
    
    m <- ifel(is.nan(m), NA, m) # Change NaN to NA
    crs(m) <- "EPSG:27700" # Standardise crs just in case
    m <- terra::project(m, temp1km, method = "bilinear")
    names(m) <- paste0(v, "_", y)
    
    out <- file.path(out_dir, v, sprintf("%s_hadukgrid_uk_1km_MarSepMean_%d.tif", v, y))
    
    terra::writeRaster(m, out, overwrite = TRUE)
    message("Wrote: ", out)
  }
}

rm(list=setdiff(ls(), c("temp1km")))

###### climatologies #######

in_dir  <- "./Data/Climate/Climate/CHELSA_CLIMATOLOGY/"
out_dir <- "./Data/Processed_data/climate/CHELSA_CLIMATOLOGY/"
dir.create(out_dir, showWarnings = FALSE)

rast <- list.files(path = in_dir,
           pattern = ".tif$",
           recursive = F,
           full.names = TRUE)

for (j in rast) {
  
  r <- terra::rast(j)
  r <- terra::project(r, temp1km, method = "bilinear")
  r_crop <- terra::crop(r, temp1km)
  name <- names(r_crop)
  
  out <- file.path(out_dir, name)
  terra::writeRaster(r_crop, paste0(out,".tif"), overwrite = TRUE)
  message("Wrote: ", out)
}
