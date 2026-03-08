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

############# chat GPT

# List rasters
LC_files <- list.files(
  path = "./Data/CEH_land_cover/",
  recursive = TRUE,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Lookup table
lut <- data.frame(
  value = 1:21,
  class = c(
    "Deciduous woodland","Coniferous woodland","Arable","Improve grassland","Neutral grassland",
    "Calcareous grassland","Acid grassland","Fen","Heather","Heather grassland","Bog","Inland rock",
    "Saltwater","Freshwater","Supralittoral rock","Supralittoral sediment","Littoral rock","Littoral sediment",
    "Saltmarsh","Urban","Suburban"
  )
)

# template raster
temp <- rast(LC_files[[1]])
temp <- temp[[1]]
values(temp) <- NA
temp

writeRaster(
  temp,
  "./Data/Processed_data/template_raster_25.tif",
  overwrite = TRUE
)

temp_100 <- terra::aggregate(temp, fact = 4)

writeRaster(
  temp_100,
  "./Data/Processed_data/template_raster_100m.tif",
  overwrite = TRUE
)

temp_1km <- terra::aggregate(temp, fact = 40)

writeRaster(
  temp_1km,
  "./Data/Processed_data/template_raster_1km.tif",
  overwrite = TRUE
)

out_dir <- "./Data/Processed_data/CEH_land_cover/"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

######### WORKSHOP

for (f in LC_files) {
  
  r <- terra::rast(f)
  r <- r[[1]]   # Land cover data is stored in the first layer
  
  # extract year from folder name (…/CEH_land_cover/1990/…)
  year <- sub(".*CEH_land_cover/([0-9]{4})/.*", "\\1", f)
  
  # Harmonise 2007 scheme
  if (year == 2007) {
    
    rcl <- rbind(
      c(1,1),
      c(2,2),
      c(3,3),
      c(4,4),
      c(5,5),
      c(6,  5),
      c(7,  6),
      c(8, 7),
      c(9,  8),
      c(10,  9),
      c(11, 10),
      c(12, 11),
      c(13, 12),
      c(14, 12),
      c(15, 13),
      c(16, 14),
      c(17, 15),
      c(18, 16),
      c(19, 17),
      c(20, 18),
      c(21, 19),
      c(22, 20),
      c(23, 21)
    )
    
    r <- classify(r, rcl, others = NA)
  }
  
  # Check dimensions
  same_geom <- terra::compareGeom(
    r, temp,
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE,
    stopOnError = FALSE
  )
  
  cat(basename(f), ":", same_geom, "\n")
  
  # Harmonise 2000 CEH scheme
  if (year == 2000) {
    
    r <- resample(r, temp, method = "near")
    rcl <- rbind(
      c(11,  1),  # broadleaf -> Deciduous woodland
      c(21,  2),  # conifer -> Coniferous woodland
      c(41,  3),  # arable cereals -> Arable
      c(42,  3),  # horticulture -> Arable
      c(43,  3),  # non-rotational -> Arable
      c(51, 4),
      c(52, 4),
      c(61, 5),
      c(71, 6),
      c(81, 7),
      c(91, 10),
      c(101, 9),
      c(102, 10),
      c(111, 8),
      c(121, 11),
      c(131, 14),
      c(151, 12),
      c(161, 12),
      c(171, 21),
      c(172, 20),
      c(181, 15),
      c(191, 16),
      c(201, 17),
      c(211, 18),
      c(212, 19),
      c(221, 13)
    )
    
    r <- classify(r, rcl, others = NA)
    
    # Check dimensions
    same_geom <- terra::compareGeom(
      r, temp,
      crs = TRUE,
      ext = TRUE,
      rowcol = TRUE,
      res = TRUE,
      stopOnError = FALSE
    )
    
    cat(basename(f), ":", same_geom, "\n")

  }
  
  # attach labels
  levels(r) <- lut

  out_file <- file.path(out_dir, paste0("LC_", year, ".tif"))
  
  terra::writeRaster(r, out_file, overwrite = TRUE, datatype = "INT1U")
}

#####################
#########################################################
######################

# TEST STACK
test <- list.files(
  path = "./Data/Processed_data/CEH_land_cover/",
  recursive = TRUE,
  pattern = "\\.tif$",
  full.names = TRUE
)

x<- rast(test)
unique(x[[4]]) # Check this after a rerun of the code tomorrow. Unclassified should not be assigned to 1

# TO DO: 
# some of the rasters have unclassified values, need to assign these as NA
