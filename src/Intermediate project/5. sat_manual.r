#Code 5 : satellite data (for my 5 species)

#------------------------------------------------
# 1. Load required packages

library(luna)
library(MODIStsp)
library(appeears)
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

#from allmatrixfulltogether code
matrix_full_eco_elev_clim <- readRDS("matrix_full_eco_elev_clim")

#------------------------------------------------
# 2. Export the Switzerland polygon for manual upload in AppEEARS

switzerland_sf <- ne_countries(
  scale = "medium",
  country = "Switzerland",
  returnclass = "sf"
)

dir.create(".data", showWarnings = FALSE)

st_write(
  switzerland_sf,
  "switzerland.geojson",
  delete_dsn = TRUE
)

#x11()
#plot(st_geometry(switzerland_sf), col = "lightgray", main = "Switzerland")

#--------------------------------------------------
# 3. Read the manually downloaded NDVI raster (5 in total)

#Folder containing the 5 downloaded tif files
manual_path <- "./data/appeears_manual_download"

# List all tif files in the folder
manual_tif <- list.files(
  manual_path,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

print(manual_tif)

#Read all the rasters
ndvi_stack <- rast(manual_tif)

#Compute mean NDVI across all tif files
ndvi_raster <- mean(ndvi_stack, na.rm = TRUE)

#Check the raster information
print(ndvi_raster)

#Plot mean raster
#x11()
#plot(ndvi_raster, main = "Mean NDVI raster")

#------------------------------------------------
# 4. Clip the raster to the exact Switzerland border

switzerland_vect <- vect(switzerland_sf)
# Reproject the Switzerland polygon to the raster CRS
switzerland_vect <- project(switzerland_vect, crs(ndvi_raster))
# Crop and mask
ndvi_switzerland <- crop(ndvi_raster, switzerland_vect)
ndvi_switzerland <- mask(ndvi_switzerland, switzerland_vect)

# Plot the clipped raster
#x11()
#plot(ndvi_switzerland, main = "Mean NDVI clipped to Switzerland")

#plot(switzerland_vect, add = TRUE, border = "black", lwd = 1)


#------------------------------------------------
# 5. Convert the sampling table to spatial points

points_vect <- vect(
  matrix_full_eco_elev_clim,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the points to the raster CRS
points_vect <- project(points_vect, crs(ndvi_switzerland))

#x11()
#plot(ndvi_switzerland, main = "Sampling points over NDVI raster")
#plot(points_vect, add = TRUE, col = "red", pch = 16)

#---------------------------------------------------
# 6. Extract NDVI values at point locations

ndvi_values <- terra::extract(ndvi_switzerland, points_vect)

#Check values
head(ndvi_values)

##------------------------------------------------
# 7. Add NDVI values to the original data frame

NDVI = ndvi_values[, 2]

finalmatrix <- cbind(matrix_full_eco_elev_clim, NDVI)

finalmatrixclean <- finalmatrix[, !duplicated(as.data.frame(t(finalmatrix)))]

colnames(finalmatrixclean)[colnames(finalmatrixclean) %in% c("tmax_mean_c")] <- "temp"
colnames(finalmatrixclean)[colnames(finalmatrixclean) %in% c("prec_mean_annual")] <- "precip"

saveRDS(finalmatrixclean, file = "finalmatrix")

write.csv(finalmatrixclean, "~/Desktop/finalmatrix.csv", row.names = FALSE)
#This is the csv file that will be used for the final analysis

#------------------------------------------------
# 8. NDVI niche of the 5 species

x11()
psat <- ggplot(
  finalmatrixclean,
  aes(x = NDVI, color = species)) +
  geom_density(linewidth = 1, adjust = 3) +
  labs(
    title = "NDVI niche of the 5 species (based on summer 2023 data)",
    x = "NDVI",
    y = "Density",
    color = "Species") +
  theme_minimal()
print(psat)

