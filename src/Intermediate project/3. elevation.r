#Code 3 : elevation (for my 5 species)

#------------------------------------------
# 1. Load required packages

library(sf)        # modern spatial data handling (simple features)
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation (maps)
library(ggplot2)   # data visualization

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)

#rds from ecosystem code
matrix_full_eco <- readRDS("matrix_full_eco.rds")

#------------------------------------------
# 2. Load Switzerland boundaries

# Retrieve country borders from Natural Earth
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland")

#------------------------------------------
# 3. Download elevation data

# z controls resolution (higher = more detail but slower)
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)

# Quick visualization of the elevation raster
#plot(elevation_switzerland)

#------------------------------------------
# 4. Prepare sampling points

# We assume your dataset contains:
# - longitude
# - latitude

# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points <- SpatialPoints(
  coords = matrix_full_eco[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84"))

#------------------------------------------
# 5. Extract elevation values

# Extract raster values at each point location
elevation <- raster::extract(elevation_switzerland, spatial_points)

#------------------------------------------
# 6. Add elevation to the dataset

matrix_full_eco_elev <- cbind(matrix_full_eco,elevation)

#in case I need it later (but not used yet)
saveRDS(matrix_full_eco_elev, file = "matrix_full_eco_elev")

#------------------------------------------
# 7. Visualization: elevation distribution

#I don't find this plot very relevant for my study case but I keep it still in case
# Compare elevation distributions of my 5 species together across climate categories
p3 <- ggplot(matrix_full_eco_elev, aes(x = elevation, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

#x11()
#print(p3)


#elevation per species plot (with my 5 species separately)
p4 <- ggplot(matrix_full_eco, aes(x = elevation, color = species)) +
  geom_density(adjust = 3, linewidth = 1) +
  labs(
    title = "Elevation Distribution by Climate and by species",
    x = "Elevation (m)",
    y = "Density",
    fill = "Species"
  ) +
  theme_minimal()

x11()
print(p4)
