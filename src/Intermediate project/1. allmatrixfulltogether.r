#Code 1 : matrix_full (with the 5 species together)
#GBIF + iNaturalist occurences

#----------------------------------
# 1) PACKAGES

#install.packages(c("rgbif", "rnaturalearth",
#"ggplot2", "rinat", "raster", "dplyr", "sf"))

library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects

#Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)

#-------------------------------------
# 2) USER PARAMETERS

# Species of interest (two bees & 3 plants)
species_list <- c(
  "Apis mellifera",
  "Osmia bicornis",
  "Corylus avellana",
  "Ranunculus acris",
  "Prunus spinosa"
)

# Maximum number of GBIF records to download
gbif_limit <- 5000

# Time filtering period
date_start <- as.Date("2020-01-01")
date_end   <- as.Date("2026-01-01")

# Simplified geographic extent for Switzerland
xmin <- 6
xmax <- 11
ymin <- 46
ymax <- 48

#--------------------------------------
# 3) BASE MAP: SWITZERLAND

# Download the outline of Switzerland
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland")

# Simple visualization of the map
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  theme_classic()

#------------------------------------
#4) DOWNLOAD GBIF DATA
#(with help and comments of ChatGPT to help me do the loop)

#list to store data for each species
gbif_list <- list()

# Loop over each species in the list
for(sp in species_list){

  # Print progress in console (useful for debugging and tracking)
  cat("Downloading GBIF data for:", sp, "\n")

  # Download occurrence data for the current species
tmp <- occ_data(
  scientificName = sp,
  hasCoordinate = TRUE,
  country = "CH", 
  limit = gbif_limit
)$data
  # Filter and format the data
  tmp2 <- tmp %>%

    # Keep only observations in Switzerland
    filter(country == "Switzerland") %>%

    # Create/modify columns
    mutate(
      species = sp,                     # ensure species name is consistent
      date_obs = as.Date(eventDate),    # convert to Date format
      source = "gbif"                   # identify data source
    ) %>%

    # Keep only relevant columns and rename them
    select(
      species,
      latitude = decimalLatitude,
      longitude = decimalLongitude,
      date_obs,
      source
    )

  # Store the cleaned dataset in the list
  gbif_list[[sp]] <- tmp2
}

# Combine all species datasets into a single table
data_gbif <- bind_rows(gbif_list)

#map with gbif data only
#ggplot(data = Switzerland) +
#  geom_sf(fill = "grey95", color = "black") +
#  geom_point(
#    data = data_gbif,
#    aes(x = longitude, y = latitude, fill = species),
#    size = 2,
#    shape = 21,
#    color = "black") +
#  ggtitle("GBIF occurrences (all species)") +
#  theme_classic()

#--------------------------------------------
# 5) DOWNLOAD iNaturalist DATA
#(with help and comments of ChatGPT to help me do the loop)

#list to store data for each species
inat_list <- list()

# Loop over species
for(sp in species_list){

  cat("Downloading iNat data for:", sp, "\n")

  # Query iNaturalist API
  tmp <- get_inat_obs(
    query = sp,
    place_id = "switzerland"
  )

  # Format the dataset
  tmp2 <- data.frame(
    species   = sp,                          # assign species name
    latitude  = tmp$latitude,
    longitude = tmp$longitude,
    date_obs  = as.Date(tmp$observed_on),    # convert date
    source    = "inat"                       # identify source
  )

  # Store result
  inat_list[[sp]] <- tmp2
}

# Merge all species into one dataset
data_inat <- bind_rows(inat_list)

#map with inat data only
#ggplot(data = Switzerland) +
#  geom_sf(fill = "grey95", color = "black") +
#  geom_point(
#    data = data_inat,
#    aes(x = longitude, y = latitude, fill = species),
#    size = 2,
#    shape = 21,
#    color = "black") +
#  ggtitle("iNaturalist occurrences (all species)") +
#  theme_classic()

#---------------------------------------------------------
# 6) MERGE THE TWO DATABASES

#I use bind_rows and not merge, as explained in the lab
matrix_fullnotclean <- bind_rows(data_gbif, data_inat)

matrix_full <- matrix_fullnotclean %>%
  select(species, longitude, latitude) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  #create an ID colomn (not yet there)
  mutate(occurrence_id = row_number())

#save as rds file so that I can directly read it later in my further codes
saveRDS(matrix_full, file = "matrix_full.rds")

#----------------------------------------------------------
# 7) TIME FILTERING BETWEEN TWO DATES

# Keep only observations within the selected time interval
#matrix_full_date <- matrix_full %>%
#  filter(!is.na(date_obs)) %>%
#  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
#head(matrix_full_date)
#summary(matrix_full_date$date_obs)
#table(matrix_full_date$source)

#------------------------------------------------------------
# 8) MAP OF COMBINED DATA

#map with gbif + inat data of the 5 species together
matrixfullmap <-
  ggplot(data = Switzerland) +
    geom_sf(fill = "grey95", color = "black") +
    geom_point(data = matrix_full,
    aes(x = longitude, y = latitude, fill = species),
    size = 1,
    shape = 21,
    color = "black",
    alpha = 0.8) +
    theme_classic()

x11()
print(matrixfullmap)

