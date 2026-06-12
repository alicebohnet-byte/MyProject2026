#Code 4 : climate (for my 5 species)

#---------------------------------------------
# 1) PACKAGES

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)

#from elevation code --> choice of matrix kind of arbritary... :)
#I could also have simply taken my basic matrix_full
matrix_full_eco_elev <- readRDS("matrix_full_eco_elev")

#---------------------------------------------
# 2) STARTING DATASET

# Keep only useful columns and clean data to be sure
species_df <- matrix_full_eco %>%
  select(species, longitude, latitude) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  #create an ID colomn (not yet there)
  mutate(occurrence_id = row_number())

#Check the head of each colomns
head(species_df)

#Check if species present and quantity of data available for each
table(species_df$species)

#---------------------------------------------
# 3) CREATE A SPATIAL OBJECT

# CHELSA requires coordinates. We therefore create a spatial vector
# from the longitude and latitude columns.

pts_v <- terra::vect(
  species_df,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Extract simple coordinates as a standard data frame
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude = y
  ) %>%
  mutate(occurrence_id = species_df$occurrence_id)

coords_df

#---------------------------------------------
# 4) EXTRACT MONTHLY Tmax

#I could not change the dates as some bugs appeared but results seems still coherent with my data
tmax_r <- getChelsa(
  var       = "tasmax",
  coords    = coords_df %>% select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2019-01-01"),
  dataset   = "chelsa-monthly"
)

#Remove the time column with dplyr (tmax_clean)
#Then convert to matrix (tmax_mat)
#With help of chatgpt
tmax_clean <- tmax_r %>%
  select(-time)
colnames(tmax_clean) <- make.unique(colnames(tmax_clean))
tmax_mat <- as.matrix(tmax_clean)
tmax_mean_k <- colMeans(tmax_mat, na.rm = TRUE)
#conversion from kelvin to celsius
tmax_mean_c <- tmax_mean_k - 273.15

#Create a dataframe
tmax_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  tmax_mean_c = as.numeric(tmax_mean_c))

head(tmax_df)

#---------------------------------------------
# 5) EXTRACT PRECIPITATION

#same problem with the dates. Seems coherent again

prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_df %>% select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2019-01-01"),
  dataset   = "chelsa-monthly"
)

#Remove the time column with dplyr, then convert to matrix
prec_clean <- prec_r %>%
  select(-time)
colnames(prec_clean) <- make.unique(colnames(prec_clean))
prec_mat <- as.matrix(prec_clean)
prec_mean <- colMeans(prec_mat, na.rm = TRUE)

#Create a dataframe
prec_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  prec_mean_annual = as.numeric(prec_mean)
)

head(prec_df)

#---------------------------------------------
# 6) MERGE CLIMATE VARIABLES WITH ORIGINIAL DATA

species_climate_df <- species_df %>%
  #+temperature
  left_join(tmax_df, by = "occurrence_id") %>%
  #+precipitation
  left_join(prec_df, by = "occurrence_id")

#Show results
head(species_climate_df)

#Check results
dim(species_df)           # original dimensions
dim(species_climate_df)   # enriched dimensions
names(species_climate_df) # column names after enrichment

matrix_full_eco_elev_clim <- cbind(matrix_full_eco_elev, species_climate_df)
saveRDS(matrix_full_eco_elev_clim, file = "matrix_full_eco_elev_clim")

#---------------------------------------------
# 7) CLIMATE NICHES COMBINED

#install.packages("patchwork") to put two graphs on one page
#I ask chat how to do this patchwork thing
library(patchwork)

#Temperature niche plot
p_temp <- ggplot(
  species_climate_df,
  aes(x = tmax_mean_c, color = species)) +
  geom_density(linewidth = 1) +
  theme_classic() +
  labs(
    title = "Temperature niche",
    x = "Annual mean Tmax (°C)",
    y = "Density",
    color = "Species")

#Precipitation niche plot
p_prec <- ggplot(
  species_climate_df,
  aes(x = prec_mean_annual, color = species)) +
  geom_density(linewidth = 1) +
  theme_classic() +
  labs(
    title = "Precipitation niche",
    x = "Annual precipitation",
    y = "Density",
    color = "Species")

x11()
print(p_temp + p_prec)
