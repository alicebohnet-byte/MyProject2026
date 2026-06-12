#SummaryPanel

finalmatrix <- read.csv("data/finalmatrix.csv")
#View(finalmatrix)

library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggnewscale)
library(fmsb)
library(cowplot)

library(FactoMineR)
library(factoextra)
library(tidyr)

library(randomForest) 
library(lattice)
library(caret)
library(viridisLite)
library(viridis)

# ---- Working directory and files ----

# Print a quick summary of the dataset dimensions
cat(nrow(finalmatrix), "lignes,", ncol(finalmatrix), "colonnes\n")
# Show the first rows to check that the import worked correctly
head(finalmatrix)

# ============================================================
#  PLOT A – MAP OF SWITZERLAND
#  Background colored by ecosystem type (WTE) + points by species
# ============================================================

# -- Switzerland border --
ch_sf <- ne_countries(country = "Switzerland", scale = "medium",
                      returnclass = "sf")

# -- WTE raster: load it, crop it to Switzerland, and convert it to a table --
wte    <- terra::rast("data/WorldEcosystem.tif")
wte_ch <- terra::crop(wte,    terra::vect(ch_sf))
wte_ch <- terra::mask(wte_ch, terra::vect(ch_sf))

wte_df           <- as.data.frame(wte_ch, xy = TRUE, na.rm = TRUE)
colnames(wte_df) <- c("x", "y", "Value")
wte_df$Value     <- as.numeric(wte_df$Value)

# -- Color palette: extracted directly from the dataset --
# The dataset already contains the hexadecimal color code (column "color")
# and the ecosystem name (column "W_Ecosystm") for each observation.
# We extract unique pairs to build the palette.
palette_wte <- finalmatrix %>%
  dplyr::select(Value = eco_values, W_Ecosystm, color) %>%
  dplyr::distinct()

pal_wte <- setNames(palette_wte$color, palette_wte$W_Ecosystm)

# Join ecosystem names and colors to the raster table
wte_df <- left_join(wte_df, palette_wte, by = "Value")
wte_df$W_Ecosystm <- factor(wte_df$W_Ecosystm, levels = names(pal_wte))

# -- Points: a random seed is set to make the result reproducible --
set.seed(42)
pts_sf <- st_as_sf(finalmatrix, coords = c("longitude", "latitude"), crs = 4326)

# -- Build the plot --
graphA <- ggplot() +

  # WTE raster background (one color per ecosystem)
  geom_raster(data = wte_df,
              aes(x = x, y = y, fill = W_Ecosystm)) +
  scale_fill_manual(values = pal_wte, guide = "none") +

  # new_scale_fill() is needed to use a second fill variable
  # in the same plot (here: point colors by species)
  new_scale_fill() +

  # Observation points
  geom_sf(data = pts_sf,
          aes(fill = species, shape = species),
          size = 2.5, color = "white", stroke = 0.3) +
  scale_fill_manual(values = c(
    "Apis mellifera"   = "#e63946",  # rouge
    "Osmia bicornis"   = "#1d3557",  # bleu foncé
    "Corylus avellana" = "#2d6a4f",  # vert foncé
    "Prunus spinosa"   = "#74c69d",  # vert clair
    "Ranunculus acris" = "#ffbe0b"   # jaune bouton d'or
    ),
                    name = "Species") +
  scale_shape_manual(
  values = c(
    "Apis mellifera"   = 21,
    "Osmia bicornis"   = 24,
    "Corylus avellana" = 22,
    "Prunus spinosa"   = 23,
    "Ranunculus acris" = 25
  ),
                    name = "Species"
) +

  # Switzerland outline
  geom_sf(data = ch_sf, fill = NA, color = "grey30", linewidth = 0.5) +

  # coord_sf() defines the visible geographic area of the map
  coord_sf(xlim = c(5.9, 10.6), ylim = c(45.8, 47.9), expand = FALSE) +

  labs(title = "Location of observations (World Ecosystem background)") +
  theme_void() +  # removes axes and background, useful for maps
  theme(plot.title = element_text(face = "bold", size = 10))

# ============================================================
#  PLOT B – PCA biplot --> 2. PCA and overlap analysis
# ============================================================
#PCA biplot with ellipses
#to see if the multivariate niches of the 2 bees are distinct
#and if the associated plants fall within the correct ellipses

pca_data <- finalmatrix %>%
  dplyr::select(temp, precip, NDVI, elevation)

pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

graphB <- fviz_pca_biplot(
  pca_res,
  geom.ind = "point",
  habillage = finalmatrix$species,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = "Dark1",
  repel = TRUE,
  label = "var",
  col.var = "black",
  arrowsize = 0.8
)

# ============================================================
#  PLOT C – Environmental data recap per species --> 2. PCA and overlap analysis
# ============================================================

env_long <- finalmatrix %>%
  dplyr::select(species, temp, precip, NDVI, elevation) %>%
  pivot_longer(-species)

graphC <- ggplot(env_long, aes(x = species, y = value, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7)) +
  labs(title = "Environmental differences by species")

# ============================================================
#  PLOT D – Variables' importance (machine learning) --> 3. Machinelearning.r
# ============================================================

ml_df <- finalmatrix %>%
  dplyr::select(
    species,
    elevation,
    precip,
    temp,
    NDVI,
    Red,
    Green,
    Blue,
    eco_values,
    Temperatur,
    Moisture,
    Landcover,
    Landforms,
    Climate_Re,
    W_Ecosystm
  )

ml_df <- na.omit(ml_df)

ml_df$species <- as.factor(ml_df$species)

ml_df$Temperatur <- as.factor(ml_df$Temperatur)
ml_df$Moisture   <- as.factor(ml_df$Moisture)
ml_df$Landcover  <- as.factor(ml_df$Landcover)
ml_df$Landforms  <- as.factor(ml_df$Landforms)
ml_df$Climate_Re <- as.factor(ml_df$Climate_Re)
ml_df$W_Ecosystm <- as.factor(ml_df$W_Ecosystm)

str(ml_df)

table(ml_df$species)

set.seed(123)

train_index <- createDataPartition(
  y = ml_df$species,
  p = 0.7,
  list = FALSE
)

train_df <- ml_df[train_index, ]
test_df  <- ml_df[-train_index, ]

table(train_df$species)
table(test_df$species)

rf_species <- randomForest(
  species ~ .,
  data = train_df,
  ntree = 500,
  importance = TRUE
)
#wait a bit

pred_species <- predict(
  rf_species,
  newdata = test_df
)

head(pred_species)

confusionMatrix(
  data = pred_species,
  reference = test_df$species
)

importance(rf_species)

varImpPlot(rf_species)

importance_df <- importance(rf_species) %>%
  as.data.frame()

importance_df$feature <- rownames(importance_df)

importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

graphD <- ggplot(
  importance_df,
  aes(
    x = reorder(feature, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Most important features to discriminate the species",
    x = "Feature",
    y = "Mean decrease in Gini"
  )

# ============================================================
#  COMBINE THE PLOTS WITH COWPLOT
#install.packages("gridGraphics")

figure_finale <- ggdraw() +

  draw_plot(graphA,      x = 0.00, y = 0.60, width = 1.00, height = 0.40) +
  draw_plot(graphB, x = 0.00, y = 0.25, width = 0.45, height = 0.35) +
  draw_plot(graphC,      x = 0.45, y = 0.25, width = 0.55, height = 0.35) +
  draw_plot(graphD,      x = 0.00, y = 0.00, width = 1.00, height = 0.25) +

  draw_label("A", x = 0.02, y = 0.99, fontface = "bold", size = 14) +
  draw_label("B", x = 0.02, y = 0.59, fontface = "bold", size = 14) +
  draw_label("C", x = 0.46, y = 0.59, fontface = "bold", size = 14) +
  draw_label("D", x = 0.02, y = 0.24, fontface = "bold", size = 14)

x11()
print(figure_finale)


#Conclusion :
#We can draw several conclusions from the data obtained in this summary panel.
#I wanted to assess the niches of my two bee species, one domesticated and the
#other wild, to see if they overlapped. We can clearly see on the distribution
#map, the biplot, and the environmental preferences that they correspond to a
#very similar niche. The plant species I chose also have niches that overlap
#with those of my bees, without clear differentiation between species, thus
#rejecting my hypothesis that Apis mellifera would have a stronger preference
#for Corylus avellana, Osmia bicornis for Ranunculus acris, or both for Prunus
#spinosa. The Jaccard index I calculated also confirms this. The choice of
#floral species was therefore probably not the right one or not very relevant
#for this type of analysis. It can also be noted that Ranunculus acris seems to
#have a broader niche and tolerance with respect to the analyzed environmental
#factors, but this falls outside the scope of my research question.
#
#In conclusion, yes, Apis mellifera and Osmia bicornis occupy a similar niche
#and likely exploit similar resources. From a conservation standpoint, this
#suggests competition between the two species, especially given the declining
#number of areas with sufficient flowering resources in Switzerland. This
#highlights the tension between conserving wild bee species and allowing
#human beekeeping to continue.
#
#
#Note: I declare that I used ChatGPT (OpenAI, 2026) for debugging my code and
#to help me generate some graphs. I also used Google Translate to help me
#translate my texts from French to English.