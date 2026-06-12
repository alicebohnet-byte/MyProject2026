#Niche comparison — Apis mellifera vs Osmia bicornis
#Graphs coded in this section: finalbiplot / beeoverlap / jaccard / envrecap

library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tidyr)

#Load and View data
finalmatrix <- read.csv("data/finalmatrix.csv")

#PCA biplot with ellipses
#to see if the multivariate niches of the 2 bees are distinct
#and if the associated plants fall within the correct ellipses

pca_data <- finalmatrix %>%
  select(temp, precip, NDVI, elevation)

pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

finalbiplot <- fviz_pca_biplot(
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
x11()
print(finalbiplot)

#PCA interpretation
#
#PC1 (66.1% of variance) mainly reflects variables related to precipitation,
#temperature, and elevation, while PC2 (23.3% of variance) is strongly
#associated with the NDVI gradient (vegetation productivity).
#
#Apis mellifera and Osmia bicornis largely overlap in the PCA space,
#suggesting similar environmental niches at the scale of Switzerland.
#
#Overall, the PCA provides little evidence for environmental niche
#differentiation between the two bee species.

#Niches' overlap (linked to PC1)
pca_scores <- as.data.frame(pca_res$x)
pca_scores$species <- finalmatrix$species

beeoverlap <- ggplot(pca_scores, aes(PC1, color = species)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(title = "Niches' overlap (PC1)")
x11()
print(beeoverlap)

#Intepretation
#This graph visually illustrates, in a different way than ellipses,
#how the niches of my species overlap. Overall, we see that my two bee
#species have very similar curves. My three plant species differ slightly,
#but that's not specifically what my research question is about.


#-------------------------------------------------------------
#Spatial overlap analysis
#For this analysis (not seen in class), I used AI as an help

#This creates a spatial grid (~5km)
resolution <- 0.05 
grid_data <- finalmatrix %>%
  mutate(
    lon_grid = round(longitude / resolution) * resolution,
    lat_grid = round(latitude / resolution) * resolution
  )

#Presence/absence matrix
presence <- grid_data %>%
  select(species, lon_grid, lat_grid) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = species,
    values_from = value,
    values_fill = 0
  )

#Jaccard function
#In conservation statistics, the Jaccard index allows
#to compare similarity and diversity between samples.

jaccard_index <- function(x, y){
  intersection <- sum(x == 1 & y == 1)
  union <- sum(x == 1 | y == 1)
  return(intersection / union)
}

#Overlap calculation
apis_corylus <- jaccard_index(
  presence$`Apis mellifera`,
  presence$`Corylus avellana`
)

osmia_ranunculus <- jaccard_index(
  presence$`Osmia bicornis`,
  presence$`Ranunculus acris`
)

apis_prunus <- jaccard_index(
  presence$`Apis mellifera`,
  presence$`Prunus spinosa`
)

osmia_prunus <- jaccard_index(
  presence$`Osmia bicornis`,
  presence$`Prunus spinosa`
)

osmia_corylus <- jaccard_index(
  presence$`Osmia bicornis`,
  presence$`Corylus avellana`
)

apis_ranunculus <- jaccard_index(
  presence$`Apis mellifera`,
  presence$`Ranunculus acris`
)

#Results (put in a table)

overlap_results <- data.frame(
  Comparison = c(
    "Apis mellifera - Corylus avellana",
    "Osmia bicornis - Ranunculus acris",
    "Apis mellifera - Prunus spinosa",
    "Osmia bicornis - Prunus spinosa",
    "Osmia bicornis - Corylus avellana",
    "Apis mellifera - Ranunculus acris"
  ),
  Jaccard_Overlap = c(
    apis_corylus,
    osmia_ranunculus,
    apis_prunus,
    osmia_prunus,
    osmia_corylus,
    apis_ranunculus
  )
)
print(overlap_results)

#Plot
jaccard <- ggplot(overlap_results,
       aes(x = Comparison,
           y = Jaccard_Overlap)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Spatial overlap between bees and floral resources",
    x = "",
    y = "Jaccard overlap index"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )
x11()
print(jaccard)

#Jaccard index calculation interpretation
#
#The results deosn't totally support my initial hypothesis.
#As expected, Prunus spinosa showed a similar degree of spatial
#overlap with both bee species, suggesting that it may represent
#a shared floral resource. In contrast, Apis mellifera exhibited
#a stronger overlap with Corylus avellana than Osmia bicornis did
#with Ranunculus acris. Contrarly to what I expected, the extremely
#low overlap observed between Osmia bicornis and Ranunculus acris
#suggests that this plant may not be a good spatial indicator of
#the species' niche at the scale of Switzerland. However, Osmia
#bicornis does not appear to have a marked preference for Corylus
#avellana either.
#
#Overall, the analysis highlights that the floral preferences reported
#in the literature I selected do not necessarily translate into spatial
#co-occurrence patterns in my own data. Again, this is a bias I had somewhat
#predicted, so it simply shows that not all results will necessarily be
#very scientifically relevant.

#------------------------------------------------------

#Environmental recap by species
env_long <- finalmatrix %>%
  dplyr::select(species, temp, precip, NDVI, elevation) %>%
  pivot_longer(-species)

envrecap <- ggplot(env_long, aes(x = species, y = value, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7)) +
  labs(title = "Environmental differences by species")
x11()
print(envrecap)

#Those graphs are useful for examining the environmental preferences
#of each of my species individually. Similar graphs, but with curves
#rather than boxplots, have already been created for the intermediate project.