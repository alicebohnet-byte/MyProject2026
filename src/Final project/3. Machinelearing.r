#Machine learning
#Graphs coded in this section: importance / p (to run in the loop)

#Packages
# randomForest:
# Used to train the Random Forest model.
library(randomForest) ## one of the most popular R packages for Random Forest modeling a basic ML algorithm that can be used for classification and regression tasks. 

# caret:
# Used here for train/test split and confusion matrix.
library(caret)
library(lattice)
# dplyr:
# Used for data manipulation: select, mutate, arrange, etc.
library(dplyr)

# ggplot2:
# Used to create plots and maps.
library(ggplot2)

# viridis:
# Used for nice continuous colour scales.
library(viridisLite)
library(viridis)

############################################################
# 1) Import occurrence data
############################################################

# This table contains:
# - species names
# - latitude and longitude
# - environmental predictors extracted at each occurrence point

finalmatrix <- read.csv("data/finalmatrix.csv")
#View(finalmatrix)

# Basic inspection of the data
head(finalmatrix)
str(finalmatrix)
names(finalmatrix)

# Number of occurrences per species
table(finalmatrix$species)


############################################################
# 2) Import environmental prediction grid
############################################################

# This table represents a regular environmental grid over Switzerland.
#
# Each row is one geographic point.
# Each point has environmental predictors:
# elevation, precipitation, temperature, NDVI, land cover, etc.
#
# In a real ecological study, this grid should come from real raster layers.
# For this teaching exercise, we use a pre-prepared CSV file.

grid_pred <- read.csv("data/fake_grid_switzerland.csv")

head(grid_pred)
str(grid_pred)

############################################################
# 3) First map of occurrence points
############################################################

# This first plot simply shows where the occurrence points are located.

ggplot(finalmatrix, aes(x = longitude, y = latitude, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  coord_equal() +
  theme_classic() +
  labs(
    title = "Occurrence points of the 5 species",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )

############################################################
# 4) Prepare occurrence data for machine learning
############################################################

# We create a clean table for the Random Forest model.
#
# The response variable is:
# - species
#
# The predictor variables are:
# - elevation
# - precip
# - temp
# - NDVI
# - Red, Green, Blue
# - eco_values
# - categorical environmental variables

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

# Remove missing values.
# Random Forest cannot use rows with NA values.
ml_df <- na.omit(ml_df)

# Convert the response variable to a factor.
# This tells R that species is a categorical variable.
ml_df$species <- as.factor(ml_df$species)

# Convert categorical predictors to factors.
# Random Forest can use factors as categorical predictors.
ml_df$Temperatur <- as.factor(ml_df$Temperatur)
ml_df$Moisture   <- as.factor(ml_df$Moisture)
ml_df$Landcover  <- as.factor(ml_df$Landcover)
ml_df$Landforms  <- as.factor(ml_df$Landforms)
ml_df$Climate_Re <- as.factor(ml_df$Climate_Re)
ml_df$W_Ecosystm <- as.factor(ml_df$W_Ecosystm)

# Check the final structure
str(ml_df)

# Check the number of samples per species
table(ml_df$species)


############################################################
# 5) Train / test split
############################################################

# We split the data into:
# - 70% training data
# - 30% testing data
#
# The training data are used to build the model.
# The testing data are used to evaluate the model on unseen data.

set.seed(123)

train_index <- createDataPartition(
  y = ml_df$species,
  p = 0.7,
  list = FALSE
)

train_df <- ml_df[train_index, ]
test_df  <- ml_df[-train_index, ]

# Check that both species are present in both datasets
table(train_df$species)
table(test_df$species)


############################################################
# 6) Train the Random Forest model
############################################################

# The formula species ~ . means:
# predict species using all other columns as predictors.
#
# ntree = 500 means that the forest contains 500 trees.
#
# importance = TRUE allows us to calculate variable importance.

rf_species <- randomForest(
  species ~ .,
  data = train_df,
  ntree = 500,
  importance = TRUE
)

print(rf_species)

############################################################
# 7) Prediction on test data
############################################################

# We now ask the model to predict the species
# of the test dataset.

pred_species <- predict(
  rf_species,
  newdata = test_df
)

head(pred_species)

############################################################
# 8) Model evaluation
############################################################

# The confusion matrix compares:
# - predicted species
# - observed species
#
# It gives an estimate of model performance.

confusionMatrix(
  data = pred_species,
  reference = test_df$species
)

############################################################
# 9) Feature importance
############################################################

# Random Forest can estimate which variables are most useful
# for discriminating the species.

importance(rf_species)

# Basic Random Forest importance plot
varImpPlot(rf_species)

# Create a cleaner ggplot version

importance_df <- importance(rf_species) %>%
  as.data.frame()

importance_df$feature <- rownames(importance_df)

importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

importance <- ggplot(
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
x11()
print(importance)

#Interpretation 
#Here we see that the variable that most distinguishes my species
#is NDVI, closely followed by average temperatures, elevation,
#and average precipitations. The other factors that follow#
#seem much less impactful, but I imagine the graph is also
#somewhat skewed since those last factors don't have quantitative values.

############################################################
# 10) Prepare the prediction grid
############################################################

# The prediction grid must contain the same predictor columns
# as the training data.
# It must NOT contain the response variable species,
# because this is what we want to predict.

grid_ml <- grid_pred %>%
  select(
    longitude,
    latitude,
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

# Convert categorical grid variables to factors.
# Important:
# The factor levels must be exactly the same as in the training data.
# Otherwise, R may not be able to use the Random Forest model.

grid_ml$Temperatur <- factor(
  grid_ml$Temperatur,
  levels = levels(train_df$Temperatur)
)

grid_ml$Moisture <- factor(
  grid_ml$Moisture,
  levels = levels(train_df$Moisture)
)

grid_ml$Landcover <- factor(
  grid_ml$Landcover,
  levels = levels(train_df$Landcover)
)

grid_ml$Landforms <- factor(
  grid_ml$Landforms,
  levels = levels(train_df$Landforms)
)

grid_ml$Climate_Re <- factor(
  grid_ml$Climate_Re,
  levels = levels(train_df$Climate_Re)
)

grid_ml$W_Ecosystm <- factor(
  grid_ml$W_Ecosystm,
  levels = levels(train_df$W_Ecosystm)
)

# Remove rows with missing values.
# Missing values may appear if some categories in the grid
# are not present in the training data.

grid_ml <- na.omit(grid_ml)

str(grid_ml)


############################################################
# 11) Predict species probabilities on the grid
############################################################

# type = "prob" asks the model to return probabilities
# instead of only the most likely class.
#
# The output contains one column per species.

grid_prob <- predict(
  rf_species,
  newdata = grid_ml,
  type = "prob"
)

head(grid_prob)

# Combine coordinates, predictors and probabilities in one table.

grid_map <- cbind(grid_ml, grid_prob)

head(grid_map)

############################################################
# Probability maps for each species
############################################################

# Check species names predicted by the model
colnames(grid_prob)

# Use exactly the same names as the columns of grid_prob/grid_map
target_species <- c(
  "Apis mellifera",
  "Osmia bicornis",
  "Corylus avellana",
  "Ranunculus acris",
  "Prunus spinosa"
)

for(sp in target_species){

x11()

  p <- ggplot(grid_map,
              aes(x = longitude,
                  y = latitude)) +

    # Predicted probability
    geom_tile(
      aes(fill = .data[[sp]])
    ) +

    # Observed occurrences of the focal species
    geom_point(
      data = subset(finalmatrix,
                    species == sp),
      aes(
        x = longitude,
        y = latitude
      ),
      inherit.aes = FALSE,
      color = "black",
      size = 1,
      alpha = 0.7
    ) +

    scale_fill_viridis_c(
      limits = c(0,1),
      name = "Probability"
    ) +

    coord_equal() +
    theme_classic() +

    labs(
      title = paste("Predicted distribution of", sp),
      subtitle = "Random Forest prediction over Switzerland",
      x = "Longitude",
      y = "Latitude"
    )

  print(p)

}

#Interpretation
#These graphs show a prediction of the occurrence of each of my
#species according to the fake_grid provided on Moodle.
#The resulting prediction data mostly doesn't match my actual occurrence data
#(GBIF and Inat). The only species where the colors seem more or less
#consistent is for the grid linked to Apis Mellifera. I think this
#is the case in particular because it's the species for which I have
#the most data (2175, compared to between 266 and 889 data points for
#my other species). Also, in this case, I based my prediction on a
#pre-prepared CSV file for the fake_grid, which doesn't necessarily
#correspond to the appropriate ecological context.