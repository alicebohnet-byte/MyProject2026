#Alice Bohnet's project : two bee species and three plants niche analysis
#Final project

#Library
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

library(tidyverse)
library(ggrepel)
library(ggiraph)
library(plotly)
library(leaflet)
library(MASS)

#To avoid function name conflicts between dplyr and raster packages
library(dplyr)
select <- dplyr::select
filter <- dplyr::filter
lag <- dplyr::lag
###################################

#Intermediate project sources --> leading to final matrix 
source("./src/Intermediate project/1. allmatrixfulltogether.r")

source("./src/Intermediate project/2. ecosystem.r")

source("./src/Intermediate project/3. elevation.r")

source("./src/Intermediate project/4. climate.r")

source("./src/Intermediate project/5. sat_manual.r")

#Final project --> analysis
source("./src/Final project/1. Read matrix and ecoquestion.r")

source("./src/Final project/2. PCA and overlap analysis.r")

source("./src/Final project/3. Machinelearing.r")

source("./src/Final project/4. Maps.r")

source("./src/Final project/5. SummaryPanel.r")
