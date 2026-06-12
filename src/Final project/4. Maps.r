#Maps

#I couldn't do the rayshader part because of too many beugs on my MAC

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggrepel)
library(ggiraph)
library(plotly)
library(leaflet)
library(MASS)
library(viridis)
library(viridisLite)
# Packages used only in the final 3D part --> so not used
# They can take more time to install and run.
library(elevatr)
library(raster)
library(terra)
#library(rayshader) #BEUG !!!

# For some spatial operations, disabling s2 avoids geometry issues with simple
# teaching examples. For advanced GIS workflows, this choice should be discussed.
sf::sf_use_s2(FALSE)

# ============================================================
# 1) LOAD AND PREPARE THE DATA
# ============================================================

finalmatrix <- read.csv("data/finalmatrix.csv")
#View(finalmatrix)

# Clean and prepare the table.
# We keep only rows with valid coordinates.
df <- finalmatrix %>%
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    species   = as.factor(species),
    elevation = as.numeric(elevation),
    temp      = as.numeric(temp),
    precip    = as.numeric(precip),
    NDVI      = as.numeric(NDVI)
  ) %>%
  filter(
    is.finite(latitude),
    is.finite(longitude),
    is.finite(elevation)
  )
# Quick summary.
df %>% count(species)

# Convert the occurrence table to an sf object.
# EPSG:4326 is the usual coordinate reference system for longitude/latitude.
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Get Switzerland as an sf polygon from Natural Earth.
switzerland <- rnaturalearth::ne_countries(
  country = "Switzerland",
  scale = "medium",
  returnclass = "sf"
)

# A slightly larger bounding box around Switzerland, useful for plots.
swiss_bbox <- st_bbox(switzerland)

# ============================================================
# 2) BASE R MAPS
# ============================================================
# Base R is useful because it is simple, immediate and does not require much
# syntax. It is a good way to understand the basic elements of a map.

# ------------------------------------------------------------
# 2.1 Very first map: only points
# ------------------------------------------------------------

plot(
  df$longitude, df$latitude,
  xlab = "Longitude", ylab = "Latitude",
  main = "Occurrence points - first base R map",
  pch = 19, col = "darkred"
)

# This is not yet a real map: there is no country border, no background,
# and no spatial context. But it already shows the distribution of the points.

# ------------------------------------------------------------
# 2.2 Add the country border
# ------------------------------------------------------------

plot(
  st_geometry(switzerland),
  col = "grey95", border = "grey40",
  main = "Occurrence points in Switzerland"
)
points(df$longitude, df$latitude, pch = 19, col = "darkred", cex = 0.8)

# ------------------------------------------------------------
# 2.3 Colour points by species
# ------------------------------------------------------------

species_cols <- c(
  "Apis mellifera"   = "red3",
  "Osmia bicornis"   = "royalblue3",
  "Corylus avellana" = "darkgreen",
  "Prunus spinosa"   = "forestgreen",
  "Ranunculus acris" = "goldenrod2"
)
plot(
  st_geometry(switzerland),
  col = "grey95", border = "grey40",
  main = "Base R map - points coloured by species"
)
points(
  df$longitude, df$latitude,
  pch = 19,
  col = species_cols[as.character(df$species)],
  cex = 0.8
)
legend(
  "bottomleft",
  legend = names(species_cols),
  col = species_cols,
  pch = 19,
  bty = "n"
)

# ------------------------------------------------------------
# 2.4 Change point size using an environmental variable
# ------------------------------------------------------------
# Here point size represents elevation.
# We rescale elevation to make the points readable.

point_size <- scales::rescale(df$elevation, to = c(0.5, 2.5))

plot(
  st_geometry(switzerland),
  col = "grey95", border = "grey40",
  main = "Base R map - point size represents elevation"
)
points(
  df$longitude, df$latitude,
  pch = 21,
  bg = species_cols[as.character(df$species)],
  col = "black",
  cex = point_size
)
legend(
  "bottomleft",
  legend = names(species_cols),
  pt.bg = species_cols,
  pch = 21,
  bty = "n"
)

# ============================================================
# 3) GGPLOT2 MAPS
# ============================================================
# ggplot2 is based on layers. This makes it ideal for building maps step by step.

# ------------------------------------------------------------
# 3.1 A basic ggplot2 map
# ------------------------------------------------------------
 
p_gg_basic <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude),
    colour = "darkred",
    size = 2,
    alpha = 0.8
  ) +
  coord_sf(xlim = c(swiss_bbox["xmin"], swiss_bbox["xmax"]),
           ylim = c(swiss_bbox["ymin"], swiss_bbox["ymax"])) +
  labs(
    title = "Basic ggplot2 map",
    subtitle = "Occurrence points plotted on the country border",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)

p_gg_basic

# ------------------------------------------------------------
# 3.2 Colour points by species
# ------------------------------------------------------------

p_gg_species <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude, colour = species),
    size = 2.5,
    alpha = 0.85
  ) +
  scale_colour_manual(values = species_cols) +
  coord_sf() +
  labs(
    title = "ggplot2 map - colour by species",
    colour = "Species",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)

p_gg_species

# ------------------------------------------------------------
# 3.3 Change point size using elevation
# ------------------------------------------------------------

p_gg_elev_size <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude, colour = species, size = elevation),
    alpha = 0.75
  ) +
  scale_colour_manual(values = species_cols) +
  scale_size_continuous(range = c(1, 5)) +
  coord_sf() +
  labs(
    title = "ggplot2 map - point size represents elevation",
    colour = "Species",
    size = "Elevation (m)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)
x11()
print(p_gg_elev_size)

# ------------------------------------------------------------
# 3.4 Use colour for an environmental variable
# ------------------------------------------------------------
# Here the colour represents temperature. Species is shown by point shape.

p_gg_temp <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude, colour = temp, shape = species),
    size = 3,
    alpha = 0.85
  ) +
  scale_colour_viridis_c(option = "plasma") +
  coord_sf() +
  labs(
    title = "ggplot2 map - colour represents temperature",
    colour = "Temperature",
    shape = "Species",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)
x11()
print(p_gg_temp)

# ------------------------------------------------------------
# 3.5 Add density contours around the points
# ------------------------------------------------------------
# Density contours help identify areas where occurrences are concentrated.
# stat_density_2d() computes density directly from the point coordinates.

p_gg_density <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey45") +
  stat_density_2d(
    data = df,
    aes(x = longitude, y = latitude, colour = after_stat(level)),
    linewidth = 0.8,
    bins = 8
  ) +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude, fill = species),
    shape = 21,
    size = 2.4,
    colour = "black",
    alpha = 0.8
  ) +
  scale_fill_manual(values = species_cols) +
  scale_colour_viridis_c(option = "magma") +
  coord_sf() +
  labs(
    title = "ggplot2 map - density contours around occurrence points",
    fill = "Species",
    colour = "Density level",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)
x11()
print(p_gg_density)

# ------------------------------------------------------------
# 3.6 Filled density map
# ------------------------------------------------------------
# This version uses filled density polygons. It starts to look like a spatial
# intensity map.

p_gg_filled_density <- ggplot() +
  geom_sf(data = switzerland, fill = "grey98", colour = "grey40") +
  stat_density_2d_filled(
    data = df,
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    alpha = 0.65,
    bins = 8
  ) +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude, colour = species),
    size = 1.8,
    alpha = 0.8
  ) +
  scale_colour_manual(values = species_cols) +
  coord_sf() +
  labs(
    title = "ggplot2 map - filled density map",
    subtitle = "Density surface + occurrence points",
    colour = "Species",
    fill = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

p_gg_filled_density

# ------------------------------------------------------------
# 3.7 Facet: one map per species
# ------------------------------------------------------------

p_gg_facet <- ggplot() +
  geom_sf(data = switzerland, fill = "grey96", colour = "grey50") +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude, colour = elevation),
    size = 2.2,
    alpha = 0.85
  ) +
  scale_colour_viridis_c(option = "viridis") +
  facet_wrap(~ species) +
  coord_sf() +
  labs(
    title = "ggplot2 map - one panel per species",
    colour = "Elevation (m)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)

p_gg_facet

# ============================================================
# 4) GGIRAPH: INTERACTIVE GGPLOT MAP
# ============================================================
# ggiraph makes ggplot objects interactive. It is useful when we want tooltips
# but still want to keep the grammar of ggplot2.

# Create a tooltip text for each point.
df <- df %>%
  mutate(
    tooltip = paste0(
      "Species: ", species,
      "\nElevation: ", round(elevation, 0), " m",
      "\nTemperature: ", round(temp, 2),
      "\nPrecipitation: ", round(precip, 2),
      "\nNDVI: ", round(NDVI, 3)
    ),
    point_id = paste0("point_", row_number())
  )

p_girafe <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  ggiraph::geom_point_interactive(
    data = df,
    aes(
      x = longitude,
      y = latitude,
      colour = species,
      tooltip = tooltip,
      data_id = point_id
    ),
    size = 1.2,
    alpha = 0.85
  ) +
  scale_colour_manual(values = species_cols) +
  coord_sf() +
  labs(
    title = "ggiraph map - move the mouse over the points",
    colour = "Species"
  ) +
  theme_minimal(base_size = 13)

ggraph <- ggiraph::girafe(ggobj = p_girafe)

#htmlwidgets::saveWidget(
#  ggraph,
#  "ggraph.html",
#  selfcontained = TRUE)

#browseURL("ggraph.html")

#Necessary for my computer to force opening on internet to avoid beugs

# ============================================================
# 5) PLOTLY: INTERACTIVE GGPLOT MAP
# ============================================================
# plotly can transform many ggplot figures into interactive figures.
# It is very convenient for zooming, hovering and exporting interactive HTML.

p_for_plotly <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  geom_point(
    data = df,
    aes(
      x = longitude,
      y = latitude,
      colour = species,
      size = elevation,
      text = tooltip
    ),
    alpha = 0.8
  ) +
  scale_colour_manual(values = species_cols) +
  scale_size_continuous(range = c(1, 4)) +
  coord_sf() +
  labs(
    title = "plotly map - interactive ggplot2 object",
    colour = "Species",
    size = "Elevation (m)"
  ) +
  theme_minimal(base_size = 13)

pplotly <- plotly::ggplotly(p_for_plotly, tooltip = "text")

#htmlwidgets::saveWidget(
#  pplotly,
#  "pplotly.html",
#  selfcontained = TRUE)

#browseURL("pplotly.html")

# ============================================================
# 6) LEAFLET: TRUE WEB MAPS
# ============================================================
# leaflet is different from ggplot2: it creates interactive web maps with tiles,
# zoom, popups and layers.

df <- df %>%
  mutate(
    tooltip = paste0(
      "Species: ", species,
      "<br>Elevation: ", round(elevation,0), " m",
      "<br>Tmax: ", round(temp,2), " °C",
      "<br>Precipitation: ", round(precip,0), " mm",
      "<br>NDVI: ", round(NDVI,3)
    )
  )

# ------------------------------------------------------------
# 6.1 Basic leaflet map
# ------------------------------------------------------------

L1 <- leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # see https://leaflet-extras.github.io/leaflet-providers/preview/ ... Esri.WorldImagery
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 4,
    stroke = TRUE,
    weight = 1,
    color = "black",
    fillColor = "darkred",
    fillOpacity = 0.8,
    popup = ~tooltip
  )

#  htmlwidgets::saveWidget(
#  L1,
#  "L1map.html",
#  selfcontained = TRUE)

#browseURL("L1map.html")

# ------------------------------------------------------------
# 6.2 Leaflet map coloured by species
# ------------------------------------------------------------

pal_species <- colorFactor(
  palette = species_cols,
  domain = df$species
)

L2 <- leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = switzerland,
    fillColor = "transparent",
    color = "grey40",
    weight = 1.2
  ) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = "black",
    weight = 1,
    fillColor = ~pal_species(species),
    fillOpacity = 0.85,
    popup = ~tooltip,
    group = ~species
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_species,
    values = ~species,
    title = "Species"
  ) %>%
  addLayersControl(
    overlayGroups = levels(df$species),
    options = layersControlOptions(collapsed = FALSE)
  )

#htmlwidgets::saveWidget(
#  L2,
#  "L2map.html",
#  selfcontained = TRUE)

#browseURL("L2map.html")


# ------------------------------------------------------------
# 6.3 Leaflet map with elevation colour scale
# ------------------------------------------------------------

pal_elev <- colorNumeric(
  palette = viridis::viridis(100),
  domain = df$elevation
)

L3 <- leaflet(df) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~scales::rescale(elevation, to = c(3, 9)),
    color = "black",
    weight = 0.7,
    fillColor = ~pal_elev(elevation),
    fillOpacity = 0.85,
    popup = ~tooltip
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_elev,
    values = ~elevation,
    title = "Elevation (m)"
  )

#htmlwidgets::saveWidget(L3,
#  "L3map.html",
#  selfcontained = TRUE)

#browseURL("L3map.html")


#RAYSHADER TOO MANY BEUGS --> CAN'T RUN IT

#All the graphs presented here are highly relevant for
#exploring the data or finding specific information on
#a particular point. They will not be included in the
#Summary Panel, particularly because some are interactive,
#but they provide very relevant information for interpreting
#the influence of my environmental variables on the
#distribution of my species.

