finalmatrix <- read.csv("data/finalmatrix.csv")
View(finalmatrix)

#---------------------------------------------------------------------
#Project idea : Assessing potential niche overlap between a managed
#honey bee (Apis mellifera) and a wild bee species (Osmia bicornis)
#in Switzerland.
#
#This project uses species occurrence data to investigate whether both
#bee species are associated with similar floral resources and geographic areas.
#A strong spatial overlap could suggest a potential for resource competition,
#whereas a limited overlap could indicate niche differentiation.
#
#Species Selection :
#Bee species 
#- Apis mellifera was selected because it is the most common managed
#  pollinator in Switzerland and is frequently discussed in the
#  literature regarding its interactions with wild pollinators.
#
#- Osmia bicornis was selected because it is a widespread and well-
#  studied solitary bee in Europe.
#
#Floral resources 
#Three plant species were selected to represent different levels
#of resource sharing between the two bee species
#
#1. Ranunculus acris
#   - Listed among important pollen sources for Osmia bicornis.
#   - Not identified as a major floral resource in the Agroscope
#     guide for Swiss honey bees.
#   - Used as a resource expected to be more strongly associated
#     with Osmia bicornis.
#
#2. Corylus avellana
#   - Identified by Agroscope as an important early-season pollen
#     source for Apis mellifera in Switzerland.
#   - Not listed among the preferred pollen sources of Osmia
#     bicornis in the consulted literature.
#   - Used as a resource expected to be more strongly associated
#     with Apis mellifera.
#
#3. Prunus spinosa
#   - Appears in both the Osmia bicornis pollen preference list
#     and among important floral resources for honey bees.
#   - Used as a shared resource potentially associated with niche
#     overlap between the two bee species.
#
#Species occurrence records will be obtained from GBIF and iNaturalist
#and will be restricted to Switzerland.
#
#Note : The choice of floral species is partly arbitrary. Even though my
#sources seem quite reliable and that the selected plants are intended
#to represent different levels of expected resource sharing, I imagine
#both bee species may visit all, some, or none of these plants depending
#on local conditions and resource availability.
#The purpose of this choice was to give me a guiding thread for the exercise,
#but not necessarily to demonstrate a scientific reality.
#
#References :
#- Agroscope. (2020). Sources importantes de pollen et de nectar
#pour les abeilles mellifères en Suisse.
#https://ira.agroscope.ch/fr-CH/publication/43823
#- Sedivy, C. (2024). Portrait - Osmia bicornis. Info fauna CSCF.
#https://species.infofauna.ch/groupe/1/portrait/2156
#