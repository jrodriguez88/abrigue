# Run Main Modelacion ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024


# Cargar dependencias
## Load Libraries ####

library(tidyverse)
library(terra)
library(sf)
library(geodata)
library(leaflet)
library(biscale)
library(cowplot)
library(naniar)
library(rsoi)

source("scripts/funciones_abrigue.R")



# Load data Choco
source("scripts/01_datos_ABRIGUE_choco.R")

# Load data Caqueta
source("scripts/01_datos_ABRIGUE_caqueta.R")

# Patrones Climaticos Choco
source("scripts/02_Patrones_climaticos_Choco.R")
mapa_bivar_choco
precipitacion_anual_media <- temp_ppt_choco$ppt
annual_mean_temp <- temp_ppt_choco$temp
temp_ppt <- temp_ppt_choco ; dem <- choco_dem
numero_cluster <- 6 ; paneles <- c(1, 4)
source("scripts/03_kmeans_clima.R")

# Patrones Climaticos Caqueta
source("scripts/02_Patrones_climaticos_Caqueta.R")
mapa_bivar_caqueta
precipitacion_anual_media <- temp_ppt_caqueta$ppt
annual_mean_temp <- temp_ppt_caqueta$temp
temp_ppt <- temp_ppt_caqueta ; dem <- caqueta_dem
numero_cluster <- 6 ; paneles <- c(2, 2)
source("scripts/03_kmeans_clima.R")

source("scripts/05_distribucion_especies_gbif.R")

