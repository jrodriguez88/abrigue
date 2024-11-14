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

source("scripts/funciones_abrigue.R")




# Load data Choco
source("scripts/01_datos_ABRIGUE_choco.R")

# Load data Caqueta
source("scripts/01_datos_ABRIGUE_caqueta.R")

# Patrones Climaticos Choco
source("scripts/02_Patrones_climaticos_Choco.R")
mapa_bivar_choco

# Patrones Climaticos Caqueta
source("scripts/02_Patrones_climaticos_Caqueta.R")
mapa_bivar_caqueta

source("scripts/05_distribucion_especies_gbif.R")

