## Consolidacion de bases de datos modelacion proyecto ABRIGUE ####
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024


## Load Libraries ####

# library(tidyverse)
# library(terra)
# library(sf)
# library(geodata)
# library(leaflet)



## Shapefiles - Limites territoriales ####
# Fuente: https://www.colombiaenmapas.gov.co/ 
# Líneas limítrofes de las entidades territoriales de Colombia

departamentos <- vect("data/spatial/Departamento.shp")
municipios <- vect("data/spatial/Municipio, Distrito y Área no municipalizada.shp")

choco_shp <- departamentos[departamentos$DeNombre == "Chocó"]
choco_municipios_shp <- municipios[municipios$Depto == "Chocó"]
abrigue_municipios_choco <- choco_municipios_shp[choco_municipios_shp$MpNombre %in% 
                                                   c( "Bahía Solano", "Nuquí", "Juradó")]

## Digital Elevation Model #### 
# For TPS interpolation

DEM <- geodata::elevation_30s(country = "COL", path=tempdir())
choco_dem <- crop_raster(DEM, choco_shp)


# Estaciones IDEAM #### 
# Fuente: http://dhime.ideam.gov.co/atencionciudadano/


## Precipitacion Mensual Historica

files_prec_choco <- list.files("data/ideam/precipitacion/choco/", recursive = T, full.names = T, pattern = ".csv$")

ideam_prec_choco <- map(files_prec_choco, read_csv) %>% bind_rows() 

estaciones_ideam_prec_choco <- ideam_prec_choco %>% 
  dplyr::select(CodigoEstacion, NombreEstacion, Latitud, Longitud, Altitud) %>%
  distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) %>%
  st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326)


## Temperatura Mensual Historica

files_temp_choco <- list.files("data/ideam/temperatura/choco/", recursive = T, full.names = T, pattern = ".csv$")

ideam_temp_choco <- map(files_temp_choco, read_csv) %>% bind_rows() 

estaciones_ideam_temp_choco <- ideam_temp_choco %>% 
  dplyr::select(CodigoEstacion, NombreEstacion, Latitud, Longitud, Altitud) %>%
  distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) %>%
  st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326)


## Mapas estaciones ####

# crear_mapa(choco_dem, choco_shp, abrigue_municipios_choco, estaciones_ideam_prec_choco, "Chocó - Estaciones IDEAM")
# crear_mapa(caqueta_dem, caqueta_shp, abrigue_municipios_caqueta, estaciones_ideam_prec_caqueta, "Caquetá - Estaciones IDEAM")
# crear_mapa(choco_dem, choco_shp, abrigue_municipios_choco, estaciones_ideam_temp_choco, "Chocó - Estaciones IDEAM - Temperatura", estacion_col = "red")
# crear_mapa(caqueta_dem, caqueta_shp, abrigue_municipios_caqueta, estaciones_ideam_temp_caqueta, "Caquetá - Estaciones IDEAM - Temperatura", estacion_col = "red")

plet(abrigue_municipios_choco, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_choco), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_choco), col="red", cex=2, popup=TRUE)


## CHIRPS DATA ####
# Fuente: https://chc.ucsb.edu/data/chirps

chirps_files_choco <- list.files("data/chirps_raster/choco/", full.names = T)


# Choco 

tb_chirps_choco <- chirps_files_choco %>%
  enframe(value = "file", name = NULL) %>%
  mutate(basename_file = basename(file),
         basename_file = str_remove(basename_file, "chirps-v2.0."),
         basename_file = str_remove(basename_file, ".tif")) %>%
  separate(basename_file, into = c("year", "month"), convert = T)

chirps_raster_mensual_choco  <- chirps_files_choco %>%
  rast() 

chirps_raster_anuales_choco <- tb_chirps_choco %>% nest(data = -year) %>%
  mutate(raster_anual = map(data, ~ app(rast(.x$file), sum)))

## Calcula linea base 
chirps_choco_linea_base_1995_2014 <- chirps_raster_anuales_choco %>%
  filter(year >1994, year<2015) %>%
  pull(raster_anual) %>% rast() %>% 
  app(median)


chirps_choco_linea_base_1995_2014[chirps_choco_linea_base_1995_2014 < 0] <- NA
plot(chirps_choco_linea_base_1995_2014, main = "CHIPRS-Chocó, Lluvia anual promedio (1995-2014)")


## Extract CHIRPS data by points ####
chirps_data_choco_ws <- extract_from_chirps(chirps_raster_mensual_choco, estaciones_ideam_prec_choco)


## ERA5 DATA ####

era5_files_choco <- list.files("data/era5_raster/era5/choco/", full.names = T)


# Choco 

tb_era_choco <- era5_files_choco %>%
  enframe(value = "file", name = NULL) %>%
  mutate(basename_file = basename(file),
         basename_file = str_remove(basename_file, "emperature_2m_"),
         basename_file = str_remove(basename_file, "_sum"),
         basename_file = str_remove(basename_file, "total_"),
         basename_file = str_remove(basename_file, ".tif")) %>%
  separate(basename_file, into = c("var", "date"), sep = "_mean_" , convert = T) %>% 
  separate(date, into = c("year", "month"),sep = "_", convert = T)

era5_tmax_mensual_choco  <- tb_era_choco %>% filter(var == "tmax") %>%
  mutate(raster_tmax = map(file, rast)) 

era5_tmin_mensual_choco  <- tb_era_choco %>% filter(var == "tmin") %>%
  mutate(raster_tmin = map(file, rast)) 

era5_tmean_mensual_choco <- left_join(era5_tmax_mensual_choco, 
                                      era5_tmin_mensual_choco, 
                                      by = c("year", "month")) %>%
  mutate(raster_mean = map2(raster_tmax, raster_tmin, ~(.x + .y)/2))


era5_tmean_anuales_choco <- era5_tmean_mensual_choco %>% 
  select(year, month, raster_mean) %>%
  nest(data = -year) %>%
  mutate(raster_anual = map(data, ~ app(rast(.x$raster_mean), mean)))

## Calcula linea base 
era5_choco_linea_base_1995_2014 <- era5_tmean_anuales_choco %>%
  filter(year >1994, year<2015) %>%
  pull(raster_anual) %>% rast() %>% 
  app(median)

plot(era5_choco_linea_base_1995_2014 - 273.15, main = "ERA5-Chocó, Temperatura media promedio (1995-2014)")



## Extract ERA5 data by locations ####
## Tmin
era5_tmin_data_choco_ws<- extract_from_era(tb_era_choco %>% 
                                             filter(var == "tmin") %>% 
                                             pull(file), estaciones_ideam_temp_choco)

## Tmax
era5_tmax_data_choco_ws <- extract_from_era(tb_era_choco %>% 
                                              filter(var == "tmax") %>% 
                                              pull(file), estaciones_ideam_temp_choco)
