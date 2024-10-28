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

caqueta_shp <- departamentos[departamentos$DeNombre == "Caquetá"]
caqueta_municipios_shp <- municipios[municipios$Depto == "Caquetá"]
abrigue_municipios_caqueta <- caqueta_municipios_shp[caqueta_municipios_shp$MpNombre %in% 
                                                   c("San José Del Fragua", "Montañita", "Belén De Los Andaquíes", "Albania", "El Paujil")]

## Digital Elevation Model ####
# For TPS interpolation

DEM <- geodata::elevation_30s(country = "COL", path=tempdir())
caqueta_dem <- crop_raster(DEM, caqueta_shp)


# Estaciones IDEAM #### 
# Fuente: http://dhime.ideam.gov.co/atencionciudadano/


## Precipitacion Mensual Historica

files_prec_caqueta <- list.files("data/ideam/precipitacion/caqueta/", recursive = T, full.names = T, pattern = ".csv$")

ideam_prec_caqueta <- map(files_prec_caqueta, read_csv) %>% bind_rows() 

estaciones_ideam_prec_caqueta <- ideam_prec_caqueta %>% 
  dplyr::select(CodigoEstacion, NombreEstacion, Latitud, Longitud, Altitud) %>%
  distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) %>%
  st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326)


## Temperatura Mensual Historica

files_temp_caqueta <- list.files("data/ideam/temperatura/caqueta/", recursive = T, full.names = T, pattern = ".csv$")

ideam_temp_caqueta <- map(files_temp_caqueta, read_csv) %>% bind_rows() %>% distinct()

estaciones_ideam_temp_caqueta <- ideam_temp_caqueta %>% 
  dplyr::select(CodigoEstacion, NombreEstacion, Latitud, Longitud, Altitud) %>%
  distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) %>%
  st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326)


## Mapas estaciones ####

# crear_mapa(choco_dem, choco_shp, abrigue_municipios_choco, estaciones_ideam_prec_choco, "Chocó - Estaciones IDEAM")
# crear_mapa(caqueta_dem, caqueta_shp, abrigue_municipios_caqueta, estaciones_ideam_prec_caqueta, "Caquetá - Estaciones IDEAM")
# crear_mapa(choco_dem, choco_shp, abrigue_municipios_choco, estaciones_ideam_temp_choco, "Chocó - Estaciones IDEAM - Temperatura", estacion_col = "red")
# crear_mapa(caqueta_dem, caqueta_shp, abrigue_municipios_caqueta, estaciones_ideam_temp_caqueta, "Caquetá - Estaciones IDEAM - Temperatura", estacion_col = "red")

plet(abrigue_municipios_caqueta, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_caqueta), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_caqueta), col="red", cex=2, popup=TRUE)


## CHIRPS DATA ####
# Fuente: https://chc.ucsb.edu/data/chirps

chirps_files_caqueta <- list.files("data/chirps_raster/caqueta/", full.names = T)


# Caqueta

tb_chirps_caqueta <- chirps_files_caqueta %>%
  enframe(value = "file", name = NULL) %>%
  mutate(basename_file = basename(file),
         basename_file = str_remove(basename_file, "chirps-v2.0."),
         basename_file = str_remove(basename_file, ".tif")) %>%
  separate(basename_file, into = c("year", "month"), convert = T)

chirps_raster_mensual_caqueta  <- chirps_files_caqueta %>%
  rast() 

chirps_raster_anuales_caqueta <- tb_chirps_caqueta %>% nest(data = -year) %>%
  mutate(raster_anual = map(data, ~ app(rast(.x$file), sum)))

## Calcula linea base 
chirps_caqueta_linea_base_1995_2014 <- chirps_raster_anuales_caqueta %>%
  filter(year >1994, year<2015) %>%
  pull(raster_anual) %>% rast() %>% 
  app(median)


chirps_caqueta_linea_base_1995_2014[chirps_caqueta_linea_base_1995_2014 < 0] <- NA
plot(chirps_caqueta_linea_base_1995_2014)


## Extract CHIRPS data by points ####
chirps_data_caqueta_ws <- extract_from_chirps(chirps_raster_mensual_caqueta, estaciones_ideam_prec_caqueta)


## ERA5 DATA ####

era5_files_caqueta <- list.files("data/era5_raster/era5/caqueta/", full.names = T)


# Caqueta

tb_era_caqueta <- era5_files_caqueta %>%
  enframe(value = "file", name = NULL) %>%
  mutate(basename_file = basename(file),
         basename_file = str_remove(basename_file, "emperature_2m_"),
         basename_file = str_remove(basename_file, "_sum"),
         basename_file = str_remove(basename_file, "total_"),
         basename_file = str_remove(basename_file, ".tif")) %>%
  separate(basename_file, into = c("var", "date"), sep = "_mean_" , convert = T) %>% 
  separate(date, into = c("year", "month"),sep = "_", convert = T)

era5_tmax_mensual_caqueta  <- tb_era_caqueta %>% filter(var == "tmax") %>%
  mutate(raster_tmax = map(file, rast)) 

era5_tmin_mensual_caqueta  <- tb_era_caqueta %>% filter(var == "tmin") %>%
  mutate(raster_tmin = map(file, rast)) 

era5_tmean_mensual_caqueta <- left_join(era5_tmax_mensual_caqueta, 
                                      era5_tmin_mensual_caqueta, 
                                      by = c("year", "month")) %>%
  mutate(raster_mean = map2(raster_tmax, raster_tmin, ~(.x + .y)/2))


era5_tmean_anuales_caqueta <- era5_tmean_mensual_caqueta %>% 
  select(year, month, raster_mean) %>%
  nest(data = -year) %>%
  mutate(raster_anual = map(data, ~ app(rast(.x$raster_mean), mean)))

## Calcula linea base 
era5_caqueta_linea_base_1995_2014 <- era5_tmean_anuales_caqueta %>%
  filter(year >1994, year<2015) %>%
  pull(raster_anual) %>% rast() %>% 
  app(median)

plot(era5_caqueta_linea_base_1995_2014 - 273.15, main = "ERA5-Caquetá, Temperatura media promedio (1995-2014)")


## Extract ERA5 data by locations ####
## Tmin
era5_tmin_data_caqueta_ws <- extract_from_era(rast(era5_tmin_mensual_caqueta$raster_tmin), estaciones_ideam_temp_caqueta)


## Tmax
era5_tmax_data_caqueta_ws <- extract_from_era(tb_era_caqueta %>% filter(var == "tmax") %>% 
                                                pull(file), estaciones_ideam_temp_caqueta)

