# Descarga datos SoilGrids para Modelacion de especies ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024



# https://rdrr.io/cran/geodata/man/soil_grids.html


# var	description	unit
# bdod	Bulk density of the fine earth fraction	kg dm-3
# cec	Cation Exchange Capacity of the soil	cmol(+) kg-1
# cfvo	Vol. fraction of coarse fragments (> 2 mm)	%
# nitrogen	Total nitrogen (N)	g kg-1
# phh2o	pH (H2O)	-
# sand	Sand (> 0.05 mm) in fine earth	%
# silt	Silt (0.002-0.05 mm) in fine earth	%
# clay	Clay (< 0.002 mm) in fine earth	%
# soc	Soil organic carbon in fine earth	g kg-1
# ocd	Organic carbon density	kg m-3
# ocs	Organic carbon stocks	kg m-2


## Limites Territoriales----
limites_caqueta <- project(caqueta_shp, caqueta_dem)
municipios_caqueta <- project(abrigue_municipios_caqueta, caqueta_dem)
ext_caqueta <- ext(limites_caqueta)


limites_choco <- project(choco_shp, choco_dem)
municipios_choco <- project(abrigue_municipios_choco, choco_dem)
ext_choco <- ext(limites_choco)

# test_ocs <- soil_world(var="ocs", depth=60, path="data/soilgrids/")
# plot(test_SOC)


## SOILGRIDS - Prueba de descarga Geodata ----
# Profundidades
depths <- c(5, 15, 30, 60)#, 100, 200)
#depths <- c( 100, 200)


# Variables disponibles
vars = c("clay", "sand", "silt", "soc", "phh2o", "bdod", "phh2o", "ocd")


## descarga 
sand_soilgrids <- map(depths, ~soil_world(var="sand", depth=.x, path="data/soilgrids/"))
clay_soilgrids <- map(depths, ~soil_world(var="clay", depth=.x, path="data/soilgrids/"))
silt_soilgrids <- map(depths, ~soil_world(var="silt", depth=.x, path="data/soilgrids/"))
soc_soilgrids <- map(depths, ~soil_world(var="soc", depth=.x, path="data/soilgrids/"))
bdod_soilgrids <- map(depths, ~soil_world(var="bdod", depth=.x, path="data/soilgrids/"))
nitrogen_soilgrids <- map(depths, ~soil_world(var="nitrogen", depth=.x, path="data/soilgrids/"))
phh2o_soilgrids <- map(depths, ~soil_world(var="phh2o", depth=.x, path="data/soilgrids/"))
ocd_soilgrids <- map(depths, ~soil_world(var="ocd", depth=.x, path="data/soilgrids/")) # Carbon stocks



# Carga de datos descargados

bdod_files <- list.files("data/soilgrids/soil_world/", pattern = "bdod_", full.names = T)
soc_files <- list.files("data/soilgrids/soil_world/", pattern = "soc_", full.names = T)
sand_files <- list.files("data/soilgrids/soil_world/", pattern = "sand_", full.names = T)
clay_files <- list.files("data/soilgrids/soil_world/", pattern = "clay_", full.names = T)
silt_files <- list.files("data/soilgrids/soil_world/", pattern = "silt_", full.names = T)
nitrogen_files <- list.files("data/soilgrids/soil_world/", pattern = "nitrogen_", full.names = T)
phh2o_files <- list.files("data/soilgrids/soil_world/", pattern = "phh2o_", full.names = T)
ocd_files <- list.files("data/soilgrids/soil_world/", pattern = "ocd_", full.names = T)




## Carga en R como raster
rast_list <- list(bdod_files, soc_files,  sand_files, clay_files,  nitrogen_files, phh2o_files, ocd_files) %>%
  map(.x = .,  ~rast(.x) %>% crop(limites_caqueta, mask = T))


rast_list %>% set_names(c("bdod", "soc",  "sand", "silt",  "nitrogen", "phh2o", "ocd")) %>% 
  map(~writeRaster(., paste0(names(.), "_caqueta.tif")))

## Carga en R como raster
rast_list <- list(clay_files) %>%
  map(.x = .,  ~rast(.x) %>% crop(limites_caqueta, mask = T))


rast_list %>% set_names(c("clay")) %>% 
  map(~writeRaster(., paste0(names(.), "_caqueta.tif")))



files_caqueta <- list.files("data/soilgrids/caqueta", pattern = ".tif", full.names = T) 
files_choco <- list.files("data/soilgrids/choco", pattern = ".tif", full.names = T)


par(mfrow = c(2, 4))
vars %>% enframe(name = NULL, value = "name") %>% 
  mutate(links = map(name, ~str_subset(files_caqueta, pattern = .x))) %>%
  mutate(rasters = map(links, rast),
         mean60 = map(rasters, ~app(.x, mean, na.rm = T))) %>%
  mutate(plot = map2(mean60, name, ~plot(.x, main = .y, cex = 2,  col = map.pal("elevation"))))

par(mfrow = c(2, 4))
vars %>% enframe(name = NULL, value = "name") %>% 
  mutate(links = map(name, ~str_subset(files_choco, pattern = .x))) %>%
  mutate(rasters = map(links, rast),
         mean60 = map(rasters, ~app(.x, mean, na.rm = T))) %>%
  mutate(plot = map2(mean60, name, ~plot(.x, main = .y, cex = 2,  col = map.pal("elevation"))))


# par(mfrow = c(1, 2))
# plot(rast("data/soilgrids/choco/soc_0-5cm_mean_30s.tif"), main = "SoilGrids - Carbono Organico en el Suelo -  Choco", col = map.pal("elevation"))
# plot(rast("data/soilgrids/caqueta/soc_0-5cm_mean_30s.tif"), main = "SoilGrids - Carbono Organico en el Suelo -  Caqueta", col = map.pal("elevation"))
# 
# 
# par(mfrow = c(1, 2))
# plot(rast("data/soilgrids/choco/phh2o_15-30cm_mean_30s.tif"), main = "SoilGrids - pH en el Suelo - Choco", col = map.pal("elevation"))
# plot(rast("data/soilgrids/caqueta/phh2o_15-30cm_mean_30s.tif"), main = "SoilGrids - pH en el Suelo - Caqueta", col = map.pal("elevation"))
# 
# 
# 
# 



ph_soilgrids <- rast("data/soilgrids/caqueta/phh2o_5-15cm_mean_30s.tif")


file <- "data/abrigue/BD_Servicios ecosistémicos SUELOS_FQ v10.10.2024.xlsx" 


soil_abrigue <- readxl::read_excel(file, sheet = 1)

puntos_ph <- soil_abrigue %>% select(Latitud, Longitud, pH) %>% 
  st_as_sf(.,coords = c("Longitud", "Latitud"), crs = 4326)


soilgrids_ph <- extract(ph_soilgrids, puntos_ph) 


eval_ph <- bind_cols(puntos_ph, soilgrids_ph)


eval_ph %>% ggplot(aes(pH, `phh2o_5-15cm`)) +
  geom_point() + geom_smooth(method = "lm")





N_soilgrids <- rast("data/soilgrids/caqueta/nitrogen_5-15cm_mean_30s.tif")

puntos_N <- soil_abrigue %>% select(Latitud, Longitud, `N Total`) %>% 
  st_as_sf(.,coords = c("Longitud", "Latitud"), crs = 4326)


soilgrids_N <- extract(N_soilgrids, puntos_N) 


eval_N <- bind_cols(puntos_N, soilgrids_N)


eval_N %>% ggplot(aes(`N Total`, `nitrogen_5-15cm`/10)) +
  geom_point() + geom_smooth(method = "lm")




## Suelos IGAC ----
# https://metadatos.icde.gov.co/geonetwork/srv/spa/catalog.search#/metadata/0c8290b7-d80c-4007-b2c7-247860656cdd
# https://antiguo.igac.gov.co/sites/igac.gov.co/files/i40100-1016.v1etapadeposcampoparaloslevantamientosdesuelos.pdf
# https://www.igac.gov.co/sites/default/files/listadomaestro/in-gag-pc05-04_descripcion_de_unidades_cartograficas_de_suelos_aplicado_a_los_levantamientos.pdf?form=MG0AV3
soil_igac_caqueta  <- vect("data/igac/caqueta/CAQUETA_SUELOS_VF.shp")
soil_igac_choco  <- vect("data/igac/choco/CHOCO_SUELOS_VF.shp")

plot(soil_igac_caqueta, "UCS", col = map.pal("rainbow"), main = "Unidades Cartográficas de Suelos (UCS) - IGAC, Caquetá") 

plot(soil_igac_choco, "UCS", col = map.pal("rainbow"), main = "Unidades Cartográficas de Suelos (UCS) -IGAC, Chocó") 





