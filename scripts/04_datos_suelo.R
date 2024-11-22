# Descarga datos SoilGrids para Modelacion de especies ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024



## Limites Territoriales----
limites_caqueta <- project(caqueta_shp, caqueta_dem)
municipios_caqueta <- project(abrigue_municipios_caqueta, caqueta_dem)
ext_caqueta <- ext(limites_caqueta)


limites_choco <- project(choco_shp, choco_dem)
municipios_choco <- project(abrigue_municipios_choco, choco_dem)
ext_choco <- ext(limites_choco)

# test_ocs <- soil_world(var="ocs", depth=60, path="data/soilgrids/")
# plot(test_SOC)


## Prueba de descarga Geodata
depths <- c(5, 15, 30, 60)#, 100, 200)
depths <- c( 100, 200)

vars = c("clay", "sand")

sand_soilgrids <- map(depths, ~soil_world(var="sand", depth=.x, path="data/soilgrids/"))
clay_soilgrids <- map(depths, ~soil_world(var="clay", depth=.x, path="data/soilgrids/"))
soc_soilgrids <- map(depths, ~soil_world(var="soc", depth=.x, path="data/soilgrids/"))
bdod_soilgrids <- map(depths, ~soil_world(var="bdod", depth=.x, path="data/soilgrids/"))
nitrogen_soilgrids <- map(depths, ~soil_world(var="nitrogen", depth=.x, path="data/soilgrids/"))
phh2o_soilgrids <- map(depths, ~soil_world(var="phh2o", depth=.x, path="data/soilgrids/"))
ocd_soilgrids <- map(depths, ~soil_world(var="ocd", depth=.x, path="data/soilgrids/")) # Carbon stocks



soc_choco <- crop(test_SOC, limites_choco, mask = T)
soc5_caqueta <- crop(test_SOC, limites_caqueta, mask = T)

plot(soc5_choco)
plot(soc5_caqueta)


bdod_files <- list.files("data/soilgrids/soil_world/", pattern = "bdod_", full.names = T)
soc_files <- list.files("data/soilgrids/soil_world/", pattern = "soc_", full.names = T)
sand_files <- list.files("data/soilgrids/soil_world/", pattern = "sand_", full.names = T)
clay_files <- list.files("data/soilgrids/soil_world/", pattern = "clay_", full.names = T)
nitrogen_files <- list.files("data/soilgrids/soil_world/", pattern = "nitrogen_", full.names = T)
phh2o_files <- list.files("data/soilgrids/soil_world/", pattern = "phh2o_", full.names = T)
ocd_files <- list.files("data/soilgrids/soil_world/", pattern = "ocd_", full.names = T)

ttt <- rast(soc_files)
plot(ttt)


ttt %>% crop(limites_choco, mask = T) %>% plot()

rast_list <- list(bdod_files, soc_files,  sand_files, clay_files,  nitrogen_files, phh2o_files, ocd_files) %>%
  map(.x = .,  ~rast(.x) %>% crop(limites_caqueta, mask = T))


rast_list %>% set_names(c("bdod", "soc",  "sand", "clay",  "nitrogen", "phh2o", "ocd")) %>% 
  map(~writeRaster(., paste0(names(.), "_caqueta.tif")))

par(mfrow = c(1, 2))
plot(rast("data/soilgrids/choco/soc_0-5cm_mean_30s.tif"), main = "SoilGrids - Carbono Organico en el Suelo -  Choco", , col = map.pal("elevation"))
