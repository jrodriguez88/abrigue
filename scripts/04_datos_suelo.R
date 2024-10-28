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




## Prueba de descarga Geodata
depths <- c(5, 15, 30, 60, 100, 200)
depths <- c( 100, 200)

soc_soilgrids <- map(depths, ~soil_world(var="soc", depth=.x, path="data/soilgrids/"))
ocs_soilgrids <- map(depths, ~soil_world(var="ocs", depth=.x, path="data/soilgrids/")) # Carbon stocks

test_ocs <- soil_world(var="ocs", depth=60, path="data/soilgrids/")
plot(test_SOC)

soc5_choco <- crop(test_SOC, limites_choco, mask = T)
soc5_caqueta <- crop(test_SOC, limites_caqueta, mask = T)

plot(soc5_choco)
plot(soc5_caqueta)


ttt <- rast("data/soilgrids/soil_world/soc_30-60cm_mean_30s.tif")
plot(ttt)
