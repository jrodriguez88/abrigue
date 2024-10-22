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
test_SOC <- soil_world(var="soc", depth=5, path=tempdir())
plot(test_SOC)

soc5_choco <- crop(test_SOC, limites_choco, mask = T)
soc5_caqueta <- crop(test_SOC, limites_caqueta, mask = T)

plot(soc5_choco)
plot(soc5_caqueta)
