# Descarga datos GBIF para Modelacion de especies ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024

# library(geodata)


## Especies ----

# Caqueta
cacao <- c("theobroma", "cacao")
canangucha <- c("mauritia", "flexuosa")
copoazu <- c("theobroma", "grandiflorum")

# Choco
coco <- c("cocos", "nucifera")
vainilla <- c("vanilla", "planifolia")
# c("Vanilla", "rivasii")
# c("Vanilla", "cribbiana")
# c("Vanilla", "trigonocarpa")
# c("Vanilla", "dresslerii")


## Limites Territoriales----
limites_caqueta <- project(caqueta_shp, caqueta_dem)
municipios_caqueta <- project(abrigue_municipios_caqueta, caqueta_dem)
ext_caqueta <- ext(limites_caqueta)


limites_choco <- project(choco_shp, choco_dem)
municipios_choco <- project(abrigue_municipios_choco, choco_dem)
ext_choco <- ext(limites_choco)

par(mfrow = c(1,2))
plot(limites_caqueta, main = paste0("Caquetá - ", round(ext_caqueta, 2)))
lines(municipios_caqueta)

plot(limites_choco, main = paste0("Chocó- ", round(ext_choco, 2)))
lines(municipios_choco)
#par(mfrow = c(1,1))


## Descarga de datos GBIF ----

# Caqueta
cacao_gbif <- sp_occurrence(cacao[1], species=cacao[2], ext = ext_caqueta)
canangucha_gbif <- sp_occurrence(canangucha[1], species=canangucha[2], ext = ext_caqueta)
copoazu_gbif <- sp_occurrence(copoazu[1], species=copoazu[2], ext = ext_caqueta)

# Choco
coco_gbif <- sp_occurrence(coco[1], species=coco[2], ext = ext_choco)
vainilla_gbif <- sp_occurrence(vainilla[1], species="", ext = ext_choco)


## Preprocesamiento de datos GBIF ----

# Convertir a Spatial points

# Caqueta
cacao_points <- vect(cacao_gbif, geom=c("lon", "lat"), crs=crs(limites_caqueta))
canangucha_points <- vect(canangucha_gbif, geom=c("lon", "lat"), crs=crs(limites_caqueta))
copoazu_points <- vect(copoazu_gbif, geom=c("lon", "lat"), crs=crs(limites_caqueta))

# Choco
coco_points <- vect(coco_gbif, geom=c("lon", "lat"), crs=crs(limites_caqueta))
vainilla_points <- vect(vainilla_gbif, geom=c("lon", "lat"), crs=crs(limites_caqueta))



# Visualizar en el espacio
plot(limites_caqueta)
points(cacao_points, col='red', pch=17, cex = 1)

plot(limites_choco)
points(vainilla_points, col='red', pch=17, cex = 1)


# Filtrar datos dentro del departamento

# Caqueta
cacao_gbif_caqueta <- cacao_points[limites_caqueta, ]
canangucha_gbif_caqueta <- canangucha_points[limites_caqueta, ]
copoazu_gbif_caqueta <- copoazu_points[limites_caqueta, ]

# Numero de registros
map(c(cacao_gbif_caqueta, canangucha_gbif_caqueta, copoazu_gbif_caqueta), nrow) 

# Choco
coco_gbif_choco <- coco_points[limites_choco, ]
vainilla_gbif_choco <- vainilla_points[limites_choco, ]

# Numero de registros
map(c(coco_gbif_choco, vainilla_gbif_choco), nrow) 



## Mapas de Ocurrencia - GBIF ----

plot(limites_caqueta)
lines(municipios_caqueta)
points(cacao_gbif_caqueta, col='chocolate', pch=15, cex= 1.5)
points(canangucha_gbif_caqueta, col='darkred', pch=17, cex= 1.5)
points(copoazu_gbif_caqueta, col= 'yellow', pch=19, cex= 1.5)
legend(x = -73.5, y = 3, # Coordinates
       legend = c("Cacao", "Cañangucha", "Copoazu"),
       col = c('chocolate', 'darkred', 'yellow'),
       pch = c(15, 17, 19))


plot(limites_choco)
lines(municipios_choco)
points(coco_gbif_choco, col='brown', pch=15, cex= 1)
points(vainilla_gbif_choco, col= 'darkgreen', pch=17, cex= 1)
legend(x = -76.85, y = 8.4, # Coordinates
       legend = c("Coco", "Vainilla"),
       col = c('brown', 'darkgreen'),
       pch = c(15, 17))
  
par(mfrow = c(1,1))
