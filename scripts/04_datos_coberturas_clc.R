library(terra)
library(dplyr)
library(sf)
library(stringr)


# Argumentos funcion 
clc_level = 2
resol = 250

out_path_choco <- "data/ideam/coberturas/choco/"
out_path_caqueta <- "data/ideam/coberturas/caqueta/"

list_shp_clc_choco <- list.files(out_path_choco, full.names = T, pattern = ".shp$")
list_shp_clc_caqueta <- list.files(out_path_caqueta, full.names = T, pattern = ".shp$")


# Cargar el shapefile choco
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2000_2002.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2005_2009.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2010_2012.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2018.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2020.shp")


## Funciones base

rasterize_clc <- function(clc_shp_path, out_path, clc_level = 2, resol = 250) {
  
  
  clc_shp <- vect(clc_shp_path)
  
  tag <- basename(clc_shp_path) %>% str_remove(".shp") 
  
    # #Extraer y procesar los códigos
  # clc_shp <- clc_shp %>% st_as_sf() %>%
  #   rename_with(tolower) %>%
  #   mutate(
  #     code_str = str_extract(codigo, "^[^ ]+"),
  #     code_str = str_replace(code_str, "\\.$", ""),  # Eliminar punto final si existe
  #     code_levels = str_split(code_str, "\\."),
  #     code_Nlevels = sapply(code_levels, function(x) paste0(x[1:clc_level], collapse = "")),
  #     codigo_simple = as.integer(code_Nlevels)
  #   ) %>% vect()
  
  clc_shp <- clc_shp %>% st_as_sf() %>%
    rename_with(tolower) %>%
    mutate(
      codigo_simple = as.integer(substr(codigo, 1, clc_level))
    ) %>% vect()
  
  # tag level
  nivel_clc = paste0("nivel_", clc_level)
  
  # Revisar geometrías inválidas
  invalidas <- !is.valid(clc_shp)
  if (any(invalidas)) {
    cat("Hay geometrías inválidas. Corrigiendo...\n")
    clc_shp <- buffer(clc_shp, width = 0) # Reparar geometrías
  }
  
  # Verificar si las geometrías están vacías
  if (nrow(clc_shp) == 0) {
    stop("El shapefile no contiene geometrías válidas después de la corrección.")
  }
  
  # Verificar el CRS del shapefile
  print(crs(clc_shp))
  
  # Crear el raster de plantilla con el CRS del shapefile
  template_raster <- rast(
    extent = ext(clc_shp),  # Extensión del shapefile
    resolution = resol, #c(resol, resol),           # Ajustar resolución
    crs = crs(clc_shp)     # Usar el CRS del shapefile
  )
  
  
  
  # Verificar valores únicos ategorias CLC
  print(unique(clc_shp$codigo_simple))
  
  # Remover geometrías con valores NA
  clc_shp <- clc_shp[!is.na(clc_shp$codigo_simple), ]
  if (nrow(clc_shp) == 0) {
    stop("No quedan geometrías válidas después de remover valores NA en 'codigo_simple'.")
  }
  
  # Rasterizar
  
  raster_cobertura <- rasterize(clc_shp, template_raster, field = "codigo_simple")
  
  raster_cobertura_wgs84 <- project(raster_cobertura, choco_dem)
  
  writeRaster(raster_cobertura, filename = paste0(out_path, tag,"_", nivel_clc, ".tif"),  overwrite = TRUE)
  writeRaster(raster_cobertura_wgs84, filename = paste0(out_path, tag,"_", nivel_clc, "_wgs84.tif"),  overwrite = TRUE)
  
  
  
  # Verificar si el raster tiene datos
  if (is.na(minmax(raster_cobertura)[1])) {
    stop("El raster no contiene valores. Revisa la superposición entre las geometrías y el raster.")
  }
  

# Plot para verificar
#plot(raster_cobertura, main = "Coberturas CLC")
# Extraer los códigos y leyendas únicos
  
  raster_cobertura
  
}

crear_paleta_clc <- function(nivel_clc) {
  # Crear el DataFrame de la paleta de GEE
  gee_palette <- data.frame(
    Value = c(323, 313, 311, 331, 243, 411, 322, 321, 244, 242, 231, 233, 512, 111, 124,
              112, 121, 131, 223, 241, 245, 511, 514, 312, 523, 423, 142, 232, 222, 333,
              314, 315, 334, 212, 211, 413, 221, 215, 141, 332, 335, 521, 125, 122, 421,
              123, 132, 513, 213, 412, 225, 224, 214, 422, 99),
    color = c(
      "#a6e64d",  # 323 - Vegetación secundaria o en transición
      "#4dff00",  # 313 - Bosque fragmentado
      "#80ff00",  # 311 - Bosque denso
      "#e6e6e6",  # 331 - Zonas arenosas naturales
      "#e6cc4d",  # 243 - Mosaico de cultivos, pastos y espacios naturales
      "#a6a6ff",  # 411 - Zonas pantanosas
      "#a6ff80",  # 322 - Arbustal
      "#ccf24d",  # 321 - Herbazal
      "#f2cca6",  # 244 - Mosaico de pastos con espacios naturales
      "#ffe64d",  # 242 - Mosaico de pastos y cultivos
      "#e6e64d",  # 231 - Pastos limpios
      "#e6e64d",  # 233 - Pastos enmalezados
      "#80f2e6",  # 512 - Lagunas, lagos y ciénagas naturales
      "#e6004d",  # 111 - Tejido urbano continuo
      "#e6cce6",  # 124 - Aeropuertos
      "#ff0000",  # 112 - Tejido urbano discontinuo
      "#cc4df2",  # 121 - Zonas industriales o comerciales
      "#a600cc",  # 131 - Zonas de extracción minera
      "#f2a64d",  # 223 - Cultivos permanentes arbóreos
      "#ffe6a6",  # 241 - Mosaico de cultivos
      "#e6cc4d",  # 245 - Mosaico de cultivos con espacios naturales
      "#00ccf2",  # 511 - Ríos
      "#80f2e6",  # 514 - Cuerpos de agua artificiales
      "#00a600",  # 312 - Bosque abierto
      "#e6f2ff",  # 523 - Estanques para acuicultura marina
      "#a6a6e6",  # 423 - Sedimentos expuestos en bajamar
      "#ffe6ff",  # 142 - Instalaciones recreativas
      "#e6e64d",  # 232 - Pastos arbolados
      "#f2a64d",  # 222 - Cultivos permanentes arbustivos
      "#ccffcc",  # 333 - Tierras desnudas y degradadas
      "#4dff00",  # 314 - Bosque de galería y ripario
      "#00a600",  # 315 - Plantación forestal
      "#000000",  # 334 - Zonas quemadas
      "#ffffa8",  # 212 - Cereales
      "#ffffa8",  # 211 - Otros cultivos transitorios
      "#a6a6ff",  # 413 - Vegetación acuática sobre cuerpos de agua
      "#e68000",  # 221 - Cultivos permanentes herbáceos
      "#ffffa8",  # 215 - Tubérculos
      "#ffa6ff",  # 141 - Zonas verdes urbanas
      "#cccccc",  # 332 - Afloramientos rocosos
      "#a6e6cc",  # 335 - Zonas glaciares y nivales
      "#00ffa6",  # 521 - Lagunas costeras
      "#cc0000",  # 125 - Obras hidráulicas
      "#cc0000",  # 122 - Red vial, ferroviaria y terrenos asociados
      "#ccccff",  # 421 - Pantanos costeros
      "#e6cccc",  # 123 - Zonas portuarias
      "#a64dcc",  # 132 - Zona de disposición de residuos
      "#80f2e6",  # 513 - Canales
      "#ffffa8",  # 213 - Oleaginosas y leguminosas
      "#4d4dff",  # 412 - Turberas
      "#f2a64d",  # 225 - Cultivos confinados
      "#f2cca6",  # 224 - Cultivos agroforestales
      "#ffffa8",  # 214 - Hortalizas
      "#e6e6ff",  # 422 - Salitral
      "#FFFFFF"   # 999 - Nubes
    )
  )
  
  # Crear los DataFrames para nivel_1, nivel_2 y nivel_3
  
  # nivel_1
  nivel_1 <- tibble(
    codigo_simple = c(1, 2, 3, 4, 5, 9),
    leyenda = c(
      "1. Territorios artificializados",
      "2. Territorios agrícolas",
      "3. Bosques y áreas seminaturales",
      "4. Áreas húmedas",
      "5. Superficies de agua",
      "9. Nubes"
    ),
    nivel = "nivel_1"
  )
  
  # Asignar colores a nivel_1
  colores_nivel1 <- c(
    "1. Territorios artificializados" = "#ff0000",
    "2. Territorios agrícolas" = "#ffff00",
    "3. Bosques y áreas seminaturales" = "#00a600",
    "4. Áreas húmedas" = "#4d4dff",
    "5. Superficies de agua" = "#00ccf2",
    "9. Nubes" = "#F8F8F8"
  )
  nivel_1 <- nivel_1 %>%
    mutate(color = colores_nivel1[leyenda])
  
  # nivel_2
  nivel_2 <- tibble(
    codigo_simple = c(11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 41, 42, 51, 52, 99),
    leyenda = c(
      "1.1. Zonas urbanizadas",
      "1.2. Zonas industriales o comerciales y redes de comunicación",
      "1.3. Zonas de extracción mineras y escombreras",
      "1.4. Zonas verdes artificializadas, no agrícolas",
      "2.1. Cultivos transitorios",
      "2.2. Cultivos permanentes",
      "2.3. Pastos",
      "2.4. Áreas agrícolas heterogéneas",
      "3.1. Bosques",
      "3.2. Áreas con vegetación herbácea y/o arbustiva",
      "3.3. Áreas abiertas, sin o con poca vegetación",
      "4.1. Áreas húmedas continentales",
      "4.2. Áreas húmedas costeras",
      "5.1. Aguas  continentales",
      "5.2. Aguas marítimas",
      "9.9. Nubes"
    ),
    nivel = "nivel_2"
  )
  
  # Asignar colores a nivel_2
  colores_nivel2 <- c(
    "1.1. Zonas urbanizadas" = "#ff0000",
    "1.2. Zonas industriales o comerciales y redes de comunicación" = "#cc0000",
    "1.3. Zonas de extracción mineras y escombreras" = "#a600cc",
    "1.4. Zonas verdes artificializadas, no agrícolas" = "#ffa6ff",
    "2.1. Cultivos transitorios" = "#ffff00",
    "2.2. Cultivos permanentes" = "#e68000",
    "2.3. Pastos" = "#e6e64d",
    "2.4. Áreas agrícolas heterogéneas" = "#ffe64d",
    "3.1. Bosques" = "#00a600",
    "3.2. Áreas con vegetación herbácea y/o arbustiva" = "#a6f200",
    "3.3. Áreas abiertas, sin o con poca vegetación" = "#cccccc",
    "4.1. Áreas húmedas continentales" = "#4d4dff",
    "4.2. Áreas húmedas costeras" = "#ccccff",
    "5.1. Aguas  continentales" = "#00ccf2",
    "5.2. Aguas marítimas" = "#00ffa6",
    "9.9. Nubes" = "#F8F8F8" 
  )
  nivel_2 <- nivel_2 %>%
    mutate(color = colores_nivel2[leyenda])
  
  # nivel_3
  nivel_3 <- tibble(
    codigo_simple = c(323, 313, 311, 331, 243, 411, 322, 321, 244, 242, 231, 233, 512, 111, 124,
                      112, 121, 131, 223, 241, 245, 511, 514, 312, 523, 423, 142, 232, 222, 333,
                      314, 315, 334, 212, 211, 413, 221, 215, 141, 332, 335, 521, 125, 122, 421,
                      123, 132, 513, 213, 412, 225, 224, 214, 422, 99),
    leyenda = c(
      "3.2.3. Vegetación secundaria o en transición",
      "3.1.3. Bosque fragmentado",
      "3.1.1. Bosque denso",
      "3.3.1. Zonas arenosas naturales",
      "2.4.3. Mosaico de cultivos, pastos y espacios naturales",
      "4.1.1. Zonas pantanosas",
      "3.2.2. Arbustal",
      "3.2.1. Herbazal",
      "2.4.4. Mosaico de pastos con espacios naturales",
      "2.4.2. Mosaico de pastos y cultivos",
      "2.3.1. Pastos limpios",
      "2.3.3. Pastos enmalezados",
      "5.1.2. Lagunas, lagos y ciénagas naturales",
      "1.1.1. Tejido urbano continuo",
      "1.2.4. Aeropuertos",
      "1.1.2. Tejido urbano discontinuo",
      "1.2.1. Zonas industriales o comerciales",
      "1.3.1. Zonas de extracción minera",
      "2.2.3. Cultivos permanentes arbóreos",
      "2.4.1. Mosaico de cultivos",
      "2.4.5. Mosaico de cultivos con espacios naturales",
      "5.1.1. Ríos",
      "5.1.4. Cuerpos de agua artificiales",
      "3.1.2. Bosque abierto",
      "5.2.3. Estanques para acuicultura marina",
      "4.2.3. Sedimentos expuestos en bajamar",
      "1.4.2. Instalaciones recreativas",
      "2.3.2. Pastos arbolados",
      "2.2.2. Cultivos permanentes arbustivos",
      "3.3.3. Tierras desnudas y degradadas",
      "3.1.4. Bosque de galería y ripario",
      "3.1.5. Plantación forestal",
      "3.3.4. Zonas quemadas",
      "2.1.2. Cereales",
      "2.1.1. Otros cultivos transitorios",
      "4.1.3. Vegetación acuática sobre cuerpos de agua",
      "2.2.1. Cultivos permanentes herbáceos",
      "2.1.5. Tubérculos",
      "1.4.1. Zonas verdes urbanas",
      "3.3.2. Afloramientos rocosos",
      "3.3.5. Zonas glaciares y nivales",
      "5.2.1. Lagunas costeras",
      "1.2.5. Obras hidráulicas",
      "1.2.2. Red vial, ferroviaria y terrenos asociados",
      "4.2.1. Pantanos costeros",
      "1.2.3. Zonas portuarias",
      "1.3.2. Zona de disposición de residuos",
      "5.1.3. Canales",
      "2.1.3. Oleaginosas y leguminosas",
      "4.1.2. Turberas",
      "2.2.5. Cultivos confinados",
      "2.2.4. Cultivos agroforestales",
      "2.1.4. Hortalizas",
      "4.2.2. Salitral",
      "9.9. Nubes"
    ),
    nivel = "nivel_3"
  )
  
  # Asignar colores a nivel_3 usando la paleta GEE
  nivel_3 <- nivel_3 %>%
    left_join(gee_palette, by = c("codigo_simple" = "Value")) %>%
    mutate(color = case_when(
      !is.na(color) ~ color,  
      TRUE ~ "#808080"  # Color genérico para otros
    )) %>%
    select(codigo_simple, leyenda, nivel, color)
  
  # Combinar los DataFrames en una sola tabla
  tabla_final <- bind_rows(nivel_1, nivel_2, nivel_3)
  
  # Asegurar que todas las leyendas tengan un color
  tabla_final <- tabla_final %>%
    mutate(color = ifelse(is.na(color), "#808080", color))
  
  # Resultado Final
  tabla_final %>% filter(nivel == nivel_clc)
  
}

plot_raster_clc <- function(raster_clc, clc_level = 2, tag = "") {
  
  
  # tag level
  
  nivel_clc = paste0("nivel_", clc_level)
  
  
  grupos <- crear_paleta_clc(nivel_clc)  %>%
    mutate(color = ifelse(is.na(color), "#808080", color)) %>%
    arrange(leyenda)
  
  
  
  # Verificar y convertir los tipos de datos para asegurar coincidencias
  grupos$codigo_simple <- as.character(grupos$codigo_simple)
  valores_raster <- unique(as.character(values(raster_clc)))
  
  
  
  # Crear una tabla de colores
  color_table <- data.frame(
    id = grupos$codigo_simple,
    color = grupos$color
  )
  
  # Asignar la tabla de colores al raster usando coltab()
  coltab(raster_clc) <- color_table
  
  # Visualizar el raster
  plot(raster_clc, main = paste0("Mapa de Coberturas CLC - ", tag, "-", nivel_clc), col = color_table$color)
  legend("topright", legend = grupos$leyenda, fill = grupos$color, cex = 0.7, title = "Coberturas", xpd = TRUE, inset = c(-0.1, 0))
  
  
  
}



raster_clc_caqueta <- map(.x = list_shp_clc_caqueta, ~rasterize_clc(.x, out_path = out_path_caqueta, clc_level = clc_level))
tags_caqueta <- basename(list_shp_clc_caqueta) %>% str_remove_all("clc_|.shp") 

raster_clc_choco<- map(.x = list_shp_clc_choco, ~rasterize_clc(.x, out_path = out_path_choco, clc_level = clc_level))
tags_choco <- basename(list_shp_clc_choco) %>% str_remove_all("clc_|.shp") 




par(mfrow = c(1, 5))
#map(list_shp_clc_caqueta, ~rasterize_clc(.x, out_path = out_path))
map2(raster_clc_choco, tags_choco, ~plot_raster_clc(.x, clc_level = clc_level, tag = .y))
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))
map2(raster_clc_caqueta, tags_caqueta, ~plot_raster_clc(.x, clc_level = clc_level, tag = .y))
par(mfrow = c(1, 1))



# writeRaster(raster_cobertura, filename = paste0(out_path, tag,"_", nivel_clc, ".tif"),  overwrite = TRUE)


#plot(rast("data/ideam/coberturas/choco/coberturas_CLC_nivel_3.tif"))

# par(mfrow = c(1, 5))
# 
# plot_raster_clc(raster_clc, clc_level = clc_level)
# plot_raster_clc(raster_clc, clc_level = clc_level)
# plot_raster_clc(raster_clc, clc_level = clc_level)
# plot_raster_clc(raster_clc, clc_level = clc_level)


