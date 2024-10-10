## Funciones ABRIGUE Modeling
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024


# library(terra)

# Definir la función actualizada
crop_raster <- function(raster, shapefile, output_folder) {

  # Reproyectar el shapefile al CRS del raster
  shapefile_proj <- project(shapefile, crs(raster))
  
  # Recortar el raster a la extensión del shapefile
  raster_recortado <- crop(raster, ext(shapefile_proj))
  
  # Aplicar máscara al raster utilizando el shapefile
  raster_mascara <- mask(raster_recortado, shapefile_proj)
  
  # # Crear el nombre del archivo de salida
  # raster_name <- basename(raster_path)
  # output_path <- file.path(output_folder, raster_name)
  # 
  # # Guardar el raster recortado en el folder especificado
  # writeRaster(raster_mascara, output_path, overwrite=TRUE)
  
  return(raster_mascara)
}


# Crear mapa estaciones ideam, DEM
crear_mapa <- function(dem, dpto_shp, mpio_shp, estaciones, title = "", estacion_col = "blue") {
  
  d <- project(dpto_shp, dem)
  #p <- project(choco_municipios_shp, choco_dem)
  a <- project(mpio_shp, dem)
  
  
  plot(dem, main = title, col = terrain.colors(100))
  lines(a, col = "red", lwd = 0.5)
  lines(d, lwd = 4)
  lines(d, col = "white", lwd = 1)
  points(estaciones, cex = 1.5, col = "lightblue")
  points(estaciones, cex = 1, col = estacion_col)
  text(a, "MpNombre", cex = .8, halo = TRUE)
}


# Extract values from CHIRPS raster  using spatial points
extract_from_chirps <- function(raster, puntos_estaciones){
  
  
  if(is.vector(raster)){
    raster %>%
    rast() %>% 
      terra::extract(., puntos_estaciones) %>% #tibble()
      #tibble() %>% 
      mutate(ID = puntos_estaciones$CodigoEstacion ) %>%
      pivot_longer(cols = -c(ID), names_to = "file") %>%
      
      mutate(date = lubridate::ym(str_sub(file, -7,-1)),
             year = year(date), 
             month = month(date)) %>% 
      dplyr::select(-file,  CodigoEstacion = ID, date, year, month, value)
    
  } else {
  
  raster %>%
      terra::extract(., puntos_estaciones) %>% #tibble()
      tibble() %>%
    mutate(ID = puntos_estaciones$CodigoEstacion ) %>%
    pivot_longer(cols = -c(ID), names_to = "file") %>%
    
    mutate(date = lubridate::ym(str_sub(file, -7,-1)),
           year = year(date), 
           month = month(date)) %>% 
    dplyr::select(-file,  CodigoEstacion = ID, date, year, month, value)
  }
  
  
}

# Extract values from ERA5 raster  using spatial points
extract_from_era <- function(raster, puntos_estaciones){
  
  
  if(is.vector(raster)){
    raster %>%
      rast() %>% 
      terra::extract(., puntos_estaciones) %>% #tibble()
      #tibble() %>% 
      set_names(c("ID", basename(raster))) %>%
      mutate(ID = puntos_estaciones$CodigoEstacion ) %>%
      pivot_longer(cols = -c(ID), names_to = "file") %>%
      
      mutate(date = lubridate::ym(str_sub(file, -8,-1)),
             year = year(date), 
             month = month(date)) %>% 
      dplyr::select(-file,  CodigoEstacion = ID, date, year, month, value)
    
  } else {
    
   raster %>%
      terra::extract(., puntos_estaciones) %>% #tibble()
     # tibble() tes %>%
      set_names(c("ID", basename(sources(raster)))) %>%
      mutate(ID = puntos_estaciones$CodigoEstacion ) %>%
      pivot_longer(cols = -c(ID), names_to = "file") %>%
      
      mutate(date = lubridate::ym(str_sub(file, -8,-1)),
             year = year(date), 
             month = month(date)) %>% 
      dplyr::select(-file,  CodigoEstacion = ID, date, year, month, value)
  }
  
  
}


