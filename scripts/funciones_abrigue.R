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


##Graficos clima 

time_line_estaciones <- function(ideam_data, estaciones_objetivo, ini_date, end_date){
  
  ideam_data %>%
    mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) %>% 
    filter(NombreEstacion %in% estaciones_objetivo) %>%
    select(Etiqueta, NombreEstacion, FechaInstalacion, FechaSuspension) %>% 
    distinct() %>%
    mutate(FechaInstalacion = as_date(dmy_hm(FechaInstalacion)),
           FechaInstalacion = as.Date(ifelse(FechaInstalacion < ymd(ini_date),  ymd(ini_date), FechaInstalacion)),
           FechaSuspension = as_date(dmy_hm(FechaSuspension)),
           FechaSuspension = as.Date(ifelse(is.na(FechaSuspension),  ymd(end_date), FechaSuspension))) %>%
    #  filter(FechaInstalacion > ymd("1981-01-01")) %>%
    # Crear el gráfico de segmentos
    ggplot(aes(x = FechaInstalacion, xend = FechaSuspension, 
               y = NombreEstacion, yend = NombreEstacion)) +
    geom_segment(aes(color = Etiqueta), size = 1.2, position = position_dodge(width = 0.1)) +  # Segmentos
    geom_vline(xintercept = ymd("1981-01-01")) +
    # scale_x_date(limits = c(ymd("1960-01-01"), ymd("2024-08-30"))) + 
    labs(title = "Linea de tiempo - Registro", x = "Fecha", y = "Nombre de la Estación") +
    theme_minimal() 
  
  
}

lineplot_mensual_clima <- function(data_to_plot, estaciones_objetivo, var = "Precipitacion (mm)"){
  
  data_to_plot %>%
    drop_na() %>% 
    filter(NombreEstacion %in% estaciones_objetivo) %>%
    ggplot(aes(date, value, color = Fuente)) +
    geom_line() +
    facet_wrap(~ NombreEstacion, scales = "free") +
    theme_bw(14) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom") +
    labs(x = NULL, y = var, fill = "Fuente: ") 
}


boxplot_mensual_clima <- function(data_to_plot, estaciones_objetivo, var = "Precipitacion (mm)"){
  
  data_to_plot %>%
    drop_na() %>% 
    filter(NombreEstacion %in% estaciones_objetivo) %>%
    ggplot(aes(month, value, fill = Fuente, group = interaction(month, Fuente))) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ NombreEstacion, scales = "free") +
    scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
    theme_bw(14) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom") +
    labs(x = NULL, y = var, fill = "Fuente: ") 
}

