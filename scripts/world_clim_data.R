library(terra)
library(tmap)
library(tidyverse)

# Cargar múltiples raster

files_prec <- list.files("data/worldclim/choco/", full.names = TRUE, pattern = "prec")
files_tmax <- list.files("data/worldclim/choco/", full.names = TRUE, pattern = "tmax")
files_tmin <- list.files("data/worldclim/choco/", full.names = TRUE, pattern = "tmin")



#Precipitacion


files_sp245 <- str_subset(files_prec, pattern = "sp245")
files_sp585 <- str_subset(files_prec, pattern = "sp585")

rasters_245 <- lapply(files_sp245, rast)
rasters_585 <- lapply(files_sp585, rast)

mean_245 <- app(rast(files_sp245), fun = "mean") 
mean_585 <- app(rast(files_sp585), fun = "mean") 
sum_245 <- files_sp245 %>% map(~app(rast(.x), fun = "sum") )
sum_585 <- files_sp585 %>% map(~app(rast(.x), fun = "sum") )


par(mfrow = c(1, 3))
plot(choco_ppt)
plot(app(do.call(c, sum_245), mean))
plot(app(do.call(c, sum_585), mean))


# Comparacion Escenarios
map(sum_245, as.vector) %>% 
  set_names(basename(files_sp245) %>% str_remove("wc2.1_2.5m_prec_") %>% str_remove("_ssp245_2041-2060.tif")) %>%
  enframe %>%
  unnest(value) %>% drop_na() %>% 
  mutate(Escenario = "SSP 245") %>% 
  
  bind_rows(., 
            map(sum_585, as.vector) %>% 
              set_names(basename(files_sp585) %>% str_remove("wc2.1_2.5m_prec_") %>% str_remove("_ssp585_2041-2060.tif")) %>%
              enframe %>%
              unnest(value) %>% drop_na() %>% 
              mutate(Escenario = "SSP 585")
  ) %>%
  bind_rows(.,
            
            choco_ppt %>% as.vector() %>% 
              enframe %>% drop_na() %>% 
              mutate(Escenario = "Linea Base CHIRPS", 
                     name = "z_Linea Base")
            
            
  ) %>%
  ggplot(aes(name, value, fill= Escenario)) + geom_boxplot() +
  theme_bw() + coord_flip()





#Temperatura Maxima

files_sp245 <- str_subset(files_tmax, pattern = "sp245")
files_sp585 <- str_subset(files_tmax, pattern = "sp585")

rasters_245 <- lapply(files_sp245, rast)
rasters_585 <- lapply(files_sp585, rast)

mean_245 <- app(rast(files_sp245), fun = "mean") 
mean_585 <- app(rast(files_sp585), fun = "mean") 
sum_245 <- files_sp245 %>% map(~app(rast(.x), fun = "mean") )
sum_585 <- files_sp585 %>% map(~app(rast(.x), fun = "mean") )


par(mfrow = c(1, 3))
plot(choco)
plot(mean_245)
plot(mean_585)

# Comparacion Escenarios
map(sum_245, as.vector) %>% 
  set_names(basename(files_sp245) %>% str_remove("wc2.1_2.5m_tmax_") %>% str_remove("_ssp245_2041-2060.tif")) %>%
  enframe %>%
  unnest(value) %>% drop_na() %>% 
  mutate(Escenario = "SSP 245") %>% 
  
  bind_rows(., 
            map(sum_585, as.vector) %>% 
              set_names(basename(files_sp585) %>% str_remove("wc2.1_2.5m_tmax_") %>% str_remove("_ssp585_2041-2060.tif")) %>%
              enframe %>%
              unnest(value) %>% drop_na() %>% 
              mutate(Escenario = "SSP 585")
  ) %>%
  bind_rows(.,
            
            app(do.call(c, era5_tmax_mensual_choco$raster_tmax)- 273.15, mean)   %>% 
              as.vector() %>%
              enframe %>% drop_na() %>% 
              mutate(Escenario = "Linea Base ERA5", 
                     name = "z_Linea Base _Tmax")
            
            
  ) %>%
  ggplot(aes(name, value, fill= Escenario)) + geom_boxplot() +
  theme_bw() + coord_flip()




#Temperatura Minima

files_sp245 <- str_subset(files_tmin, pattern = "sp245")
files_sp585 <- str_subset(files_tmin, pattern = "sp585")

rasters_245 <- lapply(files_sp245, rast)
rasters_585 <- lapply(files_sp585, rast)

mean_245 <- app(rast(files_sp245), fun = "mean") 
mean_585 <- app(rast(files_sp585), fun = "mean") 
sum_245 <- files_sp245 %>% map(~app(rast(.x), fun = "mean") )
sum_585 <- files_sp585 %>% map(~app(rast(.x), fun = "mean") )


par(mfrow = c(1, 3))
plot(app(do.call(c, era5_tmin_mensual_choco$raster_tmin)- 273.15, mean))
plot(mean_245)
plot(mean_585)

# Comparacion Escenarios
map(sum_245, as.vector) %>% 
  set_names(basename(files_sp245) %>% str_remove("wc2.1_2.5m_tmin_") %>% str_remove("_ssp245_2041-2060.tif")) %>%
  enframe %>%
  unnest(value) %>% drop_na() %>% 
  mutate(Escenario = "SSP 245") %>% 
  
  bind_rows(., 
            map(sum_585, as.vector) %>% 
              set_names(basename(files_sp585) %>% str_remove("wc2.1_2.5m_tmin_") %>% str_remove("_ssp585_2041-2060.tif")) %>%
              enframe %>%
              unnest(value) %>% drop_na() %>% 
              mutate(Escenario = "SSP 585")
  ) %>%
  bind_rows(.,
            
            app(do.call(c, era5_tmin_mensual_choco$raster_tmin)- 273.15, mean)   %>% 
              as.vector() %>%
              enframe %>% drop_na() %>% 
              mutate(Escenario = "Linea Base ERA5", 
                     name = "z_Linea Base _Tmin")
            
            
  ) %>%
  ggplot(aes(name, value, fill= Escenario)) + geom_boxplot() +
  theme_bw() + coord_flip()




wc_prc_anual <- list.files("data/worldclim/baseline_1970_2000/choco/", pattern = "prec", full.names = T) %>%
  rast() %>% app(sum)

wc_temp_max <- list.files("data/worldclim/baseline_1970_2000/choco/", pattern = "tmax", full.names = T) %>%
  rast() %>% app(mean)

wc_temp_min <- list.files("data/worldclim/baseline_1970_2000/choco/", pattern = "tmin", full.names = T) %>%
  rast() %>% app(mean)

wc_tmean <- (wc_temp_max + wc_temp_min)/2



# Establecer la configuración de la ventana gráfica para 1 fila y 3 columnas
par(mfrow = c(1, 2))

# Graficar el primer raster: Elevación
plot(wc_prc_anual, main = "WorldClim - Precipitacion Anual Media (mm)", col = map.pal("blues"))

# Graficar el segundo raster: Precipitación anual media
plot(precipitacion_anual_media, main = "CHIRPS - Precipitación Anual Media (mm)", col = map.pal("blues"))

# Graficar el tercer raster: K-means Clustering
plot(wc_tmean, main = "WorldClim- Temperatura Media (oC)", col = map.pal("plasma")) 

# # Graficar el 4toraster
plot(annual_mean_temp, main = "ERA 5 - Temperatura Media (oC)", col = map.pal("plasma")) 

 

# Restablecer la configuración a la predeterminada
par(mfrow = c(1, 1))

