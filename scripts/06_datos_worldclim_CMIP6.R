library(terra)
#library(tmap)
library(tidyverse)

# Cargar múltiples raster
files_prec <- list.files("data/worldclim/choco/", full.names = TRUE, pattern = "prec")
files_tmax <- list.files("data/worldclim/choco/", full.names = TRUE, pattern = "tmax")
files_tmin <- list.files("data/worldclim/choco/", full.names = TRUE, pattern = "tmin")

files_prec <- list.files("data/worldclim/caqueta/", full.names = TRUE, pattern = "prec")
files_tmax <- list.files("data/worldclim/caqueta/", full.names = TRUE, pattern = "tmax")
files_tmin <- list.files("data/worldclim/caqueta/", full.names = TRUE, pattern = "tmin")



#Precipitacion


files_sp245 <- str_subset(files_prec, pattern = "sp245")
files_sp585 <- str_subset(files_prec, pattern = "sp585")

rasters_245 <- lapply(files_sp245, rast)
rasters_585 <- lapply(files_sp585, rast)

mean_245 <- app(rast(files_sp245), fun = "mean") 
mean_585 <- app(rast(files_sp585), fun = "mean") 
sum_245 <- files_sp245 %>% map(~app(rast(.x), fun = "sum") )
sum_585 <- files_sp585 %>% map(~app(rast(.x), fun = "sum") )


baseline_ppt <- caqueta_ppt

par(mfrow = c(1, 3))
plot(baseline_ppt, main = "Linea Base CHIRPS")
plot(app(do.call(c, sum_245), mean), main = "SSP 245")
plot(app(do.call(c, sum_585), mean), main = "SSP 585")


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
            
            baseline_ppt %>% as.vector() %>% 
              enframe %>% drop_na() %>% 
              mutate(Escenario = "Linea Base CHIRPS", 
                     name = "z_Linea Base")
            
            
  ) %>%
  ggplot(aes(name, value, fill= Escenario)) + geom_boxplot() +
  theme_bw() + coord_flip() +
  labs(title = "Comparativa precipitacion Anual Media vs 2050", x = "Modelos GCM",  y = "Prec. (mm)")





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
plot(caqueta_temp)
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
  theme_bw() + coord_flip() + 
  labs(title = "Comparativa Temperatura Maxima Anual Media  vs 2050", x = "Modelos GCM",  y = "Temp. (oC)")




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
plot(app(do.call(c, era5_tmin_mensual_caqueta$raster_tmin)- 273.15, mean))
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
  theme_bw() + coord_flip() +
  labs(title = "Comparativa Temperatura Minima Anual Media  vs 2050", x = "Modelos GCM",  y = "Temp. (oC)")





