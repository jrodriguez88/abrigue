#Ubicacion de Estaciones


library(leaflet)


plet(abrigue_municipios_caqueta, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_caqueta), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_caqueta), col="red", cex=2, popup=TRUE)

#Establecer estaciones y periodos
estaciones_objetivo_caqueta <- c(unique(ideam_tmax$NombreEstacion))
ini_date <- "1981-01-01"
end_date <- "2024-08-01"


# Subset
ideam_prec <- ideam_prec_caqueta %>% 
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)


chirps_caqueta_mensual_comparativa <- chirps_data_caqueta_ws %>% rename(chirps = value) %>%
  right_join(ideam_prec) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmax <- ideam_temp_caqueta %>% filter(Etiqueta == "TMX_MEDIA_M") %>%
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)

era5_caqueta_tmax_comparativa <- era5_tmax_data_caqueta_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%   dplyr::distinct() %>% drop_na() %>%
  right_join(ideam_tmax) %>% 
  dplyr::distinct() %>% drop_na() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") %>% 
  distinct()

ideam_tmin <- ideam_temp_caqueta %>% filter(Etiqueta == "TMN_MEDIA_M") %>%
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) 


era5_caqueta_tmin_comparativa <- era5_tmin_data_caqueta_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%
  right_join(ideam_tmin) %>% 
  dplyr::distinct() %>% drop_na() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente")  %>% 
  distinct()


## Linea de tiempo
time_line_estaciones(ideam_temp_caqueta, estaciones_objetivo_caqueta, ini_date, end_date)
time_line_estaciones(ideam_prec_caqueta, unique(ideam_prec$NombreEstacion), ini_date, end_date)


## Graficos comparativos
lineplot_mensual_clima(chirps_caqueta_mensual_comparativa, estaciones_objetivo_caqueta)
lineplot_mensual_clima(era5_caqueta_tmax_comparativa, estaciones_objetivo_caqueta, "Temperatura Maxima (oC)")
lineplot_mensual_clima(era5_caqueta_tmin_comparativa, estaciones_objetivo_caqueta, "Temperatura Minima (oC)")

boxplot_mensual_clima(chirps_caqueta_mensual_comparativa, estaciones_objetivo_caqueta)
boxplot_mensual_clima(era5_caqueta_tmax_comparativa, estaciones_objetivo_caqueta, "Temperatura Maxima (oC)")
boxplot_mensual_clima(era5_caqueta_tmin_comparativa, estaciones_objetivo_caqueta, "Temperatura Minima (oC)")




data_to_lm_plot_prec <- chirps_caqueta_mensual_comparativa  %>% 
  drop_na() %>% 
#  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 

data_to_lm_plot_tmax <- era5_caqueta_tmax_comparativa  %>% 
  drop_na() %>% 
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 

data_to_lm_plot_tmax$era5[[2]]

data_to_lm_plot_tmin <- era5_caqueta_tmin_comparativa  %>% 
  drop_na() %>% #distinct() %>%
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 


data_to_lm_plot_prec %>%
  ggplot(aes(chirps, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(chirps, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()


era5_tmin_data_caqueta_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%
  right_join(ideam_tmin) %>% 
  dplyr::distinct() %>% drop_na()  %>%
  ggplot(aes(era5, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(era5, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()

era5_tmin_data_caqueta_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%
  right_join(ideam_tmin) %>% 
  dplyr::distinct() %>% drop_na() 

era5_tmax_data_caqueta_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%   dplyr::distinct() %>% drop_na() %>%
  right_join(ideam_tmax) %>%
  dplyr::distinct() %>% drop_na()  %>%
  ggplot(aes(era5, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(era5, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()
