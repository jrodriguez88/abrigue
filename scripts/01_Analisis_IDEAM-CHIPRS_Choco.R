## Analsis Estaciones IDEAM vs CHIRPS , ERA5 ####
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024


# library(leaflet)


plet(abrigue_municipios_choco, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_choco), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_choco), col="red", cex=2, popup=TRUE)

#Establecer estaciones y periodos
estaciones_objetivo_choco <- c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi", "Amargal")
ini_date <- "1981-01-01"
end_date <- "2024-08-01"


# Subset
ideam_prec <- ideam_prec_choco %>% 
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)


chirps_choco_mensual_comparativa <- chirps_data_choco_ws %>% rename(chirps = value) %>%
  right_join(ideam_prec) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmax <- ideam_temp_choco %>% filter(Etiqueta == "TMX_MEDIA_M") %>%
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)

era5_choco_tmax_comparativa <- era5_tmax_data_choco_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%
  right_join(ideam_tmax) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmin <- ideam_temp_choco %>% filter(Etiqueta == "TMN_MEDIA_M") %>%
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)


era5_choco_tmin_comparativa <- era5_tmin_data_choco_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%
  right_join(ideam_tmin) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 


## Linea de tiempo
time_line_estaciones(ideam_temp_choco, estaciones_objetivo_choco, ini_date, end_date)
time_line_estaciones(ideam_prec_choco, estaciones_objetivo_choco, ini_date, end_date)


## Graficos comparativos
lineplot_mensual_clima(chirps_choco_mensual_comparativa, estaciones_objetivo_choco)
lineplot_mensual_clima(era5_choco_tmax_comparativa, estaciones_objetivo_choco, "Temperatura Maxima (oC)")
lineplot_mensual_clima(era5_choco_tmin_comparativa, estaciones_objetivo_choco, "Temperatura Minima (oC)")

boxplot_mensual_clima(chirps_choco_mensual_comparativa, estaciones_objetivo_choco)
boxplot_mensual_clima(era5_choco_tmax_comparativa, estaciones_objetivo_choco, "Temperatura Maxima (oC)")
boxplot_mensual_clima(era5_choco_tmin_comparativa, estaciones_objetivo_choco, "Temperatura Minima (oC)")




data_to_lm_plot <- chirps_choco_mensual_comparativa  %>% 
  drop_na() %>% 
  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 


data_to_lm_plot %>%
  ggplot(aes(chirps, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(chirps, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()


chirps_choco_mensual_comparativa 




