## Analsis Estaciones IDEAM vs CHIRPS , ERA5  - Caqueta ####
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024

# library(leaflet)
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/refs/heads/main/R/get_metrics.R")
#source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/refs/heads/main/R/get_metrics.R")

plet(abrigue_municipios_caqueta, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_caqueta), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_caqueta), col="red", cex=2, popup=TRUE)

#Establecer estaciones y periodos
#estaciones_objetivo <- c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi", "Amargal")
ini_date <- "1981-01-01"
end_date <- "2024-08-01"


# Subset
ideam_prec <- ideam_prec_caqueta %>% 
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)

test_data <- ideam_prec %>% distinct() %>% 
  nest(ideam = c(date, ideam)) %>% 
  mutate(ideam = map(ideam, ~left_join(base_date, .x))) %>%
  mutate(ideam_baseline_ar6 = map(ideam, ~left_join(baseline_ar6, .x))) %>%
  left_join(chirps_data_caqueta_ws %>% nest(chirps = -CodigoEstacion))

test_data %>%
  mutate(na_pct = map_dbl(ideam, naniar::pct_miss)) %>%
  mutate(na_pct_ar6 = map_dbl(ideam_baseline_ar6, naniar::pct_miss)) %>%
  print(n=47)




chirps_caqueta_mensual_comparativa <- left_join(base_date, ideam_prec) %>% 
  left_join(
    chirps_data_caqueta_ws %>% rename(chirps = value)) %>%
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmax <- ideam_temp_caqueta %>% filter(Etiqueta == "TMX_MEDIA_M") %>% 
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>% distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)

era5_caqueta_tmax_comparativa <- left_join(base_date, ideam_tmax) %>% 
  left_join(era5_tmax_data_caqueta_ws %>% rename(era5 = value) %>%
              mutate(era5 = era5 - 273.15) %>% distinct()) %>%
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmin <- ideam_temp_caqueta %>% filter(Etiqueta == "TMN_MEDIA_M") %>%
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>% distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)


era5_caqueta_tmin_comparativa <- left_join(base_date, ideam_tmin) %>% 
  left_join(era5_tmin_data_caqueta_ws %>% rename(era5 = value) %>%
              mutate(era5 = era5 - 273.15) %>% distinct()) %>%
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 


estaciones_objetivo_caqueta <- c(unique(ideam_tmax$NombreEstacion))

## Linea de tiempo
time_line_estaciones(ideam_temp_caqueta, estaciones_objetivo_caqueta, ini_date, end_date)
time_line_estaciones(ideam_prec_caqueta, estaciones_objetivo_caqueta, ini_date, end_date)


## Graficos comparativos
lineplot_mensual_clima(chirps_caqueta_mensual_comparativa, unique(chirps_caqueta_mensual_comparativa$NombreEstacion))
lineplot_mensual_clima(era5_caqueta_tmax_comparativa, estaciones_objetivo_caqueta, "Temperatura Maxima (oC)")
lineplot_mensual_clima(era5_caqueta_tmin_comparativa, estaciones_objetivo_caqueta, "Temperatura Minima (oC)")

boxplot_mensual_clima(chirps_caqueta_mensual_comparativa, estaciones_objetivo_caqueta)
boxplot_mensual_clima(era5_caqueta_tmax_comparativa, estaciones_objetivo_caqueta, "Temperatura Maxima (oC)")
boxplot_mensual_clima(era5_caqueta_tmin_comparativa, estaciones_objetivo_caqueta, "Temperatura Minima (oC)")




data_to_evaluate_chirps <- chirps_caqueta_mensual_comparativa  %>% 
  drop_na() %>% 
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 

data_to_evaluate_eraTmax <- era5_caqueta_tmax_comparativa  %>% 
  drop_na() %>% 
  distinct() %>%
  # dplyr::summarise(n = dplyr::n(), .by = c(CodigoEstacion, date, year, month, NombreEstacion, Fuente)) |>
  # dplyr::filter(n > 1L) 
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) %>% 
  unnest(ideam) %>% unnest(era5) %>% drop_na()

data_to_evaluate_eraTmin <- era5_caqueta_tmin_comparativa  %>%  
  distinct() %>%
  drop_na() %>% 
  # dplyr::summarise(n = dplyr::n(), .by = c(CodigoEstacion, date, year, month, NombreEstacion, Fuente)) |>
  # dplyr::filter(n > 1L)
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value)  %>% 
  unnest(ideam) %>% unnest(era5) %>% drop_na()




eval_reanalisis <- list(chirps = data_to_evaluate_chirps %>% rename(obs = ideam, sim = chirps) %>% get_metrics,
                        era_tmax = data_to_evaluate_eraTmax %>% rename(obs = ideam, sim = era5) %>% get_metrics,
                        era_tmin = data_to_evaluate_eraTmin %>% rename(obs = ideam, sim = era5) %>% get_metrics) %>%
  bind_rows(.id = "Fuente")


## CHIRPS
data_to_evaluate_chirps %>%
  ggplot(aes(chirps, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(chirps, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()

data_to_evaluate_chirps %>% rename(obs = ideam, sim = chirps) %>% 
  get_metrics

data_to_evaluate_chirps %>% rename(obs = ideam, sim = chirps) %>% 
  split(.$NombreEstacion) %>%
  map(get_metrics) %>% bind_rows(.id = "Nombre_estacion") %>%
  print(n=47)



## ERA5 Tmax
data_to_evaluate_eraTmax %>%
  ggplot(aes(era5, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(era5, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()

data_to_evaluate_eraTmax %>% rename(obs = ideam, sim = era5) %>% 
  get_metrics

data_to_evaluate_eraTmax %>% rename(obs = ideam, sim = era5) %>% 
  split(.$NombreEstacion) %>%
  map(get_metrics) %>% bind_rows(.id = "Nombre_estacion") %>%
  print(n=47)

## ERA5 Tmin

data_to_evaluate_eraTmin %>%
  ggplot(aes(era5, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(era5, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()

data_to_evaluate_eraTmin %>% rename(obs = ideam, sim = era5) %>% 
  get_metrics

data_to_evaluate_eraTmin %>% rename(obs = ideam, sim = era5) %>% 
  split(.$NombreEstacion) %>%
  map(get_metrics) %>% bind_rows(.id = "Nombre_estacion")%>%
  print(n=47)

