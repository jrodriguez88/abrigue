## Analsis Estaciones IDEAM vs CHIRPS , ERA5 ####
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024


# library(leaflet)
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/refs/heads/main/R/get_metrics.R")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/refs/heads/main/R/get_metrics.R")

plet(abrigue_municipios_choco, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_choco), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_choco), col="red", cex=2, popup=TRUE)

#Establecer estaciones y periodos
estaciones_objetivo <- c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi", "Amargal")
ini_date <- ymd("1981-01-01")
end_date <- ymd("2024-07-01")

base_date <- seq.Date(ini_date, end_date, by = "month") %>% enframe(name = NULL, value = "date")
baseline_ar6 <- seq.Date(ymd("1995-01-01"), ymd("2014-12-31"), by = "month") %>% 
  enframe(name = NULL, value = "date")


# Subset
ideam_prec <- ideam_prec_choco %>% 
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)

test_data <- ideam_prec %>% distinct() %>% 
  nest(ideam = c(date, ideam)) %>% 
  mutate(ideam = map(ideam, ~left_join(base_date, .x))) %>%
  mutate(ideam_baseline_ar6 = map(ideam, ~left_join(baseline_ar6, .x))) %>%
  left_join(chirps_data_choco_ws %>% nest(chirps = -CodigoEstacion))

test_data %>%
  mutate(na_pct = map_dbl(data, naniar::pct_miss)) %>%
  mutate(na_pct_ar6 = map_dbl(baseline_ar6, naniar::pct_miss)) %>%
  print(n=47)

left_join(base_date, ideam_prec)


chirps_choco_mensual_comparativa <- left_join(base_date, ideam_prec) %>% 
  left_join(
  chirps_data_choco_ws %>% rename(chirps = value)) %>%
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmax <- ideam_temp_choco %>% filter(Etiqueta == "TMX_MEDIA_M") %>% 
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>% distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)

era5_choco_tmax_comparativa <- era5_tmax_data_choco_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>% distinct() %>%
  left_join(ideam_tmax) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 

ideam_tmin <- ideam_temp_choco %>% filter(Etiqueta == "TMN_MEDIA_M") %>%
  select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
  mutate(date = as.Date(date)) %>% distinct() %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)


era5_choco_tmin_comparativa <- era5_tmin_data_choco_ws %>% rename(era5 = value) %>%
  mutate(era5 = era5 - 273.15) %>%
  right_join(ideam_tmin) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") 


## Linea de tiempo
time_line_estaciones(ideam_temp_choco, estaciones_objetivo, ini_date, end_date)
time_line_estaciones(ideam_prec_choco, estaciones_objetivo, ini_date, end_date)


## Graficos comparativos
lineplot_mensual_clima(chirps_choco_mensual_comparativa, estaciones_objetivo)
lineplot_mensual_clima(era5_choco_tmax_comparativa, estaciones_objetivo, "Temperatura Maxima (oC)")
lineplot_mensual_clima(era5_choco_tmin_comparativa, estaciones_objetivo, "Temperatura Minima (oC)")

boxplot_mensual_clima(chirps_choco_mensual_comparativa, estaciones_objetivo)
boxplot_mensual_clima(era5_choco_tmax_comparativa, estaciones_objetivo, "Temperatura Maxima (oC)")
boxplot_mensual_clima(era5_choco_tmin_comparativa, estaciones_objetivo, "Temperatura Minima (oC)")




data_to_evaluate_chirps <- chirps_choco_mensual_comparativa  %>% 
  drop_na() %>% 
#  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 

data_to_evaluate_eraTmax <- era5_choco_tmax_comparativa  %>% 
  drop_na() %>% distinct() %>%
  # dplyr::summarise(n = dplyr::n(), .by = c(CodigoEstacion, date, year, month, NombreEstacion, Fuente)) |>
  # dplyr::filter(n > 1L) 
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 

data_to_evaluate_eraTmin <- era5_choco_tmin_comparativa  %>% 
  drop_na() %>% 
  dplyr::summarise(n = dplyr::n(), .by = c(CodigoEstacion, date, year, month, NombreEstacion, Fuente)) |>
  dplyr::filter(n > 1L)
  #  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) 


data_to_lm_plot %>%
  ggplot(aes(chirps, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(chirps, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()

data_to_lm_plot %>% rename(obs = ideam, sim = chirps) %>% get_metrics

data_to_lm_plot %>% rename(obs = ideam, sim = chirps) %>% split(.$NombreEstacion) %>%
  map(get_metrics)

data_to_lm_plot %>% rename(obs = ideam, sim = chirps) %>% get_metrics

tidy_data <- function(db1, db2, iyear = 1985, fyear = 2010, join_by = "date") {
  
  left_join(db1, db2, by = join_by) %>%
    filter(lubridate::year(date) >= iyear,
           lubridate::year(date) <= fyear) %>%
    gather(var, value, -date)%>%
    mutate(source = case_when(str_detect(var, "[.x]$") ~ "obs", 
                              str_detect(var, "[.y]$") ~ "sim"),
           var = str_sub(var, 1, -3)) %>%
    spread(source, value) %>%
    nest(-var)
  




