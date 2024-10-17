#Ubicacion de Estaciones


library(leaflet)


plet(abrigue_municipios_caqueta, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_caqueta), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_caqueta), col="red", cex=2, popup=TRUE)

plet(abrigue_municipios_choco, "MpNombre", split=TRUE, alpha=.2) |> 
  points(vect(estaciones_ideam_prec_choco), col="blue", cex=2, popup=TRUE) |> 
  points(vect(estaciones_ideam_temp_choco), col="red", cex=2, popup=TRUE)




chirps_choco_mensual_comparativa <- chirps_data_choco_ws %>% rename(chirps = value) %>%
  right_join(ideam_prec_choco %>% 
               select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
               mutate(date = as.Date(date))) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title) 


chirps_caqueta_mensual_comparativa <- chirps_data_caqueta_ws %>% rename(chirps = value) %>%
  right_join(ideam_prec_caqueta %>% 
               select(CodigoEstacion, NombreEstacion, date = Fecha, ideam = Valor) %>%   
               mutate(date = as.Date(date))) %>% 
  distinct() %>%
  pivot_longer(cols = -c(CodigoEstacion, NombreEstacion, date, year, month), names_to = "Fuente") %>%
  mutate(NombreEstacion = str_remove(NombreEstacion, "\\s*\\[.*?\\]") %>% str_to_title)




chirps_choco_mensual_comparativa %>% drop_na() %>% 
  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  ggplot(aes(date, value, color = Fuente)) +
  geom_line() +
  facet_wrap(~ NombreEstacion, scales = "free") +
  theme_bw(14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom") +
  labs(x = NULL, y = "Precipitacion (mm)", fill = "Fuente: ") 


chirps_choco_mensual_comparativa %>% drop_na() %>% 
  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
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
  labs(x = NULL, y = "Precipitacion (mm)", fill = "Fuente: ") 




ideam_prec_choco %>% 
  select(NombreEstacion, FechaInstalacion, FechaSuspension) %>% 
  distinct() %>%
  mutate(FechaInstalacion = as_date(dmy_hm(FechaInstalacion)),
         FechaInstalacion = as.Date(ifelse(FechaInstalacion < ymd("1981-01-01"),  ymd("1981-01-01"), FechaInstalacion)),
         FechaSuspension = as_date(dmy_hm(FechaSuspension)),
         FechaSuspension = as.Date(ifelse(is.na(FechaSuspension),  ymd("2024-09-01"), FechaSuspension))) %>%
  #  filter(FechaInstalacion > ymd("1981-01-01")) %>%
  # Crear el gráfico de segmentos
  ggplot(aes(x = FechaInstalacion, xend = FechaSuspension, 
             y = NombreEstacion, yend = NombreEstacion)) +
  geom_segment(color = "blue", size = 1.2) +  # Segmentos
  geom_vline(xintercept = ymd("1981-01-01")) +
  # scale_x_date(limits = c(ymd("1960-01-01"), ymd("2024-08-30"))) + 
  labs(title = "Duración de estaciones", x = "Fecha", y = "Nombre de la Estación") +
  theme_minimal() 



chirps_mensual_comparativa  %>% 
  drop_na() %>% 
  filter(NombreEstacion %in% c("Teresita La", "Cupica", "Panamericana", "Nuqui", "Arusi")) %>%
  pivot_wider(names_from = Fuente, values_from = value) %>%
  ggplot(aes(chirps, ideam, color = month)) + geom_point() + 
  geom_smooth(aes(chirps, ideam), method = "lm", inherit.aes = F) +
  facet_wrap(~NombreEstacion) +
  theme_minimal() +
  scale_color_viridis_c()




