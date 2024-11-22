# Analisis de Indices Oceanicos y Variabilidad Climatica
## Autores: Rodriguez-Espinoza J. // 
## github.com/jrodriguez88
## Noviembre 2024

# Cargar las librerías necesarias ----
#library(rsoi)


# Ejemplo Choco
# Cargar los datos raster de precipitación ----
chirps_raster_mensual_choco
chirps_raster_mensual_choco[chirps_raster_mensual_choco < 0] <- NA



# Preparar los datos de los índices ENSO ----
ONI <- download_enso()
MEI <- download_mei()


# gráfico ONI
ggplot(ONI, aes(x = Date, y = ONI, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Cool Phase/La Nina" = "blue",
    "Neutral Phase" = "gray",
    "Warm Phase/El Nino" = "red"
  )) +
  labs(
    title = "Índice Oceánico de El Niño (ONI)",
    x = "Fecha",
    y = "Valor ONI",
    fill = "Fase"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# gráfico MEI
ggplot(drop_na(MEI), aes(x = Date, y = MEI, fill = Phase)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Cool Phase/La Nina" = "blue",
    "Neutral Phase" = "gray",
    "Warm Phase/El Nino" = "red"
  )) +
  labs(
    title = "Índice Multivariado de El Niño (MEI)",
    x = "Fecha",
    y = "Valor MEI",
    fill = "Fase"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


# Alinear las fechas de los datos raster con las fases ENSO ----

# Obtener los nombres de las capas
layer_names <- names(chirps_raster_mensual_choco)

# Extraer año y mes de los nombres de las capas
dates <- str_extract(layer_names, "\\d{4}\\.\\d{2}")

# Convertir a fecha
dates <- as.Date(paste0(dates, ".01"), format = "%Y.%m.%d")

# Asignar las fechas a las capas raster
time(chirps_raster_mensual_choco) <- dates


# Crear un dataframe con las fechas de las capas raster
raster_dates <- data.frame(
  Layer = layer_names,
  Date = dates
)

# Unir con las fases ENSO
raster_phases <- raster_dates %>%
  left_join(ONI, by = "Date")

# Calcular los promedios de precipitación para cada fase ENSO ----

indices_nino <- which(raster_phases$phase == "Warm Phase/El Nino")
indices_nina <- which(raster_phases$phase == "Cool Phase/La Nina")
indices_neutral <- which(raster_phases$phase == "Neutral Phase")

# Promedio histórico
precip_promedio_historico <- mean(chirps_raster_mensual_choco, na.rm = TRUE)

# Promedio durante El Niño
precip_promedio_nino <- mean(chirps_raster_mensual_choco[[indices_nino]], na.rm = TRUE)

# Promedio durante La Niña
precip_promedio_nina <- mean(chirps_raster_mensual_choco[[indices_nina]], na.rm = TRUE)

# Promedio durante Fase Neutral
precip_promedio_neutral <- mean(chirps_raster_mensual_choco[[indices_neutral]], na.rm = TRUE)


# Crear mapas comparativos ----

# Configurar la paleta de colores
library(RColorBrewer)
pal <- brewer.pal(9, "Blues")

# Plotear los promedios
par(mfrow = c(2, 2))  # Para mostrar varios mapas juntos

plot(precip_promedio_historico, main = "Promedio Histórico", col = pal)
plot(precip_promedio_nino, main = "Promedio El Niño", col = pal)
plot(precip_promedio_nina, main = "Promedio La Niña", col = pal)
plot(precip_promedio_neutral, main = "Promedio Fase Neutral", col = pal)

# Convertir raster a data frame
df_historico <- as.data.frame(precip_promedio_historico, xy = TRUE)
df_nino <- as.data.frame(precip_promedio_nino, xy = TRUE)
df_nina <- as.data.frame(precip_promedio_nina, xy = TRUE)
df_neutral <- as.data.frame(precip_promedio_neutral, xy = TRUE)


# Mapa del promedio histórico
ggplot(df_historico, aes(x = x, y = y, fill = mean)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(title = "Promedio Histórico de Precipitación", fill = "mm") +
  theme_minimal()

# Crear box plots de las diferencias de precipitación ----

# Extraer valores de precipitación para cada fase
vals_nino <- values(chirps_raster_mensual_choco[[indices_nino]])
vals_nina <- values(chirps_raster_mensual_choco[[indices_nina]])
vals_neutral <- values(chirps_raster_mensual_choco[[indices_neutral]])

# Crear dataframes con los valores y la fase correspondiente
df_nino <- data.frame(Precipitacion = as.vector(vals_nino), Fase = "El Niño")
df_nina <- data.frame(Precipitacion = as.vector(vals_nina), Fase = "La Niña")
df_neutral <- data.frame(Precipitacion = as.vector(vals_neutral), Fase = "Neutral")

# Unir los dataframes
df_boxplot <- rbind(df_nino, df_nina, df_neutral)

# Eliminar valores NA y valores inválidos (e.g., -9999)
df_boxplot <- df_boxplot %>%
  filter(!is.na(Precipitacion)) %>%
  filter(Precipitacion >= 0)

ggplot(df_boxplot, aes(x = Fase, y = Precipitacion, fill = Fase)) +
  geom_boxplot() +
  scale_fill_manual(values = c("El Niño" = "red", "La Niña" = "blue", "Neutral" = "gray")) +
  labs(title = "Distribución de Precipitación según Fase ENSO", x = "Fase ENSO", y = "Precipitación (mm)") +
  theme_minimal()



######
# Crear una variable temporal en meses desde el inicio
time_months <- 1:nlyr(chirps_raster_mensual_choco)

# Definir una función para calcular la tendencia
calc_trend <- function(y) {
  if (all(is.na(y))) {
    return(NA)
  } else {
    model <- lm(y ~ time_months)
    return(coef(model)[2])  # La pendiente
  }
}


# Aplicar la función calc_trend a cada píxel
precip_trend <- app(chirps_raster_mensual_choco, calc_trend)

# Visualizar el raster de tendencias
plot(precip_trend, main = "Tendencia de Precipitación (mm/mes)")


# Convertir el raster a data frame
df_trend <- as.data.frame(precip_trend, xy = TRUE)
names(df_trend)[3] <- "trend"

# Crear el mapa
ggplot(df_trend, aes(x = x, y = y, fill = trend)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_equal() +
  labs(title = "Tendencia de Precipitación", fill = "mm/mes") +
  theme_minimal()

