# Directorio de los archivos raster
path <- "data/chirps_raster/choco/"
files <- list.files(path, full.names = TRUE)

# Extraer el año de los nombres de archivo
years <- unique(substr(basename(files), 13, 16))


annual_precipitation <- list()

for (year in years) {
  # Filtrar archivos del año específico
  yearly_files <- files[grepl(paste0(year, "\\."), basename(files))]
  
  # Cargar los raster del año
  monthly_rasters <- rast(yearly_files)
  
  # Sumar los raster mensuales para obtener el acumulado anual
  annual_raster <- app(monthly_rasters, sum)
  
  # Guardar el resultado en la lista
  annual_precipitation[[year]] <- annual_raster
  
  # # O guardar el raster en un archivo
  # writeRaster(annual_raster, 
  #             filename = paste0("data/chirps_raster/choco_raster_yearly/annual_precipitation_", year, ".tif"), 
  #             overwrite = TRUE)
}

annual_files <- list.files(path = "data/chirps_raster/choco_raster_yearly/", full.names = T)

# Filtrar los archivos para el periodo 1995-2014
period_files <- annual_files[grepl("199[5-9]|200[0-9]|201[0-4]", basename(annual_files))]


# Cargar los raster del periodo
period_rasters <- rast(annual_precipitation)

# Calcular el raster promedio del periodo
mean_raster <- app(a, median)

# Guardar el raster promedio en un archivo
writeRaster(mean_raster, 
            filename = "data/chirps_raster/mean_precipitation_choco_1995_2014.tif", 
            overwrite = TRUE)

precipitacion_anual_media <- rast("data/chirps_raster/mean_precipitation_choco_1995_2014.tif")
period_rasters[period_rasters < 0] <- NA

mean_raster <- app(period_rasters, median)

precipitacion_anual_media <- mean_raster


plot(mean_raster, main = "precipictacion anual")


# Resample the annual mean temperature raster to a finer resolution (approximately 1 km)
new_res <- 0.025  # Set the new resolution
new_raster <- rast(ext(precipitacion_anual_media), resolution = new_res, crs = crs(precipitacion_anual_media))
annual_mean_rain <- resample(x = precipitacion_anual_media, y = new_raster, method="bilinear")

# Plot the cropped annual mean temperature for the United Kingdom
plot(annual_mean_rain)

library(magick)
# Función para crear imágenes de los raster
create_image <- function(raster, year, output_dir) {
  # Reemplazar valores <= 0 por NA
  raster[raster < 0] <- NA
  
  # Convertir el raster a un dataframe para usar ggplot2
  raster_df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
  colnames(raster_df)[3] <- "precipitation"
  
  # municipalities <-  project(municipalities, raster) %>% st_as_sf()
  
  # Crear el plot
  p <- ggplot(raster_df) +
    geom_tile(aes(x = x, y = y, fill = precipitation)) +
    scale_fill_viridis_c(na.value = "transparent") +  # Opcional: Color para NA
    theme_minimal() +
    labs(title = paste("Precipitation - Year:", year), fill = "Precipitation (mm)", x = "Longitud", y = "Latitud") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_fixed(ratio = 1)   # Asegura que la relación de aspecto sea 1:1
    # geom_sf(data = municipalities, fill = NA, color = "black", size = 0.5)  # Agregar el borde de los municipios
  
  # Guardar la imagen temporalmente
  output_file <- paste0(output_dir, "/precipitation_", year, ".png")
  ggsave(output_file, plot = p, width = 8, height = 6)
  
  return(output_file)
}

temp_dir <- tempdir()

# Lista para almacenar las rutas de las imágenes generadas
image_files <- c()

# Loop para crear una imagen por cada archivo en annual_files
for (file in annual_files) {
  # Extraer el año del nombre del archivo
  year <- substr(basename(file), 22, 25)
  
  # Cargar el raster
  raster <- rast(file)
  
  # Crear la imagen y guardarla
  image_path <- create_image(raster, year, temp_dir)
  image_files <- c(image_files, image_path)
}

# Crear el GIF usando magick
gif <- image_read(image_files) %>%
  image_animate(fps = 1) %>%
  image_write("data/chirps_raster/precipitation_animation.gif")





# Cargar y procesar los rásteres anuales
raster_list <- lapply(period_files, function(file) {
  r <- rast(file)
  r[r < 0] <- NA  # Cambiar valores menores a 0 por NA
  return(r)
})

template <- raster_list[[1]]  # Usar el primer raster como plantilla
raster_list <- lapply(raster_list, function(r) {
  resample(r, template, method = "bilinear")
})

# Apilar los rásteres
raster_stack <- rast(temp_ppt)

# Cargar el raster stack (en tu caso ya está en memoria)
r_stack <- temp_ppt

# Convertir el raster stack a una matriz donde cada fila es un píxel y cada columna es una capa
r_matrix <- as.matrix(r_stack)

# Eliminar filas con valores NA (si hay)
r_matrix <- na.omit(r_matrix)

# Identificar las filas que no contienen NAs
valid_indices <- complete.cases(r_matrix)

# Aplicar K-means solo a los píxeles válidos
set.seed(123) # Para reproducibilidad
k <- 4 # Número de clusters, ajusta según lo que necesites
kmeans_result <- kmeans(r_matrix[valid_indices, ], centers = k, nstart = 25)

# Crear un nuevo raster con la misma estructura que el original
cluster_raster <- rast(r_stack[[1]])

# Inicializar el raster con NA
values(cluster_raster) <- NA

# Asignar los valores de los clusters solo a los píxeles válidos
values(cluster_raster)[valid_indices] <- kmeans_result$cluster

# Visualizar el resultado
plot(cluster_raster, main = "K-means Clustering")




# Establecer la configuración de la ventana gráfica para 1 fila y 3 columnas
par(mfrow = c(1, 3))

# Graficar el primer raster: Elevación
plot(choco_dem, main = "Elevación (m)", col = map.pal("elevation")) 

# Graficar el segundo raster: Precipitación anual media
plot(precipitacion_anual_media, main = "Precipitación Anual Media (mm)", col = rev(map.pal("viridis")))

# Graficar el tercer raster: K-means Clustering
plot(cluster_raster, main = "K-means Clustering", col = rainbow(5)) # Ajusta la paleta según el número de clusters

# Restablecer la configuración a la predeterminada
par(mfrow = c(1, 1))



# Extraer los valores de precipitación y los clusters
precipitacion_values <- values(precipitacion_anual_media)
cluster_values <- values(cluster_raster)

# Crear un data frame con los valores
data <- data.frame(cluster = cluster_values[,1], precipitacion = precipitacion_values[,1])

# Eliminar filas con NA (en caso de que existan)
data <- na.omit(data)

# Calcular la precipitación media para cada cluster
precipitacion_media_por_cluster <- aggregate(precipitacion ~ cluster, data = data, FUN = mean)

# Ver los resultados
print(precipitacion_media_por_cluster)


# Calcular la precipitación media y desviación estándar por cluster
precipitacion_stats <- aggregate(precipitacion ~ cluster, data = data, FUN = function(x) c(media = mean(x), sd = sd(x)))

# Convertir a un data frame
precipitacion_stats <- do.call(data.frame, precipitacion_stats)

# Renombrar las columnas
colnames(precipitacion_stats) <- c("cluster", "media", "sd")

# Calcular el intervalo de confianza del 95% (IC)
n <- table(data$cluster)  # Número de observaciones por cluster
alpha <- 0.05  # Nivel de significancia
error_margin <- qnorm(1 - alpha / 2) * (precipitacion_stats$sd / sqrt(n))

# Calcular los límites inferior y superior del IC
precipitacion_stats$IC_inferior <- precipitacion_stats$media - error_margin
precipitacion_stats$IC_superior <- precipitacion_stats$media + error_margin

# Ver los resultados
print(precipitacion_stats)

