## Kmeans clima -  ABRIGUE ####
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024

# Directorio de los archivos raster
plot(chirps_choco_linea_base_1995_2014, main = "Precipitacion anual - referencia")

plot(temp_ppt)

precipitacion_anual_media <- chirps_choco_linea_base_1995_2014


# # Resample the annual mean temperature raster to a finer resolution (approximately 1 km)
# new_res <- 0.025  # Set the new resolution
# new_raster <- rast(ext(precipitacion_anual_media), resolution = new_res, crs = crs(precipitacion_anual_media))
# annual_mean_rain <- resample(x = precipitacion_anual_media, y = new_raster, method="bilinear")
# 
# # Plot the cropped annual mean temperature for the United Kingdom
# plot(annual_mean_rain)

# 
# 
# # Cargar y procesar los rásteres anuales
# raster_list <- lapply(chirps_raster_anuales_choco$raster_anual, function(file) {
#   r <- file
#   r[r < 0] <- NA  # Cambiar valores menores a 0 por NA
#   return(r)
# })
# 
# template <- raster_list[[1]]  # Usar el primer raster como plantilla
# raster_list <- lapply(chirps_raster_anuales_choco$raster_anual, function(r) {
#   resample(r, template, method = "bilinear")
# })
# 
# # Apilar los rásteres
# raster_stack <- rast(temp_ppt)
# 
# # Cargar el raster stack (en tu caso ya está en memoria)
# r_stack <- temp_ppt

# Convertir el raster stack a una matriz donde cada fila es un píxel y cada columna es una capa
r_matrix <- as.matrix(temp_ppt)

# Eliminar filas con valores NA (si hay)
#r_matrix <- na.omit(r_matrix)

# Identificar las filas que no contienen NAs
valid_indices <- complete.cases(r_matrix)

# Aplicar K-means solo a los píxeles válidos
set.seed(123) # Para reproducibilidad
k <- 4 # Número de clusters, ajusta según lo que necesites
kmeans_result <- kmeans(r_matrix[valid_indices, ], centers = k, nstart = 25)

# Crear un nuevo raster con la misma estructura que el original
cluster_raster <- rast(temp_ppt)

# Inicializar el raster con NA
values(cluster_raster) <- NA

# Asignar los valores de los clusters solo a los píxeles válidos
values(cluster_raster)[valid_indices] <- kmeans_result$cluster

# Visualizar el resultado
plot(cluster_raster[[1]], main = "K-means Clustering")



# Establecer la configuración de la ventana gráfica para 1 fila y 3 columnas
par(mfrow = c(1, 4))

# Graficar el primer raster: Elevación
plot(choco_dem, main = "Elevación (m)", col = map.pal("elevation")) 

# Graficar el segundo raster: Precipitación anual media
plot(precipitacion_anual_media, main = "Precipitación Anual Media (mm)", col = rev(map.pal("viridis")))

# # Graficar el tercer raster
plot(annual_mean_temp - 273.15, main = "Temperatura (oC)", col = map.pal("plasma")) 

# Graficar el tercer raster: K-means Clustering
plot(cluster_raster[[1]], main = "K-means Clustering", col = rainbow(5)) # Ajusta la paleta según el número de clusters

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

