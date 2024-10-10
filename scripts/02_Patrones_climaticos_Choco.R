## Mapa de patrones climaticos Choco - Bivariate maps
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Octubre 2024


# library(biscale)
# library(cowplot)


chirps_choco_linea_base_1995_2014
era5_choco_linea_base_1995_2014



# Resample the annual mean temperature raster to a finer resolution (approximately 1 km)
new_res <- 0.025  # Set the new resolution
new_raster <- rast(ext(era5_choco_linea_base_1995_2014), resolution = new_res, crs = crs(era5_choco_linea_base_1995_2014))
annual_mean_temp <- resample(x = era5_choco_linea_base_1995_2014, y = new_raster, method="bilinear")
choco_temp <- terra::crop(annual_mean_temp - 273.15, y = project(choco_shp, annual_mean_temp), mask = TRUE)


# Resample the annual mean precipitation raster to a finer resolution (approximately 1 km)
new_raster <- rast(ext(chirps_choco_linea_base_1995_2014), resolution = new_res, crs = crs(chirps_choco_linea_base_1995_2014))
annual_mean_ppt <- resample(x = chirps_choco_linea_base_1995_2014, y = new_raster, method = "bilinear")
choco_ppt <- terra::crop(annual_mean_ppt, y = project(choco_shp, annual_mean_ppt), mask = TRUE)



# Re-muestrear choco_temp para que coincida con choco_ppt
choco_temp_resampled <- resample(choco_temp, choco_ppt)

# Combinar los rasters una vez que tienen la misma extensión y resolución
temp_ppt <- c(choco_temp_resampled, choco_ppt)


# Assign descriptive names to each raster layer in the stack
names(temp_ppt) <- c("temp", "ppt")
temp_ppt_df <- temp_ppt |> 
  # project(cho) |> 
  as.data.frame(xy = TRUE) |> drop_na()


# Classify the temperature and precipitation data into bivariate classes using the 'biscale' package
# 'style = "quantile"' divides data into quantiles, and 'dim = 4' creates 4 classes for each variable, resulting in 16 bivariate categories
data <- bi_class(temp_ppt_df,
                 x = temp, 
                 y = ppt, 
                 style = "quantile", dim = 4)

# Plot the distribution of the bivariate classes to visualize the frequency of each class
data |> 
  count(bi_class) |> 
  ggplot(aes(x = bi_class, y = n)) +
  geom_col() +  # Create a bar plot to show the count of each bivariate class
  labs(title = "Distribution of Bivariate Classes", x = "Bivariate Class", y = "Frequency")



# Set the color palette for the bivariate map
pallet <- "BlueOr"

# Create the bivariate map using ggplot2
map <- ggplot() +
  theme_light(base_size = 14) +  # Set a minimal theme for the map
 # xlim(3.96, 8.675) +  # Set the x-axis limits for the map (longitude range)
 # ylim(-77.9, -75) +  # Set the y-axis limits for the map (latitude range)
  # Plot the bivariate raster data with appropriate fill color based on bivariate classes
  geom_raster(data = data, mapping = aes(x = x, y = y, fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  # Apply the bivariate color scale using the selected palette and dimensions
  bi_scale_fill(pal = pallet, dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  # Overlay the first administrative level boundaries 
  geom_sf(data = st_as_sf(project(abrigue_municipios_choco, choco_ppt)), fill = NA, color = "black", linewidth = 0.10) +
  # Overlay the country-level boundary 
  geom_sf(data = st_as_sf(project(choco_shp, choco_ppt)), fill = NA, color = "black", linewidth = 0.40) +
  # Add labels for the map
  labs(x = "Longitud", y = "Latitud",
       title = "Patrones de Temperatura y Precipitation en el Departamento del Chocó:", 
       subtitle = "Promedio de Temperatura y precipitacion durante el periodo 1995 - 2014",
       caption = "Source: Temperatura (ERA5), Precipitacion (CHIRPS)      Author: Rodriguez-Espinoza, 2024") +
  # Customize the appearance of the title, subtitle, and caption
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 10, face = "bold", hjust = 0.5))

# Create the legend for the bivariate map
legend <- bi_legend(pal = pallet,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Temperatura (oC)",
                    ylab = "Precipitacion (mm)",
                    size = 10)


# Combine the map and legend using cowplot
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +  # Draw the main map plot
  draw_plot(legend, 0.17, 0.05, 0.28, 0.28)   # Draw the legend in the specified position

# Display the final map with legend
finalPlot







# library(gridBase)
# library(grid)
# 
# par(mfrow=c(1, 4))
# # Graficar el primer raster: Elevación
# plot(annual_mean_temp - 273.15, main = "Temperatura (oC)", col = map.pal("plasma")) 
# 
# # Graficar el segundo raster: Precipitación anual media
# plot(annual_mean_ppt, main = "Precipitación Anual Media (mm)", col = rev(map.pal("viridis")))
# 
# ## the last one is the current plot
# plot.new()              ## suggested by @Josh
# vps <- baseViewports()
# pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
# vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
# print(finalPlot, vp = vp1) 
# 
# # Graficar el tercer raster: K-means Clustering
# plot(cluster_raster, main = "K-means Clustering", col = rainbow(5)) # Ajusta la paleta según el número de clusters
# 
# # Restablecer la configuración a la predeterminada
# par(mfrow = c(1, 1))
