## Analisis Areas de pesca


# 
library(patchwork)
# gráfico ONI
(ggplot(ONI, aes(x = Date, y = ONI, fill = phase)) +
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
  )) +


# gráfico MEI
(ggplot(drop_na(MEI), aes(x = Date, y = MEI, fill = Phase)) +
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
  ) + guides(values = F))

