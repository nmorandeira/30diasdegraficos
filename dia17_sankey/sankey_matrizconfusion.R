#### 30díasdegráficos día 17 #####
#### Gráfico de Sankey y su utilidad para visualizar matrices de confusión ####

# Datos de Mariela Rajenwerc
# Gráfico: Natalia Morandeira
# Parte del script se basa en el código de: Stephanie Orellana (@sporella)

## Cargar librerías ####

library(tidyverse)

## Cargar y procesar datos ####

#La tabla de origen es una típica matriz de confusión: 
# una matriz cuadrada donde para cada píxel (o polígono) se indica
# en las filas a qué clase de información corresponde (valor observado o "verdad terrena")
# y en las columnas en qué clase fue clasificado (valor predico o clasificado)

library(readr)
matriz_conf <- read_csv(file = "data/class1.csv")

library(tidyr)

#lo paso de matriz cuadrada a listado
list_conf <- matriz_conf1 %>% 
  pivot_longer(Agua:Sauce, "Clasificado")
head(list_conf)

#### Cálculo exactitud global e índice Kappa #######
cantidad_clases <- length(matriz_conf) -1
cantidad_clases

matriz <- as.matrix(matriz_conf[, 2:(cantidad_clases+1)])

diagonal.counts <- diag(matriz)
N <- sum(matriz)

#Exactitud global
Exactitud_global <- sum(diagonal.counts) / N
Exactitud_global

row.marginal.props <- rowSums(matriz)/N
col.marginal.props <- colSums(matriz)/N

#### Cálculo de kappa (K)
Po <- sum(diagonal.counts)/N
Pe <- sum(row.marginal.props*col.marginal.props)
Kappa_index <- (Po - Pe)/(1 - Pe)
Kappa_index

### plot #########
library(ggplot2)
library(ggalluvial)
library(wesanderson)

pal <-wes_palette("Moonrise3", n = 5)


ggplot(data = list_conf, aes(axis2 = Observado, axis1 = Clasificado, y = value)) +
  geom_alluvium(aes(fill = Clasificado), show.legend = FALSE) +
  geom_stratum(aes(fill = Clasificado), colour = "grey33", alpha = 0.2, show.legend = FALSE) +
  geom_text(size = 4, stat = "stratum", infer.label = TRUE) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(limits = c("Observado", "Clasificado"),  expand = c(.2, .05)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Evaluación de una clasificación de ambientes con imágenes satelitales",
       subtitle = "Clasificación de humedales del Bajo Delta del Paraná (Buenos Aires, Argentina) \ncon imágenes SAR Sentinel-1. Datos: Mariela Rajngewerc") +
    theme(axis.text.y   = element_blank(),
          axis.text.x   = element_text(size = 12, face= "bold"),
    axis.ticks = element_blank(),
    axis.title = element_blank(), 
    panel.background = element_rect(size = 0, fill=NA), 
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

ggsave("sankey.png",width = 8, height = 5, dpi = 300)

