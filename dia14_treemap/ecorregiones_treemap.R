## Ecorregiones de Argentina: representación en el país y porcentaje protegido
## Día 14 - Treemap

# Fuente de los datos: 
# Sistema de Información de Biodiversidad de la Administración de Parques Nacionales de Argentina
# https://sib.gob.ar/ecorregiones



## Lectura de datos ####

## La tabla está online en formato JSON y se exporta como xls: 
## https://sib.gob.ar/api/2.0.0/jsonToExcel?url=https://sib.gob.ar/api/2.0.0/ecorregiones&name=ecorregiones 
## No conseguí bajarla fácilmente a R. La bajo manualmente como xls e importo acá

library(readxl)

ecorregiones <- read_excel("data/ecorregiones.xls")
head(ecorregiones)
str(ecorregiones)
ecorregiones$superficie <- as.numeric(ecorregiones$superficie)

## gráfico treemap ###
library(ggplot2)
library(treemapify)

p <- ggplot(ecorregiones, aes(area = superficie, fill = porcentaje_protegido, label = nombre)) +
  geom_treemap(color = "black") +
  geom_treemap_text( colour = "white", place = "centre", grow = TRUE, min.size = 1) +
  theme(plot.title = element_text(face = "bold"), legend.position="bottom") +
  labs (title = "Ecorregiones de Argentina", subtitle= "Representación del territorio nacional (áreas) y Porcentaje protegido (gradiente)" , caption = "Fuente: Sistema de Información de Biodiversidad de la Administración de Parques Nacionales") +
  scale_fill_gradient(low = "#b5dce9", high="#003333", name="Superficie protegida en áreas nacionales (%):  ")
  
p

ggsave("salida_ecorregiones.png", plot = p, width = 7, height = 4, dpi = 300)




