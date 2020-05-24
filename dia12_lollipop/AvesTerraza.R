## Listado de aves observadas desde la terraza de casa (CABA, Argentina)
## Observadora: Natalia Morandeira
## Anotaciones al azar, sin método de muestreo, a ojo desnudo o con binoculares: 2017-presente

## Lectura de datos ####

library(readr)
avesterraza <- read_csv("avesterraza.csv", 
                        locale = locale(asciify = TRUE))

library(png)
library(grid)
img <- readPNG("cielo.png") #foto del cielo, sacada desde la terraza

## organizacion de datos ####
library(dplyr)

avesterraza_orden <- avesterraza %>%
  count(Orden, sort = T)

avesterraza_orden

avesterraza_familia <- avesterraza %>%
  count(Familia, sort = T)

avesterraza_familia

## gráfico ####
library(ggplot2)


p <- ggplot(data = avesterraza_orden, aes(x=reorder(Orden, n), y=n)) +
  annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_segment( aes(x=reorder(Orden, n), xend=reorder(Orden, n), y=0, yend=n), color="white", size = 1) +
  geom_point( color="white", size=4, alpha=0.8) +
  theme(axis.line=element_line(color="black"), axis.text.x=element_text(color="black"), axis.text.y=element_text(color="black")) +
  coord_flip() +
  
  labs(y = "Cantidad de especies", x="Orden", title = "Órdenes de las aves que veo desde mi terraza", subtitle = "Ciudad de Buenos Aires, Argentina") 

p

ggsave("salida_Orden.png", plot = p, width = 7, height = 4, dpi = 300)


p2 <- ggplot(data = avesterraza_familia, aes(x=reorder(Familia, n), y=n)) +
  annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_segment( aes(x=reorder(Familia, n), xend=reorder(Familia, n), y=0, yend=n), color="white", size = 1) +
  geom_point( color="white", size=4, alpha=0.8) +
  theme(axis.line=element_line(color="black"), axis.text.x=element_text(color="black"), axis.text.y=element_text(color="black")) +
  coord_flip() +
  
  labs(y = "Cantidad de especies", x="Familia", title = "Familias de las aves que veo desde mi terraza", subtitle = "Ciudad de Buenos Aires, Argentina") 

p2

ggsave("salida_Familia.png", plot = p, width = 7, height = 4, dpi = 300)
  

