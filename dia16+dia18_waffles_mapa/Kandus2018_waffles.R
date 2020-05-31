### Publicaciones de teledetección en humedales de Sudamérica ####
### Datos de la publicación y revisión bibliográfica:

# Patricia Kandus, Priscilla Gail Minotti, Natalia Soledad Morandeira, Rafael Grimson, Gabriela González Trilla, Eliana Belén González, Laura San Martín & Maira Patricia Gayol (2018) Remote sensing of wetlands in South America: status and challenges, International Journal of Remote Sensing, 39:4, 993-1016, DOI: 10.1080/01431161.2017.1395971

# Link: https://doi.org/10.1080/01431161.2017.1395971



# Cargar datos -------------
library(readr)
paises <- read_csv("data/Kandus2018_paises.csv", locale = locale(encoding = "WINDOWS-1252")) #en qué países se realizaron los estudios?
humedales <- read_csv("data/Kandus2018_humedales.csv", locale = locale(encoding = "WINDOWS-1252")) #qué humedales estudian?

library(dplyr)
paises <- paises %>%
  arrange(desc(Publicaciones))

paises_pub <- paises$Publicaciones
names(paises_pub) <- paises$Pais


humedales <- humedales %>%
  arrange(desc(Publicaciones))
humedales_pub <- humedales$Publicaciones
names(humedales_pub) <- humedales$Humedal


### dia 16 - gráficos de waffles -------------------
# Graficar
library(ggplot2)
library(waffle) 
library(gridExtra)


colors10 <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#355c7d")


p1 <- waffle(paises_pub, rows = 10, size = 4,
                      color = colors10) +
  labs(title = "Países", subtitle="Publicaciones en humedales de Sudamérica con teledetección", caption="Kandus et al. 2018")

p1

p2 <- waffle(humedales_pub, rows = 10, size = 4,
             color = colors10[1:9] ) +
  labs(title = "Tipos de humedales", subtitle="Publicaciones en humedales de Sudamérica con teledetección", caption="Kandus et al. 2018")
p2

ggsave("salida_paises.png", plot = p1, width = 8, height = 5, dpi = 300) 
ggsave("salida_tiposhumedales.png", plot = p2, width = 8, height = 5, dpi = 300) 


p <- grid.arrange(p1,p2, left=T,
                  top = "Publicaciones sobre teledetección en humedales de Sudamérica")
p

#### dia 17 - datos espaciales -----------------
### Sigo en gran parte el script de Priscilla Minotti en el Meetup 4 de RLadies de 2017
library(rgdal)
library(sp)

paises_geo <- readOGR(dsn = "data", layer = "countries") #capa vectorial de paises, en caso de usar una capa propia

#capa "World"
library(tmap)
data("World")
SouthAmerica<-World%>%filter(continent %in% c("South America")) #esta capa la voy a usar para hacer un bounding box

summary(paises_geo) # me interesa NAME

# Leer y unr los datos no espaciales: publicaciones por país a capa de mundo
paises$Pais %in% paises_geo$NAME #ver en cuáles coincide el nombre del pais

#traduzco
paises$Pais_en <- paises$Pais
paises$Pais_en[1] <- "Brazil"
paises$Pais_en[4] <- "Peru"
paises$Pais_en[8] <- "French Guiana"

paises$Pais_en %in% paises_geo$NAME #ver en cuáles coincide el nombre del pais
#sólo me van a quedar afuera los estudios regionales, que involucran más de un país
p1 #no son despreciables, pero para este ejercicio los voy a dejar afuera


#Resumen de coincidencias
summary(paises$Pais_en %in% paises_geo$NAME)

library(dplyr) 
#join o unión espacial
paises_geo@data <- left_join(paises_geo@data, paises, by = c('NAME' = 'Pais_en'))
paises_geo@data


## visualización
library(tmap)
#elegir dónde graficar
tmap_mode("view") #que la salida sea en el viewer, con fondo de OpenStreetMap detrás 
tmap_mode("plot") #que la salida sea pestaña Plot 


mapa <- tm_shape(paises_geo,  bbox= SouthAmerica) + 
  tm_layout(title = "¿Los humedales de qué países son estudiados con teledetección?", legend.frame = T) +
  tm_polygons(col = "Publicaciones") 
mapa

#mejorar la paleta
tmaptools::palette_explorer() #se pueden generar paletas y copiar el código

mapa <- tm_shape(paises_geo,  bbox= SouthAmerica) + 
  tm_layout(main.title = "¿Los humedales de qué países son estudiados con teledetección?", main.title.size = 0.8,legend.frame = T) +
  tm_polygons(col = "Publicaciones", style = "cont", palette = "YlGn", n = 8) + #cont indica que la escala es continua, el resto lo copio del explorador de paletas 
  tm_credits("Fuente: Kandus et al. 2018", bg.color = "white")
mapa

tmap_save(mapa,filename = "salida_mapa.png", width = 5, height = 5, dpi = 150)


### plot final
# no encontré cómo hacer un grid.arrange del mapa y el waffle
