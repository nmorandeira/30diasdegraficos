### Script para el procesamiento y visualización de (algunos de los datos) ##

## Limpieza de los datos ####
#Cargar la base de datos
plague_orig <- read.csv("https://raw.githubusercontent.com/hofstra/itinerary/gh-pages/_data/a-frightful-number-plague.csv") #esta base de datos está en https://github.com/hofstra/itinerary


#eliminar registros sin mes
plague <- subset(plague_orig, plague_orig$date_month_exact == TRUE) 

#generar columna de fecha
plague$date  <- as.Date(ISOdate(plague$date_year, plague$date_month, plague$date_day) )

#voy a quedarme sólo con algunas columnas
colnames(plague)
plague <- plague[,c(8:10, 17:20, 23)]
str(plague) #las columnas con datos numéricos están almacenadas como factores. Tienen asteriscos, paréntesis y otras cosas raras que hay que limpiar

plague$buried_plague #hay datos entre paréntesis. Significa: dato oficial de registros oficiales (sospecha de dato real según Daniel Defoe). Creo otras dos columnas. Luego las edito medio a mano porque no se me ocurre otro modo :(
plague$buried_plague_official <- as.character(plague$buried_plague)
plague$buried_plague_suspected <- as.character(plague$buried_plague)

plague$buried_plague_official[53]= 14
plague$buried_plague_suspected[53]=50
plague$buried_plague_official[54]=9
plague$buried_plague_suspected[54]=20
plague$buried_plague_official[57]=68
plague$buried_plague_suspected[57]=100
plague$buried_plague_official[62]=NA
plague$buried_plague_suspected[62]=900
plague$buried_plague_official[72]=NA
plague$buried_plague_suspected[72]=850
plague$buried_plague_official[173]=7
plague$buried_plague_suspected[173]=7
plague$buried_plague_official[174]=60
plague$buried_plague_suspected[174]=60
plague$buried_plague_official[175]=2
plague$buried_plague_suspected[175]=2
plague$buried_plague_official[176]=1
plague$buried_plague_suspected[176]=1

plague$buried_plague_official <- as.numeric(plague$buried_plague_official)
plague$buried_plague_suspected <- as.numeric(plague$buried_plague_suspected)

#El resto de las columnas no me interesan por ahora, me voy a quedar sólo con las filas/columnas con datos para los muertos por peste

### subset Buried Plague ###
plague_buriedplague <- subset(plague, plague$buried_plague_official != "NA")
plague_buriedplague <- plague_buriedplague[, c(1, 8:10)]
library(dplyr)
plague_buriedplague <- arrange(plague_buriedplague, date)
plague_buriedplague$buried_plague_official_CUM <-  cumsum(plague_buriedplague[, 3]) #aquí creé la columna de muertes acumuladas
  
#para graficar sólo 1665 ("el año de la peste"), asigno los casos acumulados de diciembre a 1/01 enero
plague_buriedplague$date_1965 <- plague_buriedplague$date
plague_buriedplague$date_1965[1] = "1665-01-01"
plague_buriedplague$date_1965[2] = "1665-01-01"
plague_buriedplague$buried_plague1965 <- plague_buriedplague$buried_plague_official
plague_buriedplague$buried_plague1965[1] <- 0
plague_buriedplague$buried_plague1965[2] <- 0
plague_buriedplague$buried_plague1965_CUM <- plague_buriedplague$buried_plague_official_CUM
plague_buriedplague$buried_plague1965_CUM[1] <- 3
plague_buriedplague$buried_plague1965_CUM[2] <- 3

#armo base en formato tidy
library(tidyr)
plague_buried1665 <- plague_buriedplague[,c(1:2, 6:8)]
colnames(plague_buried1665) <- c("date_descriptive",      "date" ,                 "date_1965" ,            "Nuevos_casos", "Casos_acumulados")
plague_buried1665 <- gather(plague_buried1665,variable,Muertes,Nuevos_casos:Casos_acumulados)
head(plague_buried1665)

### Plots Buried plague 1965 ####
#plots de datos oficiales que registra Daniel Defoe (en algunos casos, él sospecha que hay más muertes)

library(ggplot2)
library(scales)

#acumulados y nuevos
p <- ggplot(plague_buried1665, aes(x=date_1965, y=Muertes, color=variable)) +
     geom_line(size=1) +
  xlab("Fecha") +
  ylab("Número de muertes por peste bubónica") +
  scale_x_date(date_labels = "%b/%Y", breaks = "month") +
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.caption = element_text(hjust = 0, vjust=-1, face = "italic"), plot.title = element_text(face = "bold"), legend.position="bottom", legend.title = element_blank()) +
  labs(title="'Diario del año de la peste' de Daniel Defoe", subtitle = "Muertes por peste bubónica en Londres durante 1665, según la crónica publicada en 1722", caption = "Base de datos: 'A Frightful Number! Digital Resource Center - Hofstra University'") 
p

#agregar la tapa de la publicación
library(png)
library(grid)
cover <- readPNG("book_cover/cover_clip.png")
p <- p + annotation_raster(cover, ymin = 10000,ymax= 46000,xmin = -111405,xmax = -111320) #busqué un tamaño que respete la relación de aspecto original de la imagen

ggsave("salida_Defoe.png", plot = p, width = 7, height = 5, dpi = 300)


