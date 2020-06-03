### NiUnaMenos - 3 de junio en Argentina ###########

#En los últimos 5 años en Argentina hubo 1582 víctimas letales por violencia de género (1423 femicidios directos y 159 femicidios vinculados), datos del Observatorio de la Casa del Encuentro. El 83% de los casos están o estuvieron en proceso judicial y figuran en el registro de la Corte Suprema de Justicia de la Nación. El 74% de las víctimas fueron asesinadas por una pareja, ex-pareja o familiar

## Fuente de los datos:
# Registro Nacional de Femicidios de La Justicia Argentina, Corte Suprema de Justicia de la Nación: https://www.csjn.gov.ar/omrecopilacion/omfemicidio/homefemicidio.html
# Casa del Encuentro: http://www.lacasadelencuentro.org/femicidios03.html

# Cargar datos
library(readr)
library(tidyverse)
femicidios_anio<-read_csv('data/femicidios_anio.csv') #df
femicidios_vinculo<-read_csv('data/femicidios_vinculo.csv') #lvl1 


femicidios <-read_csv('data/femicidios_vinculo.csv')

# orden de datos y plot basados en: https://stackoverflow.com/questions/50004058/multiple-dependent-level-sunburst-doughnut-chart-using-ggplot2
lvl0 <- tibble(name = "#NiUnaMenos", value = 0, level = 0, fill = NA)
head(lvl0)#estructura: name / value / level / fill
str(lvl0)


lvl1 <- femicidios_anio #me interesa Anio (como name) y Total_CSJN (como value) 
lvl1$name <- as.character(lvl1$Anio)
lvl1$value <- lvl1$Total_CSJN
lvl1$level <- 1
lvl1$fill <- as.character(lvl1$Anio)
lvl1 <- lvl1[,6:9]
head(lvl1)
str(lvl1)



lvl2 <- femicidios_vinculo
lvl2$name <- lvl2$Vinculo
lvl2$value <- lvl2$Total
lvl2$level <- 2
lvl2$fill <- as.character(lvl2$Anio)
lvl2 <- lvl2[,6:9]
head(lvl2)


datos <- bind_rows(lvl0, lvl1, lvl2) %>%
  mutate(name = as.factor(name) %>% fct_reorder2(fill, value)) %>%
  arrange(fill, name) %>%
  mutate(level = as.factor(level)) 

levels(datos$name) = c("254 femicidios ", "273 femicidios" , "235 femicidios" , "278 femicidios", "268 femicidios", "Pareja y ex-pareja", "Familiar", "Otro conocido", "Desconocido", "Sin datos", "#NiUnaMenos" )


p <-  ggplot(data = datos, aes(x = level, y = value, fill = fill, alpha = level)) +
  geom_col(width = 1, color = "gray90", size = 0.5, position = position_stack()) +
  geom_text(aes(label = name), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.6), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_brewer(palette = "Dark2", na.translate = F) +
  labs(x = NULL, y = NULL, title= "#NiUnaMenos - Femicidios en Argentina en los últimos cinco años", subtitle = "Total de femicidios directos y vinculados - por año y según el vínculo entre víctima y victimario", caption = "Fuente: Registro Nacional de Femicidios de La Justicia Argentina, Corte Suprema de Justicia de la Nación", fill = "Año" ) +
    theme(  plot.caption = element_text(hjust = 0), panel.background = element_blank(), legend.title = element_text("Año"))

p

# Guardar gráfico
ggsave(filename = "salida.png",p,width = unit(13,"cm") ,height = unit(6.5,"cm")) # medidas tw
ggsave(filename = "salida.png",p,width = unit(9,"cm") ,height = unit(7,"cm"), dpi = 150) # medidas más cuadradas




